repos <- c("https://predictiveecology.r-universe.dev", getOption("repos"))
# source("https://raw.githubusercontent.com/PredictiveEcology/pemisc/refs/heads/development/R/getOrUpdatePkg.R")
# getOrUpdatePkg(c("Require", "remotes"), c("1.0.1.9013", "0.0.0")) # only install/update if required
# remotes::install_github("PredictiveEcology/SpaDES.project@cacheRequire")


suppressWarnings(rm(.ELFind)) # This is a precaution as this may exist if there is a failure below; and this is rerun

####################
# pre RUN the global.R setupProject
####################

outs <- SpaDES.project::preRunSetupProject(file = "global.R", upTo = "params")

####################
# RUN fireSense_ELFs to get the ELF map
####################

.ELFinds <- fireSenseUtils::runELFs(outs)
.ELFinds <- c("6.2.2", "6.3.1", "6.6.1", "6.5", "6.6.2", "9.1.1")
.ELFinds <- paste0("ELF", .ELFinds)
####################
# SET UP EXPERIMENT
####################

.reps <- 1:2 # how many reps do we want?
.GCM <- c("CNRM-ESM2-1", "NRV")
# .GCM <- "NRV" #for test
.SSP <- 370
.samplingRange <- c(2100)

expt <- expand.grid(.ELFind = .ELFinds, .rep = .reps, .GCM = .GCM, .SSP = .SSP, 
                    .samplingRange = .samplingRange, stringsAsFactors = FALSE)
if (exists(".modules"))
  expt <- cbind(expt, .modules = I(lapply(seq_len(NROW(expt)), function(x) .modules)))
if (exists(".times"))
  expt <- cbind(expt, .times = I(lapply(seq_len(NROW(expt)), function(x) .times)))
# expt <- expt[order(expt[, 1], expt[, 2]),] # do all reps of each ELF first, then next ELF
rownames(expt) <- 1:NROW(expt) # re-number each row

expt[expt$.GCM == "NRV",]$.samplingRange <- c(2020)
expt[expt$.GCM == "NRV",]$.SSP <- 370 #in case more SSP added, make these non-unique so below works
#make unique in case we add more sampling ranges
expt <- unique(expt)

####################
# Run the experiment -- this must be run at a command prompt, inside tmux
#  --> MUST MONITOR `activeRunningPath`. It should represent only "currently running ELFs".
#      If an ELF has stopped running, and the log file is still present, then it must be
#      manually deleted (automated deletion has failed)
####################
workers <- SpaDES.project::experimentTmux(
  df                  = expt,          # df provided here
  global_path         = "global.R",
  n_workers           = 10,
  queue_path          = "predict_queue.rds",
  delay_before_source = 15,
  workersToMonitor = "localhost",
  times = outs$times,
  outputPath = outs$paths$outputPath,
  statusCalculate = # statusCalculator(type = "fireSense")
    quote({
    dirWithUpdatedElf <- gsub("4.3", strsplit(runName, "-")[[1]][[1]], outputPath)
    dirWithUpdatedElf <- gsub("rep1", paste0("rep", strsplit(runName, "-")[[1]][[2]]), dirWithUpdatedElf)
    dd <- dir(dirWithUpdatedElf, recursive = TRUE, full.names = TRUE)
    ee <- grep(value = TRUE, pattern = "burnMap.*tif$", dd)
    done <- grepl(paste0("year", times$end), ee)
    if (all(done %in% FALSE)) { # running
      runningFile <- dir(activeRunningPathForTmux(queue_path = queue_path), pattern = runName, full.names = TRUE)
      ff <- grep(value = TRUE, pattern = "Annual Fire Maps", dd)
      fi2 <- file.info(ff)
      
      if (length(runningFile)) {
        fi <- file.info(runningFile)
        started_at <- format(fi[, "mtime"])
        mostRecentFile <- tail(ff[fi2[, "mtime"] > fi[, "mtime"]], 1)
        heartbeat_at <- if (length(mostRecentFile) > 0) format(file.info(mostRecentFile)[, "mtime"]) else NA
        heartbeat_iter <- if (is.na(heartbeat_at)) times$start else gsub(".+Maps ([[:digit:]]{4,4}).+", "\\1", mostRecentFile)
      }
      
    }
    finishedFile <- ee[done]
    if (length(finishedFile)) {
      iterationsTotal <- gsub(".+year([[:digit:]]{4,4}).+", "\\1", finishedFile)
      finished_at <- if (length(finishedFile) > 0) format(file.info(finishedFile)[, "mtime"]) else NA
      done <- any(done)
    }
  })
  ,
  ss_id = "https://drive.google.com/drive/folders/1X9-mRjyLMNpgkP_cfqhbr_AQEPOsVCHf"
)


if (FALSE) {
  # This is the abandonned experiment3 way
  future::plan("cluster", workers = min(NROW(expt), 3), persistent = TRUE)
  p <- SpaDES.project::experiment3(expt, file = "global.R",
                                   saveSimToDisk = FALSE, tmuxName = "ex8")
  # get google authentication
  sss <- readLines("~/googledriveAuthentication.R")
  eval(parse(text = sss)) |> options()
  
  # For estimating elapsed time
  sim = SpaDES.core:::savedSimEnv()$.sim
  ee = elapsedTime(sim)
  ee[, Predict := c("Fit", "Predict")[1 + as.numeric(grepl("redict", moduleName) | grepl("Dispersal|mortalityAndGrowth|summaryBGM", eventType))]]
  ee[, sum (elapsedTime), by = Predict]
  sim$.runName
}