repos <- c("https://predictiveecology.r-universe.dev", getOption("repos"))
# source("https://raw.githubusercontent.com/PredictiveEcology/pemisc/refs/heads/development/R/getOrUpdatePkg.R")
# getOrUpdatePkg(c("Require", "remotes"), c("1.0.1.9013", "0.0.0")) # only install/update if required
# remotes::install_github("PredictiveEcology/SpaDES.project@development", upgrade = FALSE)
# remotes::install_github("PredictiveEcology/SpaDES.projec")

devPkgs <- c("PredictiveEcology/Require@usePak", 
             "PredictiveEcology/reproducible@recovery2",
             "PredictiveEcology/SpaDES.project@working/combined-prs",
             "PredictiveEcology/SpaDES.core@fixRCMDcheckWarnings",
             "PredictiveEcology/climateData@modsDuringFireSense3")
source("installDevPkgs.R", local = environment())


suppressWarnings(rm(.ELFind)) # This is a precaution as this may exist if there is a failure below; and this is rerun

####################
# pre RUN the global.R setupProject
####################
outs <- SpaDES.project::preRunSetupProject(file = "global.R", upTo = "params")

####################
# RUN fireSense_ELFs to get the ELF map
####################

.ELFs <- fireSenseUtils::runELFs(outs, whatOut = "maps")
.ELFinds <- names(.ELFs$rasCentered)
#TODO: this is a subset of well-behaved ELFs
.ELFinds <- c(# "6.1.1", 
              "6.1.2", "6.1.1", "6.2.2", "6.3.1", "6.6.1", "6.5", "6.6.2", "9.1.1") 
# .ELFinds <- paste0("ELF", .ELFinds)
####################
# SET UP EXPERIMENT
####################

if (TRUE) { # This is Ian/Jonathan's stuff
  .reps <- 2:5 # how many reps do we want?
  .reps <- 1:5
  .GCMs <- list(future = c("CNRM-ESM2-1"), past = "NRV")
  .SSPs <- list(future = 370, past = 370)
  .samplingRanges <- list(future = list(2071:2100), past = list(1991:2020))
  expts <- Map(.GCM = .GCMs, .SSP = .SSPs, .samplingRange = .samplingRanges, function(.GCM, .SSP, .samplingRange) {
    expand.grid(.ELFind = .ELFinds, .rep = .reps, .GCM = .GCM, .SSP = .SSP, 
                .samplingRange = .samplingRange, stringsAsFactors = FALSE)
  })
  
  # Do NRV first, multiple reps of each next, then move to next ELF --> Alex needs reps
  expt <- data.table::rbindlist(expts)
  data.table::setorder(expt, -.GCM, .ELFind, .rep)
  if (exists(".modules"))
    expt <- cbind(expt, .modules = I(lapply(seq_len(NROW(expt)), function(x) .modules)))
  if (exists(".times"))
    expt <- cbind(expt, .times = I(lapply(seq_len(NROW(expt)), function(x) .times)))
  # expt <- expt[order(expt[, 1], expt[, 2]),] # do all reps of each ELF first, then next ELF
  rownames(expt) <- 1:NROW(expt) # re-number each row
  
  
  #we can use .samplingRange to determine if NRV or not (if 2020 is assumed to be NRV), but in that case, rvPeriod should be derived from that. 
  # otherwise, we should flatten outputPath here
  #e.g. outputs/4.3/1991-2020/NRV/rep1
  #     outputs/4.3/2070-2100/CNRM-ESM2-1_ssp370/rep1
  
  #instead of outputs/4.3/2100/NRV/
  
  
  #make unique in case we add more sampling ranges
} else { # This is Eliot's stuff
  .reps <- 2:5 # how many reps do we want?
  .GCM <- "CNRM-ESM2-1"
  .SSP <- 370
  .times <- list(start = 2020, end = 2020 + 1000)
  firstElfs <- "6.|9."
  .ELFinds <- c(grep(firstElfs, .ELFinds, value = TRUE), grep(firstElfs, .ELFinds, value = TRUE, invert = TRUE))
  expt <- expand.grid(.rep = .reps, .ELFind = .ELFinds, .GCM, .SSP, stringsAsFactors = FALSE)
  # if (exists(".modules"))
  #   expt <- cbind(expt, .modules = I(lapply(seq_len(NROW(expt)), function(x) .modules)))
  # if (exists(".times"))
  #   expt <- cbind(expt, .times = I(lapply(seq_len(NROW(expt)), function(x) .times)))
}

expt <- unique(expt, by = colnames(expt)[!sapply(expt, is, "list")])

####################
# Run the experiment -- this must be run at a command prompt, inside tmux
#  --> MUST MONITOR `activeRunningPath`. It should represent only "currently running ELFs".
#      If an ELF has stopped running, and the log file is still present, then it must be
#      manually deleted (automated deletion has failed)
####################

pak::pak("PredictiveEcology/Require@usePak", ask = FALSE, upgrade = FALSE)
# options(warn = 2)

workers <- SpaDES.project::experimentTmux(
  df                  = expt,          # df provided here
  global_path         = "global.R",
  # cores = rep("localhost", 2),
  cores = c(
    rep("localhost", 9),
    rep(c(
      "dougfir"
      , "mpb"
      # , "localhost"
    ), 2),
    rep(c(
      "fire"
      , "coco"
    ), 2),
    rep(c(
      "mpb"
      , "coco"
      , "camas"
      , "carbon"
      , "caribou"
      , "core" 
      , "sbw"
    ), 1),
      # "birds", "biomass", 
    c("acer"
      , "birds"
      # "biomass", 
      , "abies"
      , "pinus")
  ) |> sort(), # sort puts them next to each other in tmux
  # cores = c(rep("biomass", 2), rep("coco", 2)),
  queue_path          = "longRuns.rds",
  delay_before_source = 15,
  times = outs$times,
  runNameLabel = quote(pathBuild(.ELFind, .samplingRange, .GCM, .SSP, .rep)), #c("ELFind", "rep", "GCM"),
  on_interrupt = "fail",
  forceLocalQueueToGS = FALSE,
  # outputPath = outs$paths$outputPath,
  pathBuild = pathBuild,
  statusCalculate = # statusCalculator(type = "fireSense")
    quote({
      dirWithUpdatedElf <- pathBuild(.ELFind, .samplingRange, .GCM, .SSP, .rep)
      dd <- dir(dirWithUpdatedElf, recursive = TRUE, full.names = TRUE)
      if (NROW(dd)) {
        ee <- grep(value = TRUE, pattern = "burnMap.*tif$", dd)
        done <- grepl(paste0("year", times$end), ee)
        if (all(done %in% FALSE)) { # running
          runningFile <- dir(activeRunningPathForTmux(queue_path = queue_path), 
                             pattern = .ELFind, full.names = TRUE)
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
  sim$..ELFind
}