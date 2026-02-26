repos <- c("https://predictiveecology.r-universe.dev", getOption("repos"))
source("https://raw.githubusercontent.com/PredictiveEcology/pemisc/refs/heads/development/R/getOrUpdatePkg.R")
getOrUpdatePkg(c("Require", "remotes"), c("1.0.1.9013", "0.0.0")) # only install/update if required
# remotes::install_github("PredictiveEcology/SpaDES.project@cacheRequire")


suppressWarnings(rm(.ELFind)) # This is a precaution as this may exist if there is a failure below; and this is rerun

####################
# pre RUN the global.R setupProject
####################

outs <- SpaDES.project::preRunSetupProject(file = "global.R", upTo = "params")

####################
# RUN fireSense_ELFs to get the ELF map
####################

# Get all ELFs
queue_path <- "experiment_queue_predict5.rds"
outs$params$fireSense_ELFs$queue_path <- queue_path
.ELFinds <- fireSenseUtils::runELFs(outs, onlyFittedELFs = FALSE)

####################
# SET UP EXPERIMENT
####################

.reps <- 1
expt <- expand.grid(.ELFind = .ELFinds, .rep = .reps, stringsAsFactors = FALSE)
if (exists(".modules"))
  expt <- cbind(expt, .modules = I(lapply(seq_len(NROW(expt)), function(x) .modules)))
if (exists(".times"))
  expt <- cbind(expt, .times = I(lapply(seq_len(NROW(expt)), function(x) .times)))

# Some ELFs didn't work in earlier attempts; removing them here; they may be re-introduced later
failed <- c("5.1.1", "5.1.2", "5.1.3" # something in climate, missing in future tile 39; only has 2011,12
            # , "6.1.3"
            # , "5.4"# can't get past 1000000 in DEoptim
            , "3.1.1" # 
            
            , "5.2.2", "5.4", "11.2", "11.1"  # Error in purrr::pmap(.l = list(igOrEsc = whichProcessesToFit), sim = sim,  :
            #ℹ In index: 2.
            #ℹ With name: fireSense_EscapeFitted.
            #Caused by error in `roc.default()`:
            #  ! 'response' must have two levels
) 
expt <- expt[!expt$.ELFind %in% failed, ]

noFires <- "12.1" # had no fires
expt <- expt[!expt$.ELFind %in% noFires, ]

# Put them in an interesting order i.e., prioritize
top <- c("4", "6", "5", "9", "14", "12", "11", "15")
ord <- grepl(
  paste(paste0("^", top), collapse = "|"),
  expt$.ELFind   )
vals <- sapply(strsplit(expt$.ELFind[ord], "\\."), function(x) x[[1]])
ord2 <- match(vals, top)
ord3 <- as.numeric(!ord) * (max(ord2) + 1)
# ord3[ord3 == 0] <- 
expt <- expt[order(ord3), ]
first <- c("4.3", "6.1.1", "6.2.3","6.3.1")
expt <- rbind(expt[expt$.ELFind %in% first,], expt[!expt$.ELFind %in% first,])

rownames(expt) <- 1:NROW(expt) # re-number each row
####################
# Run the experiment -- this must be run at a command prompt, inside tmux
####################
workers <- SpaDES.project::experimentTmux(
  df                  = expt,          # df provided here
  global_path         = "global.R",
  n_workers           = 7,
  queue_path          = queue_path,
  delay_before_source = 120,
  folderWithDoneIndicator = quote({dd <- dir(file.path("outputs", runName), recursive = TRUE, full.names = TRUE)
                                  ee <- grep(value = TRUE, pattern = "objFun.*png$", dd)
                                  fi <- file.info(ee)
                                  tail(fi[order(fi$mtime),], 1) |> rownames() |> dirname()}),
  folderWithIterInFilename = quote({dd <- dir(file.path("outputs", runName), recursive = TRUE, full.names = TRUE)
                                   ee <- grep(value = TRUE, pattern = "hists", dd)
                                   fi <- file.info(ee)
                                   tail(fi[order(fi$mtime),], 1) |> rownames() |> dirname()}),
  workersToMonitor = outs$cores,
  runNameLabel = quote(colnames(q)[1]), # just first column in the queue
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