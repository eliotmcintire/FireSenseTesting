repos <- c("https://predictiveecology.r-universe.dev", getOption("repos"))
# source("https://raw.githubusercontent.com/PredictiveEcology/pemisc/refs/heads/development/R/getOrUpdatePkg.R")
# getOrUpdatePkg(c("Require", "remotes"), c("1.0.1.9013", "0.0.0")) # only install/update if required
# remotes::install_github("PredictiveEcology/SpaDES.project@development", upgrade = FALSE)
# remotes::install_github("PredictiveEcology/SpaDES.projec")

devPkgs <- c("PredictiveEcology/Require@usePak", 
             "PredictiveEcology/reproducible@COG",
             "PredictiveEcology/SpaDES.project@working/combined-prs",
             "PredictiveEcology/SpaDES.core@fixRCMDcheckWarnings",
             "PredictiveEcology/climateData@modsDuringFireSense3")
source("installDevPkgs.R", local = environment())


suppressWarnings(rm(.ELFind)) # This is a precaution as this may exist if there is a failure below; and this is rerun

SpaDES.project::register_scenario_format(withFieldLabel = c(.SSP = "ssp", .rep = "rep")) # for pathBuild below

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

.reps <- 1:5 # how many reps do we want?
.GCMs <- list(future = c("CNRM-ESM2-1"), past = "NRV") # used in 
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
rownames(expt) <- 1:NROW(expt) # re-number each row

expt <- unique(expt, by = colnames(expt)[!sapply(expt, is, "list")])

####################
# Run the experiment -- this must be run at a command prompt, inside tmux
#  --> MUST MONITOR `activeRunningPath`. It should represent only "currently running ELFs".
#      If an ELF has stopped running, and the log file is still present, then it must be
#      manually deleted (automated deletion has failed)
####################
workers <- SpaDES.project::experimentTmux(
  df                  = expt,          # df provided here
  global_path         = "global.R",
  # enableGSSync = TRUE,
  # cores = rep("localhost", 2),
  cores = c(

    ## long4
    rep("localhost", 6)
    
    # ## long4
    # rep("fire", 2),
    # "sbw",
    # "camas"
    
    # ## long3
    # rep("fire", 2),
    #  "sbw",
    #  "camas"
    
    # ## long2
    # rep("localhost", 2),
    # "biomass",
    # "caribou",
    # "dougfir"
    
    ## long
    #   rep(c(
    #     "dougfir"
    #     , "mpb"
    #     # , "localhost"
    #   ), 2),
    #   rep(c(
    #     "fire"
    #     , "coco"
    #   ), 2),
    #   rep(c(
    #     "mpb"
    #     , "coco"
    #     , "camas"
    #     , "carbon"
    #     , "caribou"
    #     , "core" 
    #     , "sbw"
    #   ), 1),
    #     # "birds", "biomass", 
    #   c("acer"
    #     , "birds"
    #     # "biomass", 
    #     , "abies"
    #     , "pinus")
  ) |> 
    sort(), # sort puts them next to each other in tmux
  # cores = c(rep("biomass", 2), rep("coco", 2)),
  queue_path          = "longRuns.rds",
  delay_before_source = 15,
  times = outs$times,
  runNameLabel = quote(SpaDES.project::pathBuild(.ELFind, .samplingRange, .GCM, .SSP, .rep)), #c("ELFind", "rep", "GCM"),
  on_interrupt = "fail",
  forceLocalQueueToGS = FALSE,
  
  statusCalculate = SpaDES.project::statusCalculate_LandR
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