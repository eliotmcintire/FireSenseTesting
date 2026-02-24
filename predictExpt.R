repos <- c("https://predictiveecology.r-universe.dev", getOption("repos"))
source("https://raw.githubusercontent.com/PredictiveEcology/pemisc/refs/heads/development/R/getOrUpdatePkg.R")
getOrUpdatePkg(c("Require", "remotes"), c("1.0.1.9013", "0.0.0")) # only install/update if required
# remotes::install_github("PredictiveEcology/SpaDES.project@cacheRequire")


suppressWarnings(rm(.ELFind)) # This is a precaution as this may exist if there is a failure below; and this is rerun

####################
# RUN fireSense_ELFs to get the ELF map
####################

outs <- SpaDES.project::preRunSetupProject(file = "global.R", upTo = "params")

# system.time({
outs$modules <- grep("ELFs", outs$modules, value = TRUE)
# For digesting; i.e., whether it needs to be re-run
srcFiles <- asPath(dir(file.path(outs$paths$modulePath, outs$modules), 
                       pattern = "\\.R$", recursive = TRUE, full.names = TRUE) |> 
                     grep(pattern = "tests\\/", invert = TRUE, value = TRUE))
urlELFresults <- "https://drive.google.com/file/d/1tkC944mPzR9-y-qCMDB5o2cAR_1MoDz4/view?usp=drive_link"
md <- reproducible:::getRemoteMetadata(url = urlELFresults)
outs$params$fireSense_ELFs$hashSpreadFitRemoteFile <- md$remoteHas

# Run the module
sim <- SpaDES.core::simInitAndSpades2(outs) |> 
  Cache(.cacheExtra = list(src = srcFiles, remoteHash = md$remoteHash),
        omitArgs = "l")

# Upload if it is emcintir; and if it has changed
if (SpaDES.project::user() %in% "emcintir") {
  cid <- cacheId(sim)
  ll <- googledrive::drive_update(file = urlELFresults,
                                  media = grep("ELF", sim@outputs$file, value = TRUE)) |> 
    Cache(omitArgs = c("file", "media"), .cacheExtra = list(cacheId = cid))
}
# })

.ELFinds <- sim$spreadFitPreRun$polygonID

####################
# SET UP EXPERIMENT
####################

.reps <- 1
expt <- expand.grid(.ELFind = .ELFinds, .rep = .reps, stringsAsFactors = FALSE)
if (exists(".modules"))
  expt <- cbind(expt, .modules = I(lapply(seq_len(NROW(expt)), function(x) .modules)))
if (exists(".times"))
  expt <- cbind(expt, .times = I(lapply(seq_len(NROW(expt)), function(x) .times)))
rownames(expt) <- 1:NROW(expt) # re-number each row
####################
# Run the experiment -- this must be run at a command prompt, inside tmux
####################
workers <- SpaDES.project::experimentTmux(
  df                  = expt,          # df provided here
  global_path         = "global.R",
  n_workers           = 2,
  queue_path          = "predict_queue.rds",
  delay_before_source = 15,
  workersToMonitor = NULL,
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