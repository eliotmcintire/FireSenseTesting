repos <- c("https://predictiveecology.r-universe.dev", getOption("repos"))
source("https://raw.githubusercontent.com/PredictiveEcology/pemisc/refs/heads/development/R/getOrUpdatePkg.R")
getOrUpdatePkg(c("Require", "remotes"), c("1.0.1.9013", "0.0.0")) # only install/update if required
remotes::install_github("PredictiveEcology/SpaDES.project@cacheRequire")

#Require::Install(c(future, future.callr, googlesheets4))
#future::plan("sequential")
# future::plan("multicore", workers = 10)
# future::plan(future.callr::callr(workers = 1, supervise  =  TRUE))
# unlink(dir("logs", full.names = TRUE)) ; source("expt.R")
suppressWarnings(rm(.ELFind)) # This is a precaution as this may exist if there is a failure below; and this is rerun
# pkgload::load_all("~/GitHub/reproducible/");
# pkgload::load_all("~/GitHub/SpaDES.project/");

outs <- SpaDES.project::preRunSetupProject(file = "global.R", upTo = "params")
outs$modules <- grep("ELFs", outs$modules, value = TRUE)
sim <- SpaDES.core::simInitAndSpades2(outs) |> 
  Cache(.cacheExtra = list(src = asPath(file.path(outs$paths$modulePath, 
                                                  outs$modules, paste0(outs$modules, ".R"))),
                           objs = outs$ELFind),
        omitArgs = "l")
.ELFinds <- names(sim$ELFs$rasCentered)

# If you can run them in parallel on the same linux machine:
# 
# prepInputsFSURL <- "https://drive.google.com/drive/folders/1X9-mRjyLMNpgkP_cfqhbr_AQEPOsVCHf"
# gdLs <- googledrive::drive_ls(prepInputsFSURL)
# fireSenseParamsRDS <- sim@params$.globals$spreadFitFilename
# remoteFile <- gdLs[gdLs$name %in% fireSenseParamsRDS,]
# digRemote <- remoteFile$drive_resource[[1]]$md5Checksum
# gdMeta <- googledrive::drive_download(remoteFile, 
#                             path = file.path("/home/emcintir/GitHub/FireSenseTesting/inputs", remoteFile$name),
#                             overwrite = TRUE) |> 
#   reproducible::Cache(.cacheExtra = digRemote)
# aa <- readRDS(gdMeta$local_path)
# aa

.reps <- 1
expt <- expand.grid(.ELFind = .ELFinds, .rep = .reps, stringsAsFactors = FALSE)

top <- c("4", "6", "5")
ord <- grepl(
  paste(paste0("^", top), collapse = "|"),
  expt$.ELFind   )
vals <- sapply(strsplit(expt$.ELFind[ord], "\\."), function(x) x[[1]])
ord2 <- match(vals, top)
ord3 <- as.numeric(!ord) * (max(ord2) + 1)
ord3[ord3 == 0] <- 
expt <- expt[order(ord3), ]
first <- c("4.3", "6.1.1", "6.2.3","6.3.1")
expt <- rbind(expt[expt$.ELFind %in% first,], expt[!expt$.ELFind %in% first,])

if (TRUE) {
  failed <- c("5.1.1", "5.1.2", "5.1.3", # something in climate, missing in future tile 39; only has 2011,12
              "6.1.3",
              "5.4") # can't get past 1000000 in DEoptim
  expt <- expt[!expt$.ELFind %in% failed, ]
  
  # This is manually derived
  #noFit <- c("9.2.3", "6.6.1", "6.1.2")
  #expt <- expt[!expt$.ELFind %in% noFit, ]
  
  noFires <- "12.1"
  expt <- expt[!expt$.ELFind %in% noFires, ]
}

# only run the ones that have not been fitted

expt <- expt[!expt$.ELFind %in% sim$spreadFitPreRun$polygonID,]

rownames(expt) <- 1:NROW(expt)

if (FALSE) {
  # This is the abandonned experiment3 way
  future::plan("cluster", workers = min(NROW(expt), 3), persistent = TRUE)
  p <- SpaDES.project::experiment3(expt, file = "global.R",
                                   saveSimToDisk = FALSE, tmuxName = "ex8")
}

########
if (FALSE) {
  # get google authentication
  sss <- readLines("~/googledriveAuthentication.R")
  eval(parse(text = sss)) |> options()
  
}
workers <- SpaDES.project::tmux_spawn_workers_from_df(
  df                  = expt,          # df provided here
  global_path         = "global.R",
  n_workers           = 6,
  queue_path          = "experiment_queue_predict5.rds",
  delay_before_source = 120,
  workersToMonitor = outs$cores,
  ss_id = "https://drive.google.com/drive/folders/1X9-mRjyLMNpgkP_cfqhbr_AQEPOsVCHf"
)

if (FALSE) {
  # For estimating elapsed time
  sim = SpaDES.core:::savedSimEnv()$.sim
  ee = elapsedTime(sim)
  ee[, Predict := c("Fit", "Predict")[1 + as.numeric(grepl("redict", moduleName) | grepl("Dispersal|mortalityAndGrowth|summaryBGM", eventType))]]
  ee[, sum (elapsedTime), by = Predict]
  sim$.runName
}