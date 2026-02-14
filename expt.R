repos <- c("https://predictiveecology.r-universe.dev", getOption("repos"))
source("https://raw.githubusercontent.com/PredictiveEcology/pemisc/refs/heads/development/R/getOrUpdatePkg.R")
getOrUpdatePkg(c("Require", "SpaDES.project"), c("1.0.1.9013", "0.1.1.9054")) # only install/update if required

#Require::Install(c(future, future.callr, googlesheets4))
#future::plan("sequential")
# future::plan("multicore", workers = 10)
# future::plan(future.callr::callr(workers = 1, supervise  =  TRUE))
# unlink(dir("logs", full.names = TRUE)) ; source("expt.R")
suppressWarnings(rm(.ELFind)) # This is a precaution as this may exist if there is a failure below; and this is rerun
pkgload::load_all("~/GitHub/SpaDES.project/");

outs <- SpaDES.project::preRunSetupProject(file = "global.R", upTo = "params")
outs$modules <- grep("ELFs", outs$modules, value = TRUE)
sim <- SpaDES.core::simInitAndSpades2(outs) |> 
  Cache(.cacheExtra = list(src = asPath(file.path(outs$paths$modulePath, 
                                                  outs$modules, paste0(outs$modules, ".R"))),
                           objs = outs$ELFind),
        omitArgs = "l")
.ELFinds <- names(sim$ELFs$rasCentered)

# If you can run them in parallel on the same linux machine:
prepInputsFSURL <- "https://drive.google.com/file/d/1M6j3KMkr8oIgWcqrWGdyYMXskNj1deQm/view?usp=drivesdk"
digRemote <- reproducible:::getRemoteMetadata(url = prepInputsFSURL, isGDurl = TRUE)$remoteHash
aa = prepInputs(targetFile = "fireSenseParams.rds", 
                url = prepInputsFSURL,
                destinationPath = "/home/emcintir/GitHub/FireSenseTesting/inputs",
                useCache = FALSE, purge = 7, overwrite = TRUE) |>
  reproducible::Cache(.cacheExtra = digRemote)

.reps <- 1
expt <- expand.grid(.ELFind = .ELFinds, .rep = .reps, stringsAsFactors = FALSE)

# Remove 1. and 2. as they are in the arctic
expt <- expt[-grep("^1\\.|^2\\.", expt$.ELFind), ] # ELFs in the 1. and 2. are in the far north; no burning
# Can specify which first, in order
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
expt # done <- c("6.1.2", "4.3", "6.1.1", "6.2.3", "6.3.1")
# expt <- expt[!expt$.ELFind %in% done, ]

if (TRUE) {
  failed <- c("5.1.1", "5.1.2", "5.1.3", # something in climate, missing in future tile 39; only has 2011,12
              "6.1.3",
              "5.4") # can't get past 1000000 in DEoptim
  expt <- expt[!expt$.ELFind %in% failed, ]
  
  # This is manually derived
  noFit <- c("9.2.3", "6.6.1", "6.1.2")
  expt <- expt[!expt$.ELFind %in% noFit, ]
  
  noFires <- "12.1"
  expt <- expt[!expt$.ELFind %in% noFires, ]
}

# only run the ones that have fitted
expt <- expt[expt$.ELFind %in% aa$polygonID,]


# expt <- expt[1, ]
# only do missing ones
# expt <- expt[!expt$.ELFind %in% aa$polygonID, ]
rownames(expt) <- 1:NROW(expt)
# expt <- expt[!expt$.ELFind %in% c("6.4", "6.2.2"),]

if (FALSE) {
  future::plan("cluster", workers = min(NROW(expt), 3), persistent = TRUE)
  # future::plan(future.callr::callr(workers = min(NROW(expt), 2), supervise  =  TRUE))
  # debug()
  
  p <- SpaDES.project::experiment3(expt, file = "global.R",
                                   saveSimToDisk = FALSE, tmuxName = "ex8")
  
}

########
if (FALSE) {
  # get google authentication
  sss <- readLines("~/googledriveAuthentication.R")
  eval(parse(text = sss))
  
}
queue_path <- "experiment_queue_predict5.rds"
# queue_path <- "experiment_queue_predict.rds"
global <- "global.R"
SpaDES.project::tmux_prepare_queue_from_df(queue_path = queue_path, expt)
SpaDES.project::tmux_refresh_queue_status(queue_path)
# debug(tmux_spawn_workers_from_df)
workers <- SpaDES.project::tmux_spawn_workers_from_df(
  df                  = expt,          # df provided here
  global_path         = global,
  n_workers           = min(20, NROW(expt)),
  start_cmd           = "R",
  queue_path          = queue_path,
  delay_before_source = 10,
  workersToMonitor = sim$cores,
  ss_id = "https://drive.google.com/drive/folders/1X9-mRjyLMNpgkP_cfqhbr_AQEPOsVCHf"
)

if (FALSE) {
  # For estimating elapsed time
  sim = SpaDES.core:::savedSimEnv()$.sim
  ee = elapsedTime(sim)
  ee[, Predict := c("Fit", "Predict")[1 + as.numeric(grepl("redict", moduleName) | grepl("Dispersal|mortalityAndGrowth|summaryBGM", eventType))]]
  ee[, sum (elapsedTime), by = Predict]
  sim$.runName
  
  # If something goes wrong during development:
#' tmux_kill_panes(workers)
# googlesheets4::gs4_auth(email = "eliotmcintire@gmail.com", cache = ".secrets")
  # rsync -avH --delete --relative --progress --info=progress2 --stats ./data ./GitHub/FireSenseTesting/inputs mega:~
  # rsync -avH --delete --relative --progress --info=progress2 --stats ./data ./GitHub/FireSenseTesting/cache mega:~  
  # rsync -avH --delete --relative --progress --info=progress2 --stats ./data ./GitHub/FireSenseTesting/outputs mega:~  
  #     
  # # rsync -aH   --info=progress2 --stats --delete ./cache/ mega:~/GitHub/FireSenseTesting/cache/
  # # rsync -aH   --info=progress2 --stats --delete ./outputs/ mega:~/GitHub/FireSenseTesting/outputs/
  # # rsync -aH   --info=progress2 --stats --delete ./inputs/ mega:~/GitHub/FireSenseTesting/inputs/
}