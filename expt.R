repos <- c("https://predictiveecology.r-universe.dev", getOption("repos"))
source("https://raw.githubusercontent.com/PredictiveEcology/pemisc/refs/heads/development/R/getOrUpdatePkg.R")
getOrUpdatePkg(c("Require", "SpaDES.project"), c("1.0.1.9021", "0.1.1.9054")) # only install/update if required

#Require::Install(c(future, future.callr, googlesheets4))
#future::plan("sequential")
# future::plan("multicore", workers = 10)
# future::plan(future.callr::callr(workers = 1, supervise  =  TRUE))

outs <- SpaDES.project::preRunSetupProject(file = "global.R", upTo = "ELFs")
.ELFinds <- names(outs$ELFs$rasCentered)

# If you can run them in parallel on the same linux machine:
# aa <- reproducible::prepInputs(targetFile = "fireSenseParams.rds", url = "https://drive.google.com/file/d/1-iD7Pj4cX3kag4TEHeGxGgW42Rf0ag2l/view?usp=drivesdk",
#                                destinationPath = "/home/emcintir/GitHub/FireSenseTesting/inputs",
#                                useCache = TRUE, purge = 7, overwrite = TRUE)

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
ord3[ord3 == 0] <- ord2
expt <- expt[order(ord3), ]
first <- c("4.3", "6.1.1", "6.2.3","6.3.1")
expt <- rbind(expt[expt$.ELFind %in% first,], expt[!expt$.ELFind %in% first,])
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

# pkgload::load_all("~/GitHub/SpaDES.project/");
########
queue_path <- "experiment_queue.rds"
global <- "global.R"
workers <- SpaDES.project::tmux_spawn_workers_from_df(
  df                  = expt,          # df provided here
  global_path         = global,
  n_workers           = 4,
  start_cmd           = "R",
  queue_path          = queue_path,
  delay_before_source = 10,
  ss_id = "https://drive.google.com/drive/folders/1X9-mRjyLMNpgkP_cfqhbr_AQEPOsVCHf"
)

# If something goes wrong during development:
#' tmux_kill_panes(workers)



# googlesheets4::gs4_auth(email = "eliotmcintire@gmail.com", cache = ".secrets")

