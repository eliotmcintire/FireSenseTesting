repos <- c("https://predictiveecology.r-universe.dev", getOption("repos"))
source("https://raw.githubusercontent.com/PredictiveEcology/pemisc/refs/heads/development/R/getOrUpdatePkg.R")
getOrUpdatePkg(c("Require", "SpaDES.project"), c("1.0.1.9021", "0.1.1.9054")) # only install/update if required

Require::Install(c(future, future.callr))
future::plan("sequential")
future::plan(future.callr::callr(workers = 3, supervise  =  TRUE))
# future::plan("multicore", workers = 10)
# future::plan(future.callr::callr(workers = 1, supervise  =  TRUE))

outs <- SpaDES.project::preRunSetupProject(file = "global.R", upTo = "ELFs")
.ELFinds <- names(outs$ELFs$rasCentered)

# If you can run them in parallel on the same linux machine:
.reps <- 1
expt <- expand.grid(.ELFind = .ELFinds, .rep = .reps, stringsAsFactors = FALSE)
# Remove 1. and 2. as they are in the arctic
expt <- expt[-grep("^1\\.|^2\\.", expt$.ELFind), ] # ELFs in the 1. and 2. are in the far north; no burning
# Can specify which first, in order
top <- c("6", "4", "14")
ord <- grepl(
  paste(paste0("^", top), collapse = "|"),
  expt$.ELFind   )
vals <- sapply(strsplit(expt$.ELFind[ord], "\\."), function(x) x[[1]])
ord2 <- match(vals, top)
ord3 <- as.numeric(!ord) * (max(ord2) + 1)
ord3[ord3 == 0] <- ord2
expt <- expt[order(ord3), ]

# expt <- expt[3, ]
# debug(experiment3)
# ppkgload::load_all("~/GitHub/SpaDES.project/");
p <- SpaDES.project::experiment3(expt, saveSimToDisk = TRUE, tmuxName = "ex3")
