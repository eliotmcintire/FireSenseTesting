repos <- c("https://predictiveecology.r-universe.dev", getOption("repos"))
source("https://raw.githubusercontent.com/PredictiveEcology/pemisc/refs/heads/development/R/getOrUpdatePkg.R")
getOrUpdatePkg(c("Require", "SpaDES.project"), c("1.0.1.9021", "0.1.1.9050")) # only install/update if required

library(future)
future::plan("sequential")
future::plan(future.callr::callr(workers = 2, supervise  =  TRUE))
# future::plan(multisession(workers = 3))

outs <- SpaDES.project::preRunSetupProject(file = "global.R", upTo = "ELFs")
.ELFinds <- names(outs$ELFs$rasCentered)

# If you can run them in parallel on the same linux machine:
.reps <- 1
expt <- expand.grid(.ELFind = .ELFinds, .rep = .reps, stringsAsFactors = FALSE)

# Remove 1. and 2. as they are in the arctic
expt <- expt[-grep("^1\\.|^2\\.", expt$.ELFind), ] # ELFs in the 1. and 2. are in the far north; no burning
# Put 4, 6, 14 first as they are interesting
top <- c("4", "6", "14")
ord <- as.numeric(!grepl(
  paste0("^", paste(top, collapse = "|")),
  expt$.ELFind   ))
expt <- expt[order(ord), ]

p <- SpaDES.project::experiment3(expt)
