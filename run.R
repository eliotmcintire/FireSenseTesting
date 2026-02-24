repos <- c("https://predictiveecology.r-universe.dev", getOption("repos"))
source("https://raw.githubusercontent.com/PredictiveEcology/pemisc/refs/heads/development/R/getOrUpdatePkg.R")
getOrUpdatePkg(c("Require", "remotes"), c("1.0.1.9013", "0.0.0")) # only install/update if required
# remotes::install_github("PredictiveEcology/SpaDES.project@cacheRequire")

####################
# Run very partial setupProject to get modules, paths, times
####################
outs <- SpaDES.project::preRunSetupProject(file = "global.R", upTo = "modules")

####################
# Run SpreadFit Experiment - subset of the modules; shorter time of sim; 
#   - need lots of machines per run of global.R
#   - .ELFind is set inside expt.R
#   - MUST be run with `tmux` in a bash-like shell
####################
.times = modifyList(outs$times, list(end = 2020))
.modules <- grep("Predict|fireSense$", outs$modules, value = TRUE, invert = TRUE)
source("expt.R")


####################
# Run Predictions Experiment - all modules; 
#  - uses just 1 machine per run of global.R
#  - .ELFind is set inside expt.R
#   - MUST be run with `tmux` in a bash-like shell
####################
.times = outs$times # reset to global.R values
.modules <- outs$modules
source("expt.R")


