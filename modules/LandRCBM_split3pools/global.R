Require::Require("PredictiveEcology/SpaDES.core")
# Get the minimal amount of packages
repos <- c("predictiveecology.r-universe.dev", getOption("repos"))

## Objects
parameters <- list(
  LandRCBM_split3pools = list(.useCache = ".inputObjects", .plots = "png",
                              .plotInterval = 3)
  )

modules <- list("LandRCBM_split3pools")

# All the action is here
split3poolsInit <- simInit(
  params = parameters,
  modules = modules,
  paths = list(modulePath = "~/Repos/",
               inputPath = "~/Repos/LandRCBM_split3pools/inputs/",
               outputPath = "~/Repos/LandRCBM_split3pools/outputs/",
               cache = "~/Repos/LandRCBM_split3pools/cache/")
  )

split3poolsSim <- spades(split3poolsInit)
