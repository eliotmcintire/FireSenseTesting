# projectDir <- "/mnt/storage/md2/Eliot/FireSenseTesting/"
# packagePath <- file.path(dirname(projectDir), "packages", "4.3")
# dir.create(packagePath, FALSE, TRUE)
# .libPaths(packagePath)
# packageVersion("SpaDES.project")
# projectDir <- "/mnt/storage/md2/Eliot/FireSenseTesting/"
# packagePath <- file.path(dirname(projectDir), "packages", "4.3")
# dir.create(packagePath, FALSE, TRUE)
# .libPaths(packagePath)
# repos <- c("https://predictiveecology.r-universe.dev", getOption("repos"))
# source("https://raw.githubusercontent.com/PredictiveEcology/pemisc/refs/heads/development/R/getOrUpdatePkg.R")
# Sys.setenv("R_REQUIRE_CACHE" = file.path(dirname(projectDir), "packageCache"))
# Sys.setenv("HOME" = "/mnt/storage/md2/Eliot")
# getOrUpdatePkg(c("Require", "SpaDES.project"), c("1.0.1.9013", "0.1.1.9053")) # only install/update if required


# # install Require and SpaDES.project
repos <- c("https://predictiveecology.r-universe.dev", getOption("repos"))
source("https://raw.githubusercontent.com/PredictiveEcology/pemisc/refs/heads/development/R/getOrUpdatePkg.R")
getOrUpdatePkg(c("Require", "SpaDES.project"), c("1.0.1.9013", "0.1.1.9053")) # only install/update if required

# generic absolute path for anybody; but individual can change
projectDir <- "~/GitHub/FireSenseTesting/"
dir.create(projectDir, recursive = TRUE, showWarnings = FALSE)
setwd(projectDir)

# pkgload::load_all("~/GitHub/SpaDES.project/");
# debug(setupProject)
# debug(experiment3)
# pkgload::load_all("~/GitHub/fireSenseUtils/");
# undebug(makeELFs)
# devtools::install("~/GitHub/SpaDES.project/", upgrade = FALSE);
# if (exists(".ELFind")) {
#   startedFile <- file.path("logs", paste0("Running_", .ELFind, "_", Sys.getpid(), "_.rds"))
#   alreadyExists <- dir(dirname(startedFile), pattern = .ELFind, full.names = TRUE)
#   if (length(alreadyExists)) {
#     if (difftime(file.info(alreadyExists)$mtime, Sys.time()) < 20)
#     unlink(alreadyExists)
#   }
#   saveRDS(.ELFind, file = startedFile)
# }
inSim <- SpaDES.project::setupProject(
  ELFind = gsub("ELF", "", .ELFind),
  .runName = ELFind,
  .rep = .rep,
  .strategy = .strategy,
  .cc = .cc,
  cores = .cores,
  FRU = FRU,
  defaultDots = list(.strategy = 1L, # used to be 3L; but seems to get caught in local minima
                     .cc = 0.5,
                     .objfunFireReps = 25L,
                     .rep = 1,
                     .ELFind = "4.3",
                     .cores = c("birds", "biomass", "camas", "carbon", "caribou", "coco",
                                "core", "dougfir", "fire", 
                                "mpb", "sbw", "mega",
                                "acer", 
                                "abies", "pinus"
                     ),
                     FRU = 25),
  .objfunFireReps = .objfunFireReps,
  # useGit = "eliotmcintire",
  Restart = TRUE,
  paths = list(outputPath = file.path("outputs", ELFind)),
  modules = c("PredictiveEcology/canClimateData@improveCache1"
              
              , "PredictiveEcology/fireSense_ELFs@main"

              , "PredictiveEcology/fireSense_dataPrepFit@development"
              , "PredictiveEcology/fireSense_IgnitionFit@development"
              , "PredictiveEcology/fireSense_SpreadFit@development"

              , "PredictiveEcology/fireSense_dataPrepPredict@development" # prepares data for predictions
              , "PredictiveEcology/fireSense_IgnitionPredict@development" # predicts ignitions & escapes
              , "PredictiveEcology/fireSense_SpreadPredict@development" # predicts raster of spreadProb

              , "PredictiveEcology/fireSense@development" # does burning

              , "PredictiveEcology/Biomass_borealDataPrep@development"
              , "PredictiveEcology/Biomass_speciesParameters@development"
              , "PredictiveEcology/Biomass_speciesData@development"
              , "PredictiveEcology/Biomass_regeneration@development"
              , "PredictiveEcology/Biomass_core@development"

  ),
  packages = c("reproducible (>= 3.0.0)" # (HEAD)", # (HEAD)",
               , "qs2"
               , "archive"
               , "googlesheets4"
               ,"PredictiveEcology/climateData@modsDuringFireSense3 (>= 2.2.2.9000)"
               ,"SpaDES.core (>= 3.0.0)" # (HEAD)", # needed for the functions in
               , "terra" # "leaflet", "tidyterra",
               , "plyr"#, "scfmutils",
               , "rvest" # needed for prepIgnitionFitData
  ),
  require = "reproducible",
  times = list(start = 2020, end = 2100),
  options = list(# gargle_oauth_email = "predictiveecology@gmail.com",
    # gargle_oauth_cache = ".secret",
    # gargle_oauth_client_type = "web", # for command line
    "~/googledriveAuthentication.R" # has the above lines; each user can create their own file
    , repos = unique(c(# repos[[1]], 
                       'https://dmlc.r-universe.dev', getOption("repos")))
    , reproducible.cacheSaveFormat = "qs2"
    , reproducible.qsFormat = "qs2"
    , reproducible.useTry = FALSE
    , SpaDES.project.fast = FALSE
    , reproducible.shapefileRead = "terra::vect"
    , reproducible.overwrite = TRUE
    , reproducible.inputPaths = "~/data"
    , reproducible.cloudFolderID = "1oNGYVAV3goXfSzD1dziotKGCdO8P_iV9"
    , reproducible.showSimilarDepth = 8
    , reproducible.objSize = FALSE
    # Eliot during development
    , reproducible.savePreDigest = FALSE
    # , spades.debugModule = NA #c("canClimateData", "fireSense_dataPrepPredict",
    #                       "fireSense_dataPrepFit", "fireSense_SpreadFit",
    #                      "Biomass_speciesData", "Biomass_borealDataPrep"), # "fireSense_dataPrepFit", # NA
    , fireSenseUtils.runTests = FALSE
    , reproducible.memoisePersist = TRUE # sets the memoise location to .GlobalEnv; persists through a `load_all`
    , reproducible.nThreads = 4 # 
    # , repos = unique(c('https://dmlc.r-universe.dev', getOption("repos"))) # needed for xgboost
    , reproducible.inputPaths = "~/data" # means I can share data from other projects
    , reproducible.prepInputsUrlTiles = "https://drive.google.com/drive/folders/1IfeQ9rZ3-RIQwtcdo2T5Kn51NJJRWeox?usp=drive_link"
    , spades.useRequire = TRUE

    , error = recover
    
    # For batch runs, these should be off
    , reproducible.showSimilar = FALSE#interactive() && !nzchar(Sys.getenv("TMUX"))
    , reproducible.useMemoise = interactive() && !nzchar(Sys.getenv("TMUX"))
    , spades.recoveryMode = 5#(interactive() && !nzchar(Sys.getenv("TMUX"))) + 0
    , spades.cacheChaining = FALSE#TRUE
    , reproducible.cacheChaining = FALSE #interactive()

    , reproducible.gdalwarp = FALSE
    , Require.cloneFrom = Sys.getenv("R_LIBS_USER")
    , spades.moduleCodeChecks = FALSE
    # , spades.memoryUseInterval = 2
    , spades.allowInitDuringSimInit = TRUE),
  sideEffects = list(
    terra::terraOptions(memfrac = 0)
    , terra::gdalCache(size = 2048)   # 2 GB
    , pkgload::load_all("~/GitHub/reproducible/")
    , pkgload::load_all("~/GitHub/SpaDES.core/")
    , pkgload::load_all("~/GitHub/SpaDES.tools/")
    ,  pkgload::load_all("~/GitHub/clusters/")
    , pkgload::load_all("~/GitHub/LandR/")
    , pkgload::load_all("~/GitHub/fireSenseUtils/")
     # , pkgload::load_all("~/GitHub/climateData/")
  ),
  .climVars = c("CMD_sm", "CMD_sp"),
  climateVariables = {
    climateData::climateLayers(.climVars, fun = quote(quote(calcAsIs)))
  },
  climateVariablesForFire = list(ignition = gsub("_", "", .climVars), # This must match a layer in climateVariables (without 'historical_')
                                 # only sm for spread
                                 spread = gsub("_", "", grep("sm$", .climVars, value = TRUE))), # This must match a layer in climateVariables (without 'historical_')
  params = list(
    .globals = list(
      spreadFitFilename = "fireSenseParams_2026_02.rds",
      # dataYear = 2011,
      .studyAreaName = .runName,
      .plots = c("png"),
      sppEquivCol = "LandR", # will get a warning if this is not here
      .useCache = c(".inputObjects", "init", "initPlot", "estimateThreshold", "spreadFitPrepare", "checkData"),
      minCoverThreshold = 0),
    # fireSense_ELFs = list(.useCache = FALSE),
    # canClimateData = list(.useCache = ".inputObjects"),  # init is slow to cache
    # fireSense = list(.plots = c("screen", "png")),
    fireSense_SpreadFit = list(
      DEoptimTests = c("adTest", "SNLL_FS"),
      # mutuallyExclusiveCols = list(
      #   youngAge = c("nf", unique(makeSppEquiv(ecoprovinceNum = ecoprovince)$fuel))
      # ),
      # .useCache = FALSE,
      iterDEoptim = 1000,
      rep = .rep, # This means that all Cache of DEoptim will now be different name
      iterStep = 1, # run this many iterations before running again; this should be
      # set to itermax if Cache is not used; it is only useful for Cache
      cores = cores,
      NP = {if (identical(cores, unique(cores))) 100 else length(cores)}, # number of cores of machines
      trace = 1,
      mode = "fit",# "visualize"),
      # mode = "debug",
      strategy = .strategy,
      objfunFireReps = .objfunFireReps, # this is the lowest that doesn't create an error
      .c = .cc,
      # SNLL_FS_thresh = snll_thresh,
      doObjFunAssertions = FALSE
    ),
    fireSense_dataPrepFit = list(
      # missingLCCgroup = c("nf_dryland"), # must match fuel class land cover
      .useCache = c(".inputObjects",
                    "init", # CAN'T cache this one because it is the trigger to "skip" a whole bunch if SpreadParams exist for the StudyArea
                    "dataPrepInit",
                    "prepEscapeFitData",
                    "prepSpreadFitData",
                    "prepIgnitionFitData",
                    "run")
    ),
    fireSense_IgnitionFit = list(
      rescalers = c("CMDsm" = 1000),
      .useCache = c(".inputObjects", "init", "prepIgnitionFitData", "run")
    )
  )
)


if (FALSE) {
  prepInputs(targetFile = "fireSenseParams.rds", url = "https://drive.google.com/file/d/1-iD7Pj4cX3kag4TEHeGxGgW42Rf0ag2l/view?usp=drivesdk",
             destinationPath = "/home/emcintir/GitHub/FireSenseTesting/inputs",
             useCache = TRUE, purge = 7, overwrite = TRUE)
}
message(paste0(inSim$.runName, ", .rep:", inSim$.rep, ", .strategy:", inSim$.strategy,
               " .objfunFireReps:", inSim$.objfunFireReps))

# if (TRUE) {
if (SpaDES.project::user("emcintir"))
  Sys.setenv(TMPDIR = file.path("~/tmp/", attr(inSim$paths, "extraPaths")$projectPath)) #
# a <- fireSenseUtils::fireSenseCloudParametersMap()
inSim$climateVariables <- climateData::climateLayers(inSim$.climVars, fun = quote(calcAsIs))

library(SpaDES.project)
if (FALSE) {
  SpaDES.core::Plots(inSim[grep("studyArea|rasterToMatch", names(inSim))],
                       title = paste0("StudyArea ", inSim$.runName),
                       fn = plotSAs,
                       filename = paste0("studyAreas", inSim$.runName),
                       path = inSim$paths$inputPath,
                       types = c("screen", "png")) |>
      reproducible::Cache(.functionName = "Plots_studyAreas",
                          useCache = !identical(names(dev.cur()), "null device"))
}

#known bugs/undesirable behavior
#1 spreadFit dumps a bunch of figs in the project directory instead of outputs

if (FALSE) {

  SpaDES.project::plotSAsLeaflet(inSim[grep("studyArea|rasterToMatch", names(inSim))])

  fn <- "sim_FireSenseSpreadFit.qs2"
  saveState(filename = fn, files = FALSE)
  inSim2 <- SpaDES.core::loadSimList(fn)
  outSims <- restartSpades(inSim2)
  outSims <- restartSpades()
}
inSimCopy <- reproducible::Copy(inSim)

if (FALSE) {
  if (quickPlot::isRstudioServer()) {
    pkgload::load_all("~/GitHub/reproducible/");
    pkgload::load_all("~/GitHub/SpaDES.core/");
  }
  pkgload::load_all("~/GitHub/SpaDES.project/");
  pkgload::load_all("~/GitHub/clusters/");
  pkgload::load_all("~/GitHub/LandR/");
  pkgload::load_all("~/GitHub/climateData/");
  #  pkgload::load_all("~/GitHub/scfmutils/");
  pkgload::load_all("~/GitHub/fireSenseUtils/");

  if (FALSE) {
    devtools::install("~/GitHub/SpaDES.project/", upgrade = FALSE);
    devtools::install("~/GitHub/reproducible/", upgrade = FALSE);
    devtools::install("~/GitHub/SpaDES.core/", upgrade = FALSE);
    devtools::install("~/GitHub/clusters/", upgrade = FALSE);
    devtools::install("~/GitHub/LandR/", upgrade = FALSE);
    devtools::install("~/GitHub/fireSenseUtils/", upgrade = FALSE);
    devtools::install("~/GitHub/climateData/", upgrade = FALSE);
  }

}

memBefore <<- lobstr::mem_used()
# Require::Install("pkgload")
# pkgload::load_all("~/GitHub/clusters/");
# debug(SpaDES.core::loadSimList)
# options(spades.cacheChaining = TRUE)
# debug(prepSpeciesTable)# ; undebug(cacheChainingPost)
# options("spades.dotInputObjects" = FALSE)
# browser()
# sim <- simInit2(inSim)
if (TRUE) {
  st <- Sys.time()

  options(
    #  rstLCC in 2nd time is "8882282dd8bcd415"
    spades.evalPostEvent = NULL
      # quote({
      #   library(lobstr)
      #   
      #   memAfter  <<- lobstr::mem_used()
      #   print(paste("Difference After - Before: ", capture.output(memAfter - memBefore)))
      #   print(paste0("MemUsed: ", capture.output(lobstr::mem_used())))
      #   
      #   print("NEXT MODULE:")
      #   memBefore <<- lobstr::mem_used()
      #   gc()
      #   
      #   # run_module_X()
      #   
      # })
      # quote({print(sort(sim$sppEquiv$LandR))
      #        print(names(sim$sppColorVect))
      #   #print(sim$sppColorVect)
      #   #print(.robustDigest(sim[["standAgeMap"]]))
      # })
    # quote({# print(.robustDigest(sim$spreadFirePolys));
    #   print(.robustDigest(sim$rasterToMatch_biomassParam));
    #   print(.robustDigest(sim[["standAgeMap"]]))
    # })
    # quote({
    #   print(sim$standAgeMap); print(.robustDigest(sim$standAgeMap));
    #   print(sim$rstLCC); print(.robustDigest(sim$rstLCC));
    #   print(sim$studyArea); .robustDigest(sim$studyArea)
    # })
    , spades.debugModule = NULL#"fireSense_SpreadFit"
    #, reproducible.useMemoise = FALSE
  )
  # debug(rasterizeReduced)
  startedFile <- file.path("logs", paste0("Running_", inSim$ELFind, "_", Sys.getpid(), "_.rds"))
  reproducible::checkPath(dirname(startedFile), create = TRUE)
  saveRDS(inSim$ELFind, file = startedFile)
  withCallingHandlers(
  suppressPackageStartupMessages(
    simOut <- SpaDES.core::simInitAndSpades2(inSimCopy)
  ), error = function(e) {
    unlink(startedFile)  
  })
  # If it makes it here, then remove it so that the next iteration doesn't pick this up
  # rm(list = ".sim", envir= SpaDES.core:::savedSimEnv())
}
a <- inSim$ELFind
