# # install Require and SpaDES.project
repos <- c("https://predictiveecology.r-universe.dev", getOption("repos"))
# pak::pak("~/GitHub/SpaDES.project", ask = FALSE, upgrade = FALSE)
# pak::pak("PredictiveEcology/Require@pak-dep-cache", ask = FALSE, upgrade = FALSE)
tryCatch(library(pak), silent = TRUE, error = function(x) install.packages("pak"))
# source("https://raw.githubusercontent.com/PredictiveEcology/pemisc/refs/heads/development/R/getOrUpdatePkg.R")
# getOrUpdatePkg(c("Require", "SpaDES.project"), c("1.0.1.9013", "0.1.4.9008")) # only install/update if required
# getOrUpdatePkg(c("Require"), c("1.0.1.9013")) # only install/update if required
# remotes::install_github("PredictiveEcology/SpaDES.project", upgrade = FALSE)

#pak::pak(c("PredictiveEcology/Require@usePak", "PredictiveEcology/reproducible@recovery",
#           "PredictiveEcology/SpaDES.project@working/combined-prs"), ask = FALSE)

# generic absolute path for anybody; but individual can change
projectDir <- "~/GitHub/FireSenseTesting/"
if (Sys.info()["user"] == "ieddy"){
  projectDir <- "~/git/FireSenseTesting"
}
dir.create(projectDir, recursive = TRUE, showWarnings = FALSE)
setwd(projectDir)

pathBuild <- function(.ELFind, .samplingRange, .GCM, .SSP, .rep, pre = "outputs") {
  # the .samplingRange may come in as a numeric or as a quoted/call
  sr <- if (is.numeric(.samplingRange)) .samplingRange else eval(parse(text = .samplingRange))
  file.path(pre, .ELFind, 
            paste(range(sr), collapse = "-"), 
            paste0(.GCM, ifelse(is.na(.SSP), "", paste0("_ssp", .SSP))), 
            paste0("rep", .rep))
}

# debug(SpaDES.project:::setupParams)
inSim <- SpaDES.project::setupProject(
  .rep = .rep,
  .ELFind = .ELFind,
  .strategy = .strategy,
  .cc = .cc,
  cores = .cores,
  FRU = FRU,
  .SSP = .SSP,
  .GCM = .GCM,
  .samplingRange = unlist(.samplingRange),
  defaultDots = list(.strategy = 1L, # used to be 3L; but seems to get caught in local minima
                     .cc = 0.5,
                     .objfunFireReps = 25L,
                     .rep = 1,
                     .ELFind = "4.3",
                     .SSP = 370,
                     .GCM = "CNRM-ESM2-1",
                     .samplingRange = 1990:2020, # vector
                     .cores = c("birds", "biomass", "camas", "carbon", "caribou", "coco"
                                , "core", "dougfir", "fire"
                                , "mpb", "sbw", "mega"
                                , "acer"
                                , "abies"
                                , "pinus"
                     ),
                     FRU = 25,
                     .times = list(start = 2020, end = 3020),
                     .modules = c("PredictiveEcology/canClimateData@improveCache1"
                                  ,"PredictiveEcology/climateYear@development"
                                  , "PredictiveEcology/fireSense_ELFs@main"
                                  , "PredictiveEcology/fireSense_dataPrepFit@development"
                                  , "PredictiveEcology/fireSense_IgnitionFit@development"
                                  , "PredictiveEcology/fireSense_SpreadFit@development"
                                  
                                  , "PredictiveEcology/fireSense_dataPrepPredict@development" # prepares data for predictions
                                  , "PredictiveEcology/fireSense_IgnitionPredict@development" # predicts ignitions & escapes
                                  , "PredictiveEcology/fireSense_SpreadPredict@development" # predicts raster of spreadProb
                                  , "PredictiveEcology/fireSense@development" # does burning
                                  # biomass modules
                                  , "PredictiveEcology/Biomass_borealDataPrep@development"
                                  , "PredictiveEcology/Biomass_speciesParameters@development"
                                  , "PredictiveEcology/Biomass_speciesData@development"
                                  , "PredictiveEcology/Biomass_regeneration@development"
                                  , "PredictiveEcology/Biomass_core@development"
                                  # summary modules 
                                  , "FOR-CAST/NRV_summary@development"
                                  , "PredictiveEcology/burnSummaries@development"
                                  , "PredictiveEcology/fireSense_summary@development"
                                  , "PredictiveEcology/Biomass_summary@main"
                     )),
  .objfunFireReps = .objfunFireReps,
  # useGit = "eliotmcintire",
  Restart = TRUE,
  overwrite = !SpaDES.project::machine("A159568") && SpaDES.project::user("emcintir"), # redownload any updates
  paths = list(outputPath = pathBuild(.ELFind, .samplingRange, .GCM, .SSP, .rep),
               cachePath = "/mnt/shared_cache/cache",
               # use inputPath on the shared drive, so inputPaths works
               inputPath = pathBuild(pre = "/mnt/shared_cache/inputs", .ELFind, .samplingRange, .GCM, .SSP, .rep)),
  runName = gsub("/", "_", fs::path_rel(paths$outputPath)) |>
    gsub(pattern = "outputs_", replacement = ""),
  times = as.list(unlist(.times, recursive = T)), # may be coming in as a slightly deeper list
  modules = unlist(.modules),
  packages = c(
    "PredictiveEcology/reproducible@recovery (>= 3.0.0.9010)"
    , "PredictiveEcology/SpaDES.core@updatesPostHDDFail (>= 3.0.4.9002)"
    , "PredictiveEcology/SpaDES.project@main (>= 1.0.1)"
    , "PredictiveEcology/clusters@main (>= 0.0.22)"
    , "PredictiveEcology/fireSenseUtils@development (>= 0.1.3)"
    , "PredictiveEcology/pemisc@development (>= 0.0.4.9016)" # needed for LandWebUtils; not sure why
    , "qs2", "filelock"
    , "archive"
    , "googlesheets4"
    , "PredictiveEcology/climateData@modsDuringFireSense3 (>= 2.2.2.9006)"
    , "terra" # "leaflet", "tidyterra",
    , "plyr"#, "scfmutils",
    , "geodata"
    , "rvest" # needed for prepIgnitionFitData
    # , "extraPackages.R" # file not used currently; should just skip it
  ),
  require = "reproducible",
  options = list(
    # gargle_oauth_email = "predictiveecology@gmail.com",
    # gargle_oauth_cache = ".secret",
    # gargle_oauth_client_type = "web", # for command line
    "~/googledriveAuthentication.R" # has the above lines in a list; each user can create their own file
    , repos = unique(c(repos[[1]]
                       # , 'https://dmlc.r-universe.dev'
                       , getOption("repos")))
    , reproducible.cacheSaveFormat = "qs2"
    , reproducible.qsFormat = "qs2"
    , reproducible.useTry = FALSE
    , SpaDES.project.fast = FALSE
    , reproducible.shapefileRead = "terra::vect"
    , reproducible.overwrite = TRUE
    , reproducible.inputPaths = "/mnt/shared_cache/data"
    , reproducible.cloudFolderID = "1oNGYVAV3goXfSzD1dziotKGCdO8P_iV9"
    , reproducible.showSimilarDepth = 8
    , reproducible.objSize = FALSE
    , reproducible.savePreDigest = FALSE
    , fireSenseUtils.runTests = FALSE
    , reproducible.memoisePersist = TRUE # sets the memoise location to .GlobalEnv; persists through a `load_all`
    , reproducible.nThreads = 1 #  When in parallel; can't do >1 ... only a warning
    , reproducible.prepInputsUrlTiles = "https://drive.google.com/drive/folders/1IfeQ9rZ3-RIQwtcdo2T5Kn51NJJRWeox?usp=drive_link"
    , spades.useRequire = TRUE
    # , error = recover
    
    # For batch runs, these should be off
    , reproducible.showSimilar = FALSE #interactive() && !nzchar(Sys.getenv("TMUX"))
    , reproducible.useMemoise = interactive() && !nzchar(Sys.getenv("TMUX"))
    , spades.recoveryMode = (interactive() && !nzchar(Sys.getenv("TMUX"))) + 0
    , spades.cacheChaining = FALSE
    , reproducible.cacheChaining = FALSE #interactive()
    
    , reproducible.gdalwarp = FALSE
    , Require.cloneFrom = Sys.getenv("R_LIBS_USER")
    , Require.usePak = TRUE
    , Require.verbose = 1
    , spades.moduleCodeChecks = FALSE
    , spades.allowInitDuringSimInit = TRUE
    , spades.evalPostEvent = # NULL
      quote({# print(.robustDigest(sim$spreadFirePolys));
        # print(.robustDigest(sim$rasterToMatch_biomassParam));
        print(.robustDigest(sim$sppEquiv))
      })
    , warnPartialMatchArgs = TRUE #fireSense has objects that will be fooled by partial matching (rstLCC, rstLCCs)
    , warnPartialMatchAttr = TRUE
    , warnPartialMatchDollar = TRUE
    , spades.debugModule = NULL),
  sideEffects = list(
    terra::terraOptions(memfrac = 0)
    , {gd <- file.path(paths$inputPath, "geodata"); geodata::geodata_path(gd)} # gadm on a non-interactive sessino needs this
    , terra::gdalCache(size = 2048)   # 2 GB
    , "OtherExtras.R" # Eliot has some dev things he does incl pkgload::
  ),
  .climVars = c("CMD_sm", "CMD_sp"),
  climateVariables = {
    climateData::climateLayers(.climVars, fun = quote(calcAsIs), 
                              projected = ifelse(identical(.GCM, "NRV"), FALSE, TRUE))
  },
  climateVariablesForFire = list(ignition = gsub("_", "", .climVars), # This must match a layer in climateVariables (without 'historical_')
                                 # only sm for spread
                                 spread = gsub("_", "", grep("sm$", .climVars, value = TRUE))), # This must match a layer in climateVariables (without 'historical_')
  saveAndPlotInterval = 100,
  params = list(
    .globals = list(
      spreadFitFilename = "fireSenseParams_2026_02.rds"
      # dataYear = 2011,
      , .studyAreaName = paste0("ELF", .ELFind)
      , .runName = runName
      , .plotInterval = saveAndPlotInterval
      , .plots = c("png")
      , sppEquivCol = "LandR" # will get a warning if this is not here
      , .useCache = c(".inputObjects", "init", "initPlot", "estimateThreshold", "spreadFitPrepare", "checkData")
      , minCoverThreshold = 0),
    climateYear = list(
      samplingEndYear = max(.samplingRange),
      samplingRange = range(.samplingRange),
      samplingStartYear = min(.samplingRange)
    ),
    # fireSense_ELFs = list(queue_path = "experiment_queue_predict5.rds"),
    canClimateData = list(
      climateGCM =  ifelse(identical(.GCM, "NRV"), "CNRM-ESM2-1", .GCM) 
      ,climateSSP =  ifelse(identical(.SSP, ""), 370, .SSP) 
      # ,.useCache = ".inputObjects" # init is slow to cache
    ),
    climateYear = list(
      samplingRange = .samplingRange,
      samplingStartYear = max(.samplingRange) +1
    ),
    # fireSense = list(.plots = c("screen", "png")),
    fireSense_SpreadFit = list(
      DEoptimTests = c("adTest", "SNLL_FS")
      , stopIfNoPreRunFit = SpaDES.project::user("emcintir") %in% FALSE
      # mutuallyExclusiveCols = list(
      #   youngAge = c("nf", unique(makeSppEquiv(ecoprovinceNum = ecoprovince)$fuel))
      # ),
      # .useCache = FALSE,
      , iterDEoptim = 1000
      , rep = .rep # This means that all Cache of DEoptim will now be different name
      , iterStep = 1 # run this many iterations before running again; this should be
      # set to itermax if Cache is not used; it is only useful for Cache
      , cores = cores
      , NP = {if (identical(cores, unique(cores))) 100 else length(cores)} # number of cores of machines
      , trace = 1
      , mode = "fit"# "visualize"),
      # mode = "debug",
      , strategy = .strategy
      , objfunFireReps = .objfunFireReps # this is the lowest that doesn't create an error
      , .c = .cc
      # SNLL_FS_thresh = snll_thresh,
      , doObjFunAssertions = FALSE
    ),
    fireSense_dataPrepFit = list(
      # missingLCCgroup = c("nf_dryland"), # must match fuel class land cover
      .useCache = c(".inputObjects",
                    # "init", # CAN'T cache this one because it is the trigger to "skip" a whole bunch if SpreadParams exist for the StudyArea
                    "dataPrepInit",
                    "prepEscapeFitData",
                    "prepSpreadFitData",
                    "prepIgnitionFitData",
                    "run")
    ),
    fireSense_IgnitionFit = list(
      rescalers = c("CMDsm" = 1000),
      .useCache = c(".inputObjects", "init", "prepIgnitionFitData", "run")
    ),
    burnSummaries = list(mode = "single", reps = .rep), #TODO confirm all params
    NRV_summary = list(mode = "single", reps = .rep), #TODO: confirm if all prams okay 
    fireSense_summary = list(mode = "single",
                             studyAreaName  = paste0("ELF", .ELFind), 
                             #reps = .rep,  
                             years = c(times$start, times$end)), 
    Biomass_summary = list(years = c(times$start, times$end), 
                           studyAreaName  = paste0("ELF", .ELFind),
                           mode = "single"
                           #reps = .rep #only needed for multi, and would be the total reps
                           )
  ), 
  # objectSynonyms = list(c("flammableRTM", "flammableMap")),
  outputs =  {
    outputs <- rbind(
      data.table(objectName = "pixelGroupMap", saveTime = c(seq(times$start, times$end, saveAndPlotInterval)), 
                 exts = ".tif", fun = "writeRaster", package = "terra"), 
      data.table(objectName = "cohortData", saveTime = c(seq(times$start, times$end, saveAndPlotInterval))), 
      data.table(objectName = "speciesEcoregion", saveTime = times$end), 
      data.table(objectName = "ecoregion", saveTime = times$end), 
      data.table(objectName = "species", saveTime = times$end),
      data.table(objectName = "ecoregionMap", saveTime = times$end, exts = ".tif", 
                 fun = "writeRaster", package = "terra"),
      data.table(objectName =  "standAgeMap", saveTime = times$end, 
                 exts = ".tif", fun = "writeRaster", package = "terra"),
      data.table(objectName =  "nonForest_timeSinceDisturbance", saveTime = times$end, 
                 exts = ".tif", fun = "writeRaster", package = "terra"),
      data.table(objectName =  "rstLCC", saveTime = times$end, 
                 exts = ".tif", fun = "writeRaster", package = "terra"),
      data.table(objectName = "climateYearRecord", saveTime = times$end),
      
      fill = TRUE
    )
    outputs[is.na(fun), c("exts", "fun", "package") := .("rds", "saveRDS", "base")]
    outputs <- as.data.frame(outputs)
    outputs$arguments <- list(overwrite = TRUE)
    return(outputs)
  }
)
message(paste0(inSim$runName, ", .strategy:", inSim$.strategy,
               " .objfunFireReps:", inSim$.objfunFireReps))

if (!is(inSim$climateVariables, "list")) browser()
inSimCopy <- reproducible::Copy(inSim)


if (FALSE) {
  prepInputs(targetFile = "fireSenseParams.rds", url = "https://drive.google.com/file/d/1-iD7Pj4cX3kag4TEHeGxGgW42Rf0ag2l/view?usp=drivesdk",
             destinationPath = "/home/emcintir/GitHub/FireSenseTesting/inputs",
             useCache = TRUE, purge = 7, overwrite = TRUE)
  SpaDES.core::Plots(inSim[grep("studyArea|rasterToMatch", names(inSim))],
                       title = paste0("StudyArea ", inSim$.runName),
                       fn = plotSAs,
                       filename = paste0("studyAreas", inSim$.runName),
                       path = inSim$paths$inputPath,
                       types = c("screen", "png")) |>
      reproducible::Cache(.functionName = "Plots_studyAreas",
                          useCache = !identical(names(dev.cur()), "null device"))

  SpaDES.project::plotSAsLeaflet(inSim[grep("studyArea|rasterToMatch", names(inSim))])

  fn <- "sim_FireSenseSpreadFit.qs2"
  saveState(filename = fn, files = FALSE)
  inSim2 <- SpaDES.core::loadSimList(fn)
  outSims <- restartSpades(inSim2)
  outSims <- restartSpades()
}

suppressPackageStartupMessages(
  simOut <- SpaDES.core::simInitAndSpades2(inSimCopy)
)



#TODO: projected and historical climate rasters- I assume we want to keep them


fsim <- SpaDES.core::simFile(
  name = inSim$runName, #TODO: was .runName, changing to theRunName, which has the SSP, GCM etc
  path = outputPath(simOut),   ## should be based on <run_name>
  time = inSim$times$end,
  ext = "rds"             ## do not use qs!
)
if (FALSE) { # if you don't have 
  inSim$runName = gsub("/", "_", fs::path_rel(inSim$paths$outputPath)) |>
    gsub(pattern = "outputs_", replacement = "")
  fsim <- SpaDES.core::simFile(
    name = inSim$runName, #TODO: was .runName, changing to theRunName, which has the SSP, GCM etc
    path = outputPath(simOut),   ## should be based on <run_name>
    time = inSim$times$end,
    ext = "rds"             ## do not use qs!
  )
}
dir.create(dirname(fsim), showWarnings = FALSE, recursive = TRUE)
system.time(saveSimList(
  sim = simOut,
  filename = fsim,
  ## avoid costly zip/unzip operations:
  inputs = FALSE,
  outputs = FALSE,
  cache = FALSE,
  files = FALSE))
#   
# #compress outputs, upload 
resultsDir <- outputPath(simOut)
#need a runName
tarball <- paste0(inSim$runName, ".tar.gz")

tar(tarball, files = outputs(simOut), extra_flags = "-v")
# tar(tarball, files = dir(resultsDir, full.names = TRUE), extra_flags = "-v")
# archive::archive_write_dir(archive = tarball, dir = resultsDir, format = "tar")
gFolder <- googledrive::as_id("https://drive.google.com/drive/folders/188ERmd1k6s6YMv3wHtnHQHD7pgLseBjf?usp=drive_link")
googledrive::drive_upload(tarball, path = gFolder,
                          name = tarball, overwrite = TRUE)