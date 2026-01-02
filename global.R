# install Require and SpaDES.project
repos <- c("https://predictiveecology.r-universe.dev", getOption("repos"))
source("https://raw.githubusercontent.com/PredictiveEcology/pemisc/refs/heads/development/R/getOrUpdatePkg.R")
getOrUpdatePkg(c("Require", "SpaDES.project"), c("1.0.1.9021", "0.1.1.9053")) # only install/update if required

# generic absolute path for anybody; but individual can change
projectDir <- "~/GitHub/FireSenseTesting/"
dir.create(projectDir, recursive = TRUE, showWarnings = FALSE)
setwd(projectDir)

# pkgload::load_all("~/GitHub/SpaDES.project/");
# debug(experiment3)
# pkgload::load_all("~/GitHub/fireSenseUtils/");
# undebug(makeELFs)
# devtools::install("~/GitHub/SpaDES.project/", upgrade = FALSE);
# debug(setupProject)
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
                                "core", "dougfir", "fire", "mpb", "sbw", "mega",
                                # "acer", 
                                "abies"#, "pinus"
                     ),
                     FRU = 25),
  .objfunFireReps = .objfunFireReps,
  # useGit = "eliotmcintire",
  Restart = TRUE,
  paths = list(outputPath = file.path("outputs", ELFind)),
  modules = c("PredictiveEcology/canClimateData@improveCache1"

              , "PredictiveEcology/fireSense_dataPrepFit@development"
              # , "PredictiveEcology/fireSense_IgnitionFit@development"
              , "PredictiveEcology/fireSense_SpreadFit@development"

              #, "PredictiveEcology/fireSense_dataPrepPredict@development" # prepares data for predictions
              #, "PredictiveEcology/fireSense_IgnitionPredict@development" # predicts ignitions & escapes
              #, "PredictiveEcology/fireSense_SpreadPredict@development" # predicts raster of spreadProb

              #, "PredictiveEcology/fireSense@development", # does burning

              , "PredictiveEcology/Biomass_borealDataPrep@development"
              , "PredictiveEcology/Biomass_speciesParameters@development"
              , "PredictiveEcology/Biomass_speciesData@development"
              #, "PredictiveEcology/Biomass_regeneration@development",
              #, "PredictiveEcology/Biomass_core@development"

  ),
  packages = c("reproducible (>= 2.1.2.999991)" # (HEAD)", # (HEAD)",
               ,"PredictiveEcology/climateData@modsDuringFireSense3 (HEAD)"
               ,"SpaDES.core (>= 2.1.8.9999)" # (HEAD)", # needed for the functions in
               ,"PredictiveEcology/scfmutils@development" # (HEAD)",
               # "PredictiveEcology/fireSenseUtils@development (>= 0.0.6.9003)", #
               # "PredictiveEcology/SHAPforxgboost (>= 0.1.3.9001)",
               , "terra" # "leaflet", "tidyterra",
               , "plyr"#, "scfmutils",
               , "rvest" # needed for prepIgnitionFitData
  ),
  require = "reproducible",
  times = list(start = 2020, end = 2020),
  options = list(# gargle_oauth_email = "predictiveecology@gmail.com",
    # gargle_oauth_cache = ".secret",
    # gargle_oauth_client_type = "web", # for command line
    "~/googledriveAuthentication.R" # has the above lines; each user can create their own file
    , repos = unique(c(repos[[1]], 'https://dmlc.r-universe.dev', getOption("repos")))
    , reproducible.cacheSaveFormat = "qs"
    , reproducible.qsFormat = "qs"
    , SpaDES.project.fast = FALSE
    , reproducible.shapefileRead = "terra::vect"
    , reproducible.overwrite = TRUE
    , reproducible.inputPaths = "~/data"
    , reproducible.cloudFolderID = "1oNGYVAV3goXfSzD1dziotKGCdO8P_iV9"
    , reproducible.showSimilarDepth = 8
    # Eliot during development
    , reproducible.savePreDigest = FALSE
    # , spades.debugModule = NA #c("canClimateData", "fireSense_dataPrepPredict",
    #                       "fireSense_dataPrepFit", "fireSense_SpreadFit",
    #                      "Biomass_speciesData", "Biomass_borealDataPrep"), # "fireSense_dataPrepFit", # NA
    , fireSenseUtils.runTests = FALSE
    , reproducible.memoisePersist = TRUE # sets the memoise location to .GlobalEnv; persists through a `load_all`
    # , repos = unique(c('https://dmlc.r-universe.dev', getOption("repos"))) # needed for xgboost
    , reproducible.inputPaths = "~/data" # means I can share data from other projects
    , reproducible.prepInputsUrlTiles = "https://drive.google.com/drive/folders/1IfeQ9rZ3-RIQwtcdo2T5Kn51NJJRWeox?usp=drive_link"
    , spades.useRequire = F

    # For batch runs, these should be off
    , reproducible.showSimilar = interactive() && !nzchar(Sys.getenv("TMUX"))
    , reproducible.useMemoise = interactive() && !nzchar(Sys.getenv("TMUX"))
    , spades.recoveryMode = (interactive() && !nzchar(Sys.getenv("TMUX"))) + 0
    , spades.cacheChaining = TRUE
    , reproducible.cacheChaining = FALSE #interactive()

    , reproducible.gdalwarp = FALSE
    , Require.cloneFrom = Sys.getenv("R_LIBS_USER")
    , spades.moduleCodeChecks = FALSE
    , spades.allowInitDuringSimInit = TRUE),
  sideEffects = list(
    terra::terraOptions(memfrac = 0)
    , pkgload::load_all("~/GitHub/reproducible/")
    , pkgload::load_all("~/GitHub/SpaDES.core/")
    , pkgload::load_all("~/GitHub/clusters/")
    , pkgload::load_all("~/GitHub/LandR/")
    , pkgload::load_all("~/GitHub/fireSenseUtils/")
    , pkgload::load_all("~/GitHub/climateData/")
    # , bbbb <<- 1 # on.exit(rm(bbbb, envir = .GlobalEnv))
  ),
  rastTemplate = {
    # check the hash once per week
    templateURL <- "https://ftp.maps.canada.ca/pub/nrcan_rncan/Forests_Foret/SCANFI/v1/SCANFI_sps_douglasFir_SW_2020_v1.2.tif"
    hash <- reproducible:::getRemoteMetadata(isGDurl = FALSE, url = templateURL) |>
      Cache(notOlderThan = Sys.time() - 60*60*24*7)
    out <- {
      prepInputs(url = templateURL, destinationPath = paths$inputPath) |>
        (\(x) {message("Aggregating to 240m..."); x})() |>
        terra::aggregate(fact = 8, filename = file.path(paths$inputPath, "rastTemplate_Canada.tif"),
                         overwrite = TRUE)} |>
      Cache(omitArgs = "x", .cacheExtra = list(hash$remoteHash),
            # dryRun = TRUE,
            .functionName = "rasterTemplate_aggregate")
    out
  },
  homogeneousFire = {
    # bbbb <<- 1#; on.exit(rm(bbbb, envir = .GlobalEnv))
    {
      scfmutils::prepInputsFireRegimePolys(type = "FRU", destinationPath = paths$inputPath) |>
        reproducible::Cache(cacheSaveFormat = "rds")
    }},
  ELFs = {
    # fireSenseUtils::makeELFs(homogeneousFire, desiredBuffer = 20000, destinationPath = paths$inputPath) |>
    fireSenseUtils::makeELFs(rastTemplate, desiredBuffer = 20000, destinationPath = paths$inputPath) |>
      Cache(omitArgs = "nationalForestPolygon",
            .cacheExtra = list(rt = attr(rastTemplate, "tags"),
                               bufferOutFn = fireSenseUtils:::bufferOut))
  },
  d1 = 5000,
  # rastTemplate2 = { # This is HUGE 2+GB
  #   {
  #     terra::vect(homogeneousFire) |> terra::ext() |> round() |>
  #       terra::rast(res = 240, crs = homogeneousFire, vals = 1) } |>
  #     Cache(omitArgs = c("x"), .functionName = "rasterTemplate",
  #           .cacheExtra = attr(homogeneousFire, "tags"))
  # },
  rasterToMatchLarge = {
    rtml <- ELFs$rasWhole[[ELFind]]
    if (identical(1, terra::freq(is.na(rtml))$value))
      stop("This ELF has no data")
    rtml[rtml[] == 0] <- NA
    {
      postProcess(rtml, projectTo = rastTemplate, method = "near",
                  writeTo = file.path(paths$inputPath, paste0("rtml_", ELFind,".tif"))) |>
        terra::trim() } |>
      Cache(omitArgs = c("x"),
            .functionName = paste0("rasterToMatchLarge"),
            .cacheExtra = list(ELFs = attr(ELFs, "tags"),
                               ELFind = ELFind,
                               rastTemplate = attr(rastTemplate, "tags")))
  },
  rastTemplate = { # This is HUGE 2+GB
    { postProcess(rastTemplate, to = rasterToMatchLarge,
                  writeTo = file.path(paths$inputPath, paste0("rasterTemplate_", ELFind,".tif")))} |>
      Cache(omitArgs = c("x"), .cacheExtra = attr(rastTemplate, "tags"))
  },
  studyAreaLarge = {
    {
      terra::as.polygons(rasterToMatchLarge > 0) # |>
      #  terra::buffer(width = d1) |>
      #  terra::buffer(width = -d1)
    } |> Cache(omitArgs = c("x"), .functionName = "studyAreaLarge",
               .cacheExtra = list(rtml = attr(rasterToMatchLarge, "tags")))
  },
  rasterToMatch = {
    {
      rasterToMatchLarge |>
        replace(list = rasterToMatchLarge != 2, NA) |>
        terra::trim()
    } |> Cache(omitArgs = c("x"),
               .functionName = "rasterToMatch",
               .cacheExtra = list(rtml = attr(rasterToMatchLarge, "tags")))
  },
  studyArea = {
    terra::as.polygons(rasterToMatch) |>
      #  terra::buffer(width = d1) |>
      #  terra::buffer(width = -d1)
      Cache(omitArgs = c("x"), .functionName = "studyArea",
            .cacheExtra = list(rtm = attr(rasterToMatch, "tags")))
  },
  studyAreaReporting = studyArea,
  sppEquiv = {
    species <- LandR::speciesInStudyArea(studyArea, dPath = paths$inputPath) |>
      reproducible::Cache(omitArgs = "studyArea", .cacheExtra = list(sa = attr(studyArea, "tags")))
    spp <- grep("_Spp", species$speciesList, invert = TRUE, value = TRUE)
    column <- LandR::equivalentNameColumn(spp, LandR::sppEquivalencies_CA)
    sppEquiv <- LandR::sppEquivalencies_CA[which(LandR::sppEquivalencies_CA[[column]] %in% spp),]
    sppEquiv[LANDIS_traits != "",]
  },
  studyAreaPSP = {
    a <- reproducible::prepInputs(url = paste0("https://sis.agr.gc.ca/cansis/nsdb/ecostrat/",
                                               "province/ecoprovince_shp.zip"), dPath = paths$inputPath,
                                  fun = "terra::vect", projectTo = studyArea) |>
      reproducible::Cache(.functionName = "prepInputs_ecoprovince",
                          omitArgs = "projectTo", .cacheExtra = list(sa = attr(studyArea, "tags")))
    b <- reproducible::postProcess(a, studyArea = studyAreaLarge) |>
      reproducible::Cache(omitArgs = c("x", "studyArea"), .cacheExtra = list(sa = attr(studyAreaLarge, "tags"),
                                                                             sa = attr(a, "tags")))
    ecoprovinces <- unique(b$ECOPROVINC)
    a[a$ECOPROVINC %in% ecoprovinces] # |> terra::aggregate()
  },
  .climVars = c("CMD_sm", "CMD_sp"),
  climateVariables = {
    climateData::climateLayers(.climVars, fun = quote(quote(calcAsIs)))
  },
  climateVariablesForFire = list(ignition = gsub("_", "", .climVars), # This must match a layer in climateVariables (without 'historical_')
                                 # only sm for spread
                                 spread = gsub("_", "", grep("sm$", .climVars, value = TRUE))), # This must match a layer in climateVariables (without 'historical_')
  DEMLatLong = prepInputs(url = "https://drive.google.com/file/d/14puAtns8oTZDtvWzpQ6_FgK4MbozGZFK/",
                          maskTo = studyAreaLarge, cropTo = studyAreaLarge,
                          writeTo = file.path(paths$inputPath, paste0("DEM_", ELFind,".tif"))) |>
    Cache(omitArgs = c("maskTo", "cropTo"), .functionName = "prepInputs_DEMLatLong",
          .cacheExtra = attr(studyAreaLarge, "tags"))
  ,
  params = list(
    .globals = list(
      # dataYear = 2011,
      .studyAreaName = .runName,
      .plots = "png",
      sppEquivCol = "LandR", # will get a warning if this is not here
      .useCache = c(".inputObjects", "init", "initPlot", "estimateThreshold"),#, "spreadFitPrepare"),
      minCoverThreshold = 0),
    # canClimateData = list(.useCache = ".inputObjects"),  # init is slow to cache
    fireSense = list(.plots = c("screen", "png")),
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
      mode = c("fit"),# "visualize"),
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
                    # "init", # CAN'T cache this one because it is the trigger to "skip" a whole bunch if SpreadParams exist for the StudyArea
                    "dataPrepInit",
                    "prepEscapeFitData",
                    "prepSpreadFitData",
                    "prepIgnitionFitData",
                    "run")
    ),
    fireSense_IgnitionFit = list(
      rescalers = c("CMDsm" = 1000),
      .useCache = c(".inputObjects", "init", "prepIgnitionFitData")
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
message("a")
# a <- fireSenseUtils::fireSenseCloudParametersMap()
inSim$climateVariables <- climateData::climateLayers(inSim$.climVars, fun = quote(calcAsIs))
message("b")

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
message("c")

#known bugs/undesirable behavior
#1 spreadFit dumps a bunch of figs in the project directory instead of outputs

if (FALSE) {

  SpaDES.project::plotSAsLeaflet(inSim[grep("studyArea|rasterToMatch", names(inSim))])

  fn <- "sim_FireSenseSpreadFit.qs"
  saveState(filename = fn, files = FALSE)
  inSim2 <- SpaDES.core::loadSimList(fn)
  outSims <- restartSpades(inSim2)
  outSims <- restartSpades()
}
inSimCopy <- reproducible::Copy(inSim)
message("d")

# pkgload::load_all("~/GitHub/climateData/");

if (FALSE) {
  if (quickPlot::isRstudioServer()) {
    pkgload::load_all("~/GitHub/reproducible/");
    pkgload::load_all("~/GitHub/SpaDES.core/");
  }
  # pkgload::load_all("~/GitHub/SpaDES.project/");
  # pkgload::load_all("~/GitHub/clusters/");
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
# Require::Install("pkgload")
# pkgload::load_all("~/GitHub/clusters/");
# debug(SpaDES.core::loadSimList)
# options(spades.cacheChaining = TRUE)
# debug(prepSpeciesTable)# ; undebug(cacheChainingPost)
if (TRUE) {
  st <- Sys.time()
  message("e")

  options(
    #  rstLCC in 2nd time is "8882282dd8bcd415"
    spades.evalPostEvent = NULL
    # quote({# print(.robustDigest(sim$spreadFirePolys));
    #   print(params(sim)$fireSense_SpreadFit$mode);
    #   # print(.robustDigest(sim[["standAgeMap"]]))
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
    , spades.debugModule = NULL#"fireSense_dataPrepFit"
    #, reproducible.useMemoise = FALSE
  )
  startedFile <- file.path("logs", paste0("Running_", .ELFind, "_", Sys.getpid(), "_.rds"))
  saveRDS(.ELFind, file = startedFile)
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
