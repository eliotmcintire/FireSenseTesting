# install Require and SpaDES.project
repos <- c("https://predictiveecology.r-universe.dev", getOption("repos"))
source("https://raw.githubusercontent.com/PredictiveEcology/pemisc/refs/heads/development/R/getOrUpdatePkg.R")
getOrUpdatePkg(c("Require", "SpaDES.project"), c("1.0.1.9003", "0.1.1.9037")) # only install/update if required

# generic absolute path for anybody; but individual can change
projectDir <- "~/GitHub/FireSenseTesting/"
dir.create(projectDir, recursive = TRUE, showWarnings = FALSE)
setwd(projectDir)

# Require::Install("PredictiveEcology/SpaDES.project@development (>=0.1.1.9012)")
# pkgload::load_all("~/GitHub/SpaDES.project/");
pkgload::load_all("~/GitHub/SpaDES.project/");
inSim <- SpaDES.project::setupProject(
  ELFind = gsub("ELF", "", .ELFind),
  currentName = ELFind,
  .rep = .rep,
  .strategy = .strategy,
  .cc = .cc,
  FRU = FRU,
  defaultDots = list(.strategy = 3L,
                     .cc = 0.5,
                     .objfunFireReps = 25L,
                     .rep = 1,
                     .ELFind = "4.3",
                     .cores = c("birds", "biomass", "camas", "carbon", "caribou", "coco",
                                "core", "dougfir", "fire", "mpb", "sbw", "mega",
                                "acer", # "abies",
                                "pinus"),
                     FRU = 25),
  .objfunFireReps = .objfunFireReps,
  Restart = TRUE,
  modules = c("PredictiveEcology/canClimateData@improveCache1",

              "PredictiveEcology/fireSense_dataPrepFit@development",
              "PredictiveEcology/fireSense_IgnitionFit@development",
              "PredictiveEcology/fireSense_SpreadFit@development",

              "PredictiveEcology/fireSense_dataPrepPredict@development", # prepares data for predictions
              "PredictiveEcology/fireSense_IgnitionPredict@development", # predicts ignitions & escapes
              "PredictiveEcology/fireSense_SpreadPredict@development", # predicts raster of spreadProb

              "PredictiveEcology/fireSense@development", # does burning

              "PredictiveEcology/Biomass_borealDataPrep@development",
              "PredictiveEcology/Biomass_speciesData@development",
              "PredictiveEcology/Biomass_regeneration@development",
              "PredictiveEcology/Biomass_core@development"

  ),
  packages = c(# "PredictiveEcology/reproducible@AI (>= 2.1.2.9063)", # (HEAD)", # (HEAD)",
    # "PredictiveEcology/SpaDES.core@box (>= 2.1.8.9002)", # (HEAD)", # needed for the functions in
    # "PredictiveEcology/scfmutils@development", # (HEAD)",
    # "PredictiveEcology/fireSenseUtils@development (>= 0.0.6.9003)", #
    # "PredictiveEcology/SHAPforxgboost (>= 0.1.3.9001)",
    "terra", # "leaflet", "tidyterra",
    "rvest" # needed for prepIgnitionFitData
  ), # for StudyArea visualization below
  require = "reproducible",
  useGit = "eliotmcintire",
  times = list(start = 2011, end = 2081),
  options = list(# gargle_oauth_email = "predictiveecology@gmail.com",
                    # gargle_oauth_cache = ".secret",
                    # gargle_oauth_client_type = "web", # for command line
                    "~/googledriveAuthentication.R", # has the above lines; each user can create their own file
                    spades.allowInitDuringSimInit = FALSE, # TRUE
                    # reproducible.gdalwarp = TRUE,
                    repos = unique(c("predictiveecology.r-universe.dev", 'https://dmlc.r-universe.dev', getOption("repos"))),
                    spades.moduleCodeChecks = FALSE,
                    Require.cloneFrom = Sys.getenv("R_LIBS_USER"),
                    # 'reproducible.gdalwarp' = TRUE,
                    reproducible.cacheSaveFormat = "qs",
                    reproducible.useMemoise = TRUE,
                    spades.useRequire = FALSE,
                    spades.debug = list(file = list(file = file.path(paths$logPath,
                                                                     paste0("logfile_", ELFind, "_", gsub(":", "_", format(Sys.time())), ".txt")),
                                                    append = TRUE, level = 1)),
                    SpaDES.project.fast = FALSE,
                    reproducible.shapefileRead = "terra::vect",
                    spades.recoveryMode = 1,
                    warn = 0,
                    reproducible.useDBI = FALSE,
                    reproducible.overwrite = TRUE,
                    reproducible.inputPaths = "~/data",
                    # reproducible.useCache = "devMode",
                    reproducible.cloudFolderID = "1oNGYVAV3goXfSzD1dziotKGCdO8P_iV9",
                    reproducible.showSimilar = FALSE,
                    reproducible.showSimilarDepth = 8,
                    # Eliot during development
                    reproducible.savePreDigest = FALSE,
                    spades.debugModule = NA, #c("canClimateData", "fireSense_dataPrepPredict",
                    #                       "fireSense_dataPrepFit", "fireSense_SpreadFit",
                    #                      "Biomass_speciesData", "Biomass_borealDataPrep"), # "fireSense_dataPrepFit", # NA
                    fireSenseUtils.runTests = FALSE,
                    reproducible.memoisePersist = TRUE # sets the memoise location to .GlobalEnv; persists through a `load_all`


  ),
  homogeneousFire = {
    scfmutils::prepInputsFireRegimePolys(type = "FRU", destinationPath = paths$inputPath) |>
      reproducible::Cache(cacheSaveFormat = "rds")
      # reproducible::Cache()
  },
  ELFs = {
    fireSenseUtils::makeELFs(homogeneousFire, desiredBuffer = 20000, destinationPath = paths$inputPath) |>
    Cache(omitArgs = "nationalForestPolygon",
          .cacheExtra = list(hf = attr(homogeneousFire, "tags"), bufferOutFn = fireSenseUtils:::bufferOut))
  }
  ,
  d1 = 5000,
  rastTemplate = {
    terra::ext(homogeneousFire) |>
      terra::rast(res = 250, crs = homogeneousFire, vals = 1)} |>
    Cache(omitArgs = c("x"), .cacheExtra = attr(homogeneousFire, "tags")),
  rasterToMatchLarge = {
    rtml <- ELFs$rasWhole[[ELFind]]
    rtml[rtml == 0] <- NA
    {
      postProcess(rtml, projectTo = rastTemplate, method = "near") |>
        terra::trim()  # |>
        # terra::focal(w = 15, "modal", na.policy = "only") # fill in NAs
    } |> Cache(omitArgs = c("x"), .cacheExtra = list(ELFs = attr(ELFs, "tags"),
                                                     ELFind = ELFind,
                                                     rastTemplate = attr(rastTemplate, "tags")))
  },
  studyAreaLarge = {
    {
      terra::as.polygons(rasterToMatchLarge > 0) # |>
      #  terra::buffer(width = d1) |>
      #  terra::buffer(width = -d1)
    } |> Cache(omitArgs = c("x"), .cacheExtra = list(rtml = attr(rasterToMatchLarge, "tags")))
  },
  rasterToMatch = {
    {
      rasterToMatchLarge |>
        replace(list = rasterToMatchLarge != 2, NA) |>
        terra::trim()
    } |> Cache(omitArgs = c("x"), .cacheExtra = list(rtml = attr(rasterToMatchLarge, "tags")))
  },
  studyArea = {
      terra::as.polygons(rasterToMatch) |>
      #  terra::buffer(width = d1) |>
      #  terra::buffer(width = -d1)
     Cache(omitArgs = c("x"), .cacheExtra = list(rtm = attr(rasterToMatch, "tags")))
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
      reproducible::Cache(omitArgs = "projectTo", .cacheExtra = list(sa = attr(studyArea, "tags")))
    b <- reproducible::postProcess(a, studyArea = studyAreaLarge) |>
      reproducible::Cache(omitArgs = c("x", "studyArea"), .cacheExtra = list(sa = attr(studyAreaLarge, "tags"),
                                                                             sa = attr(a, "tags")))
    ecoprovinces <- unique(b$ECOPROVINC)
    a[a$ECOPROVINC %in% ecoprovinces] # |> terra::aggregate()
  },
  cores = .cores,
  .climVars = c("CMD_sm", "CMD_sp"),
  climateVariables = {
   climateLayers(.climVars, fun = quote(calcAsIs))
  },
  climateVariablesForFire = list(ignition = gsub("_", "", .climVars), # This must match a layer in climateVariables (without 'historical_')
                                 # only sm for spread
                                 spread = gsub("_", "", grep("sm$", .climVars, value = TRUE))), # This must match a layer in climateVariables (without 'historical_')
  params = list(
    .globals = list(
      # dataYear = 2011,
      .plots = "png",
      sppEquivCol = "LandR", # will get a warning if this is not here
      .useCache = c(".inputObjects", "init"),
      minCoverThreshold = 0),
    fireSense_SpreadFit = list(
      DEoptimTests = c("adTest", "SNLL_FS"),
      # mutuallyExclusiveCols = list(
      #   youngAge = c("nf", unique(makeSppEquiv(ecoprovinceNum = ecoprovince)$fuel))
      # ),
      .useCache = FALSE,
      iterDEoptim = 1000,
      rep = .rep, # This means that all Cache of DEoptim will now be different name
      iterStep = 1, # run this many iterations before running again; this should be
      # set to itermax if Cache is not used; it is only useful for Cache
      cores = cores,
      NP = {if (identical(cores, unique(cores))) 100 else length(cores)}, # number of cores of machines
      trace = 1,
      mode = c("fit"),# "visualize"),
      strategy = .strategy,
      objfunFireReps = .objfunFireReps, # this is the lowest that doesn't create an error
      .c = .cc,
      # SNLL_FS_thresh = snll_thresh,
      doObjFunAssertions = FALSE
    ),
    fireSense_dataPrepFit = list(
      # missingLCCgroup = c("nf_dryland"), # must match fuel class land cover
      .useCache = c(".inputObjects", "init", "prepSpreadFitData", "prepIgnitionFitData", "run")
    ),
    fireSense_IgnitionFit = list(
      rescalers = c("CMDsm" = 1000),
      .useCache = c(".inputObjects", "init", "prepIgnitionFitData")
    )
  )
)


library(SpaDES.project)
SpaDES.core::Plots(inSim[grep("studyArea|rasterToMatch", names(inSim))],
                   title = paste0("StudyArea ", inSim$currentName),
                   fn = plotSAs,
                   filename = paste0("studyAreas", inSim$currentName),
                   path = inSim$paths$inputPath,
                   types = c("screen", "png")) |> reproducible::Cache()

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



if (TRUE) {
  pkgload::load_all("~/GitHub/reproducible/");
  pkgload::load_all("~/GitHub/SpaDES.core/");
  # pkgload::load_all("~/GitHub/SpaDES.project/");
  # pkgload::load_all("~/GitHub/clusters/");
  pkgload::load_all("~/GitHub/LandR/");
  pkgload::load_all("~/GitHub/climateData/");
  #  pkgload::load_all("~/GitHub/scfmutils/");
  pkgload::load_all("~/GitHub/fireSenseUtils/");
}
# Require::Install("pkgload")
# pkgload::load_all("~/GitHub/clusters/");

message(paste0(inSim$currentName, ", .rep:", inSim$.rep, ", .strategy:", inSim$.strategy,
             " .objfunFireReps:", inSim$.objfunFireReps))
# debug(SpaDES.core::loadSimList)
st <- Sys.time()
options(
  #  rstLCC in 2nd time is "8882282dd8bcd415"
  spades.evalPostEvent = NULL#""#quote(print(.robustDigest(sim$rstLCC)))
    # quote({
    #   print(sim$standAgeMap); print(.robustDigest(sim$standAgeMap));
    #   print(sim$rstLCC); print(.robustDigest(sim$rstLCC));
    #   print(sim$studyArea); .robustDigest(sim$studyArea)
    # })
  , spades.debugModule = NULL#"fireSense_dataPrepFit"
  , reproducible.useMemoise = TRUE
)
fn <- paste0("simPreDispersalFit", inSim$currentName, ".qs")
if (TRUE) {
  if (TRUE) {
    # debug(SpaDES.core::restartOrSimInitAndSpades)
    # sim <- SpaDES.core::restartOrSimInitAndSpades(inSimCopy, fn) |> suppressPackageStartupMessages()
    simOut <- try(SpaDES.core::restartSpades(verbose = FALSE), silent = TRUE) |> suppressPackageStartupMessages()
    if (is(simOut, "try-error")) {
      message("There was no sim to restartSpades with... running simInitAndSpades")
      simOut <- SpaDES.core::simInitAndSpades2(inSimCopy) |> suppressPackageStartupMessages()
    }

    saveState(filename = fn, files = FALSE)
    # options(reproducible.showSimilar = FALSE)
  } else {
    simOut <- SpaDES.core::simInitAndSpades2(inSimCopy)
    saveState(filename = fn, files = FALSE)
  }
  #
  # file.copy(fn, )
}
# If it makes it here, then remove it so that the next iteration doesn't pick this up
rm(list = ".sim", envir= SpaDES.core:::savedSimEnv())
