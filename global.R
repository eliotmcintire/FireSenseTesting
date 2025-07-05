repos <- c("https://predictiveecology.r-universe.dev", getOption("repos"))
source("https://raw.githubusercontent.com/PredictiveEcology/pemisc/refs/heads/development/R/getOrUpdatePkg.R")
getOrUpdatePkg(c("Require", "SpaDES.project"), c("1.0.1.9003", "0.1.1.9037")) # only install/update if required
# Require::Install("PredictiveEcology/SpaDES.project@development (>=0.1.1.9012)")
pkgload::load_all("~/GitHub/SpaDES.project/");

bufferIn <- 1000

# This is running in tmux
FRU <- 26; .rep <- 3; fnForClusters = tail # On Erni et al FRU map rep 2 = 27 is running -- rep 3 = 26,

#if (!Require:::isRstudio())
FRU <- 27; .rep <- 2; fnForClusters = head # On Erni et al FRU map rep 2 = 27 is running -- rep 3 = 26,
#if (Require:::isRstudio())
FRU <- 27; .rep <- 2; fnForClusters = head; bufferIn <- -1000 # On Erni et al FRU map rep 2 = 27 is running -- rep 3 = 26,
FRU <- "ELF4.3"; .rep <- 1; fnForClusters = head; bufferIn <- -1000 # On Erni et al FRU map rep 2 = 27 is running -- rep 3 = 26,
.ELFind <- "ELF4.3"; .rep <- 1; fnForClusters = head; bufferIn <- -1000 # On Erni et al FRU map rep 2 = 27 is running -- rep 3 = 26,
.ELFind <- "ELF4.3"; .rep <- 1; fnForClusters = head; bufferIn <- -1000 # On Erni et al FRU map rep 2 = 27 is running -- rep 3 = 26,

# FRU <- 27; .rep <- 4; fnForClusters = head; bufferIn <- 1000 # On Erni et al FRU map rep 2 = 27 is running -- rep 3 = 26,
currentName <- paste0("FRU-", FRU)#, paste0("_minus", abs(bufferIn)))
currentName <- FRU#, paste0("_minus", abs(bufferIn)))

setwd("~/GitHub/FireSenseTesting/") # generic absolute path for anybody; but individual can change
# pkgload::load_all("~/GitHub/SpaDES.project/");
# reproducible::clearCache(Function = "speciesInStudyArea", ask = FALSE)
# try(unlink(dir(getOption('reproducible.inputPaths'), pattern = "SpeciesPresent", full.names = T)))
# unlink(dir("inputs", pattern = "SpeciesPresent", full.names = T))
if (FALSE) {
  pkgload::load_all("~/GitHub/SpaDES.project/");
  pkgload::load_all("~/GitHub/reproducible/");
  pkgload::load_all("~/GitHub/SpaDES.core/");
  pkgload::load_all("~/GitHub/clusters/");
  pkgload::load_all("~/GitHub/LandR/");
  pkgload::load_all("~/GitHub/scfmutils/");
  pkgload::load_all("~/GitHub/fireSenseUtils/");
  devtools::install("~/GitHub/reproducible/", upgrade = FALSE);
  devtools::install("~/GitHub/SpaDES.core/", upgrade = FALSE);
  devtools::install("~/GitHub/SpaDES.project/", upgrade = FALSE);
  devtools::install("~/GitHub/clusters/", upgrade = FALSE);
  devtools::install("~/GitHub/LandR/", upgrade = FALSE);
  devtools::install("~/GitHub/scfmutils/", upgrade = FALSE);
  devtools::install("~/GitHub/fireSenseUtils/", upgrade = FALSE);
}

climateLayers <- function(.climVars = "CMD_sm", historical = TRUE, projected = TRUE,
                          fun = quote(calcAsIs)) {
  hps <- c()
  if (isTRUE(historical))
    hps <- c(historical = "historical")
  if (isTRUE(projected))
    hps <- c(hps, future = "projected")

  rr <- Map(hp = unname(hps), nam = names(hps), function(hp, nam) {
    Map(cv = .climVars, function(cv) {
      ll <- list(vars = paste0(nam, "_", cv),
                 fun = fun)
      .dots = if (nam == "historical")
        list(1991:2022)
      else
        list(2011:2100)
      ll <- append(ll, list(.dots = .dots |> setNames(paste0(nam, "_years"))))
    })
  }) |> unlist(recursive = FALSE)

  names(rr) <- gsub("_", "", names(rr)) # removes _ in e.g., CMD_sm
  names(rr) <- gsub("\\.", "_", names(rr)) # converts remaining . to a _ in e.g., projected_

  rr
}

# These must be specified in the ~/.ssh/config file; and all ssh keys must be in place
FORSITEmachines <- c("birds", "biomass", "camas", "carbon", "caribou", "coco",
                     "core", "dougfir", "fire", "mega", "mpb", "sbw")
FORSITEmachinesAvailable <- setdiff(FORSITEmachines, "sbw")
if (FALSE) {

  tmpl <- terra::rast(dv, res = 5000)
  FRUras <- {tmpl |>
      terra::rasterize(dv, y = _, field = "FRU")} |> reproducible::Cache(omitArgs = "x")
  fre <- freq(FRUras)
  fre$count <- fre$count * prod(terra::res(FRUras))

  biggestWeWant <- 2.37725e+11 # FRU 27 ... or FRU 26 is 4.02075e+11

  boreal <- prepInputs(url = "https://d278fo2rk9arr5.cloudfront.net/downloads/boreal.zip",
                       destinationPath = inSim$paths$inputPath) |> reproducible::Cache()
  tmplboreal <- terra::rast(boreal, res = 5000)
  borealras <- {tmplboreal |>
      terra::rasterize(boreal, y = _, field = "TYPE")} |> reproducible::Cache(omitArgs = "x")
  borealras <- postProcess(borealras, to = FRUras) |> Cache()

  needToModel <- c(1, 3:11, 13:18, 21:35, 37:38, 40:43, 45:47, 49:50, 52:53, 55:59)
}

# Ecozone/province/region/district
inSim <- SpaDES.project::setupProject(
  defaultDots = list(.strategy = 3L,
                     .cc = 0.5,
                     .objfunFireReps = 25L,
                     .rep = 1,
                     .ELFind = "4.3",
                     # .cores = if (Require:::isRstudio()) NA else
                     .cores = # c(rep("localhost", 30), rep(c("permafrost", "caribou", "fire", "birds", "scfm"), 14)),
                     # c(rep(c("carbon", "biomass", "sbw", "birds", "firesense", "scfm"), 14), rep(c("caribou", "fire"), 7)),
                     c(rep(c("carbon", "biomass", "sbw", "birds"), 22), rep(c("caribou", "fire"), 8)),
                     sort(rep(fnForClusters(FORSITEmachines, 5), length.out = 100)),
                     FRU = 25),
  ELFind = gsub("ELF", "", .ELFind),
  .rep = .rep,
  .strategy = .strategy,
  .cc = .cc,
  FRU = FRU,
  .objfunFireReps = .objfunFireReps,
  Restart = TRUE,
  functions = "~/GitHub/FireSenseTesting/R/functions.R",
  # useGit= "eliotmcintire",
  paths = list(projectPath = "~/GitHub/FireSenseTesting",
               outputPath = file.path("outputs", currentName),
               cachePath = "cache",
               inputPath = "inputs",
               scratchPath = "scratch"),
  modules = c("PredictiveEcology/canClimateData@improveCache1",

              "PredictiveEcology/fireSense_dataPrepFit@development",
              "PredictiveEcology/fireSense_IgnitionFit@development",
              "PredictiveEcology/fireSense_SpreadFit@development",

              "PredictiveEcology/fireSense_dataPrepPredict@development", # prepares data for predictions
              "PredictiveEcology/fireSense_IgnitionPredict@development", # predicts ignitions & escapes
              "PredictiveEcology/fireSense_SpreadPredict@development", # predicts raster of spreadProb

              "PredictiveEcology/fireSense@development", # does burning

              "PredictiveEcology/Biomass_borealDataPrep@development",
              "PredictiveEcology/Biomass_speciesData@development"
              ),
  packages = c(# "PredictiveEcology/reproducible@AI", # (HEAD)", # (HEAD)",
               "PredictiveEcology/SpaDES.core@box", # (HEAD)", # needed for the functions in
               "PredictiveEcology/scfmutils@development", # (HEAD)",
               "terra", "leaflet", "rvest", "tidyterra"), # for StudyArea visualization below
  require = "PredictiveEcology/reproducible@AI",
  # loadOrder = c("canClimateData", "fireSense_dataPrepFit", "fireSense_SpreadFit",
  #               "Biomass_speciesData", "Biomass_borealDataPrep",
  #               "fireSense_dataPrepPredict", "fireSense_SpreadPredict"),
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
                    SpaDES.project.fast = FALSE,
                    reproducible.shapefileRead = "terra::vect",
                    spades.recoveryMode = 1,
                    warn = 0,
                    reproducible.useDBI = FALSE,
                    reproducible.overwrite = TRUE,
                    reproducible.inputPaths = "~/data",
                    # reproducible.useCache = "devMode",
                    reproducible.cloudFolderID = "1oNGYVAV3goXfSzD1dziotKGCdO8P_iV9",
                    reproducible.showSimilar = TRUE,
                    reproducible.showSimilarDepth = 8,
                    # Eliot during development
                    reproducible.savePreDigest = FALSE,
                    spades.debugModule = NA, #c("canClimateData", "fireSense_dataPrepPredict",
                    #                       "fireSense_dataPrepFit", "fireSense_SpreadFit",
                    #                      "Biomass_speciesData", "Biomass_borealDataPrep"), # "fireSense_dataPrepFit", # NA
                    fireSenseUtils.runTests = FALSE,
                    reproducible.memoisePersist = TRUE # sets the memoise location to .GlobalEnv; persists through a `load_all`


  ),
  times = list(start = 2011, end = 2021),
  homogeneousFire = {
    scfmutils::prepInputsFireRegimePolys(type = "FRU", destinationPath = paths$inputPath) |>
      reproducible::Cache(cacheSaveFormat = "rds")
      # reproducible::Cache()
  },
  ELFs =
    makeELFs(homogeneousFire, desiredBuffer = 20000, inputPath = paths$inputPath) |>
    Cache(omitArgs = "nationalForestPolygon",
          .cacheExtra = list(hf = attr(homogeneousFire, "tags"), bufferOutFn = bufferOut))
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
    } |> Cache(omitArgs = c("x"), .cacheExtra = list(ELFs = attr(ELFs, "tags"), rastTemplate = attr(rastTemplate, "tage")))
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
  # studyAreaInit = {
  #   homogeneousFire[homogeneousFire$FRU == FRU, 1] |>
  #     sf::st_buffer(dist = bufferIn)
  # },
  # # objectSynonyms = list(c("rstLCC", "rstLCC2011"),
  # #                       c("nonForest_timeSinceDisturbance", "nonForest_timeSinceDisturbance2011"),
  # #                       c("landcoverDT", "landcoverDT2011"),
  # #                       c("standAgeMap", "standAgeMap2011"),
  # #                       c("flammableRTM", "flammableRTM2011")),
  # rasterToMatch = {reproducible::prepInputs(destinationPath = paths$inputPath,
  #                                           url = paste0("https://ftp.maps.canada.ca/pub/nrcan_rncan/Forests_Foret/",
  #                                                        "canada-forests-attributes_attributs-forests-canada/",
  #                                                        "2001-attributes_attributs-2001/",
  #                                                        "NFI_MODIS250m_2001_kNN_Structure_Stand_Age_v1.tif"),
  #                                           cropTo = studyAreaInit, maskTo = studyAreaInit,
  #                                           writeTo = NULL,
  #                                           method = c("near"), fun = "terra::rast") |> reproducible::Cache()},
  # studyArea = reproducible::postProcessTo(studyAreaInit, projectTo = rasterToMatch) |> reproducible::Cache(),
  # studyAreaLarge = sf::st_buffer(studyArea, dist = 20000) |> reproducible::Cache(),
  # rasterToMatchLarge = reproducible::prepInputs(destinationPath = paths$inputPath,
  #                                               url = paste0("https://ftp.maps.canada.ca/pub/nrcan_rncan/Forests_Foret/",
  #                                                            "canada-forests-attributes_attributs-forests-canada/",
  #                                                            "2001-attributes_attributs-2001/",
  #                                                            "NFI_MODIS250m_2001_kNN_Structure_Stand_Age_v1.tif"),
  #                                               cropTo = studyAreaLarge, maskTo = studyAreaLarge,
  #                                               writeTo = NULL,
  #                                               method = c("near"), fun = "terra::rast") |> reproducible::Cache(),
  studyAreaReporting = studyArea,
  sppEquiv = {
    species <- LandR::speciesInStudyArea(studyArea, dPath = paths$inputPath) |>
      reproducible::Cache(omitArgs = "studyArea", .cacheExtra = list(sa = attr(studyArea, "tags")))
    spp <- grep("_Spp", species$speciesList, invert = TRUE, value = TRUE)
    column <- LandR::equivalentNameColumn(spp, LandR::sppEquivalencies_CA)
    LandR::sppEquivalencies_CA[which(LandR::sppEquivalencies_CA[[column]] %in% spp),]
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
  #nonForestedLCCGroups = list( # these are codes on LCC -- currently NTEMS
  #  "nf_dryland" = c(50, 100, 40), # shrub, herbaceous, bryoid
  #  "nf_wetland" = c(80)), #non-treed wetland.

  cores = if (Require:::isRstudio()) NULL else .cores,
  # c(rep("bc213", 13), rep("localhost", 13), rep("n105", 13), rep("n14", 13),
  #   rep("bc184", 14), rep("bc217", 12), rep("n68", 20)),
  #params last because one of them depends on sppEquiv fuel class names
  .climVars = c("CMD_sm", "CMD_sp"),
  climateVariables = {
   climateLayers(.climVars, fun = quote(calcAsIs))
  },
  # climateVariables = list(
  #   historical_CMDsm = list(
  #     vars = "historical_CMD_sm",
  #     fun = quote(calcAsIs),
  #     .dots = list(historical_years = 1991:2022)
  #   ),
  #   projected_CMDsm = list(
  #     vars = "future_CMD_sm",
  #     fun = quote(calcAsIs),
  #     .dots = list(future_years = 2011:2100)
  #   )
  # ),
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
    # Biomass_borealDataPrep = list(
    #   overrideAgeInFires = FALSE,
    #   overrideBiomassInFires = FALSE
    # ),
    # canClimateData = list(
    #   .useCloud = FALSE
    # ),
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
      # mode = c("debug"),
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
                   title = paste0("StudyArea ", currentName),
                   fn = plotSAs, filename = paste0("studyAreas", currentName),
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



if (FALSE) {
  pkgload::load_all("~/GitHub/reproducible/");
  pkgload::load_all("~/GitHub/SpaDES.core/");
  pkgload::load_all("~/GitHub/SpaDES.project/");
#  pkgload::load_all("~/GitHub/clusters/");
  pkgload::load_all("~/GitHub/LandR/");
#  pkgload::load_all("~/GitHub/scfmutils/");
  pkgload::load_all("~/GitHub/fireSenseUtils/");
}

# Start quick
pkgload::load_all("~/GitHub/reproducible/");
pkgload::load_all("~/GitHub/SpaDES.core/");
pkgload::load_all("~/GitHub/clusters/");
pkgload::load_all("~/GitHub/fireSenseUtils/");
message(paste0(inSim$FRU, ", .rep:", inSim$.rep, ", .strategy:", inSim$.strategy,
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
fn <- "simPreDispersalFit.qs"
if (TRUE) {
  sim <- SpaDES.core::restartOrSimInitAndSpades(inSimCopy, fn) |> suppressPackageStartupMessages()
  saveState(filename = fn, files = FALSE)
  # options(reproducible.showSimilar = FALSE)
} else {
  outSims <- SpaDES.core::simInitAndSpades2(inSimCopy)
  saveState(filename = fn, files = FALSE)
}
