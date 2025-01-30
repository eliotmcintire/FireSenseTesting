repos <- c("https://predictiveecology.r-universe.dev", getOption("repos"))
source("https://raw.githubusercontent.com/PredictiveEcology/pemisc/refs/heads/development/R/getOrUpdatePkg.R")
getOrUpdatePkg(c("Require", "SpaDES.project"), c("1.0.1.9003", "0.1.1.9012")) # only install/update if required
# Require::Install("PredictiveEcology/SpaDES.project@development (>=0.1.1.9012)")

FRU <- 27 # On Erni et al FRU map
bufferIn <- -135000
currentName <- paste0("FRU-", FRU, paste0("_minus", abs(bufferIn)))

setwd("~/GitHub/FireSenseTesting/") # generic absolute path for anybody; but individual can change
inSim <- SpaDES.project::setupProject(
  Restart = TRUE,
  # useGit= "eliotmcintire",
  paths = list(projectPath = "~/GitHub/FireSenseTesting",
               outputPath = file.path("outputs", currentName)),
  modules = c("PredictiveEcology/fireSense_dataPrepFit@lccFix",
              "PredictiveEcology/Biomass_borealDataPrep@development",
              "PredictiveEcology/Biomass_speciesData@development",
              "PredictiveEcology/fireSense_SpreadFit@lccFix",
              "PredictiveEcology/fireSense_IgnitionFit@biomassFuel",
              "PredictiveEcology/canClimateData@improveCache1"),
  packages = c("PredictiveEcology/reproducible@AI",
               "PredictiveEcology/SpaDES.core@box", # needed for the functions in
               "PredictiveEcology/scfmutils@development",
               "terra", "leaflet", "tidyterra"), # for StudyArea visualization below
  require = "reproducible",
  options = options(gargle_oauth_email = "predictiveecology@gmail.com",
                    gargle_oauth_cache = ".secret",
                    gargle_oauth_client_type = "web", # for command line
                    spades.allowInitDuringSimInit = TRUE,
                    # reproducible.gdalwarp = TRUE,
                    repos = unique(c("predictiveecology.r-universe.dev", getOption("repos"))),
                    spades.moduleCodeChecks = FALSE,
                    Require.cloneFrom = Sys.getenv("R_LIBS_USER"),
                    # 'reproducible.gdalwarp' = TRUE,
                    reproducible.cacheSaveFormat = "qs",
                    reproducible.useMemoise = TRUE,
                    SpaDES.project.fast = FALSE,
                    reproducible.shapefileRead = "terra::vect",
                    spades.recoveryMode = 1,
                    spades.useBox = FALSE,
                    reproducible.useDBI = FALSE,
                    reproducible.overwrite = TRUE,
                    reproducible.inputPaths = "~/data",
                    # reproducible.useCache = "devMode",
                    reproducible.cloudFolderID = "1oNGYVAV3goXfSzD1dziotKGCdO8P_iV9",
                    reproducible.showSimilar = TRUE,
                    reproducible.showSimilarDepth = 8,
                    # Eliot during development
                    fireSenseUtils.runTests = FALSE,
                    reproducible.memoisePersist = TRUE # sets the memoise location to .GlobalEnv; persists through a `load_all`


  ),
  times = list(start = 2011, end = 2021),
  studyAreaInit = {
    d <- scfmutils::prepInputsFireRegimePolys(type = "FRU", destinationPath = paths$inputPath) |>
      Cache()
    d[d$FRU == FRU,1] |> sf::st_buffer(dist = bufferIn)
  },
  rasterToMatch = {reproducible::prepInputs(destinationPath = paths$inputPath,
                                            url = paste0("https://ftp.maps.canada.ca/pub/nrcan_rncan/Forests_Foret/",
                                                         "canada-forests-attributes_attributs-forests-canada/",
                                                         "2001-attributes_attributs-2001/",
                                                         "NFI_MODIS250m_2001_kNN_Structure_Stand_Age_v1.tif"),
                                            cropTo = studyAreaInit, maskTo = studyAreaInit,
                                            writeTo = NULL,
                                            method = c("near"), fun = "terra::rast") |> Cache()},
  studyArea = postProcessTo(studyAreaInit, projectTo = rasterToMatch),
  studyAreaLarge = sf::st_buffer(studyArea, dist = 20000) |> Cache(),
  rasterToMatchLarge = reproducible::prepInputs(destinationPath = paths$inputPath,
                                                url = paste0("https://ftp.maps.canada.ca/pub/nrcan_rncan/Forests_Foret/",
                                                             "canada-forests-attributes_attributs-forests-canada/",
                                                             "2001-attributes_attributs-2001/",
                                                             "NFI_MODIS250m_2001_kNN_Structure_Stand_Age_v1.tif"),
                                                cropTo = studyAreaLarge, maskTo = studyAreaLarge,
                                                writeTo = NULL,
                                                method = c("near"), fun = "terra::rast") |> Cache(),
  studyAreaReporting = studyArea,
  sppEquiv = {
    species <- LandR::speciesInStudyArea(studyArea) |> Cache()
    spp <- grep("_Spp", species$speciesList, invert = TRUE, value = TRUE)
    column <- LandR::equivalentNameColumn(spp, LandR::sppEquivalencies_CA)
    LandR::sppEquivalencies_CA[which(LandR::sppEquivalencies_CA[[column]] %in% spp),]
  },
  studyAreaPSP = {
    a <- reproducible::prepInputs(url = paste0("https://sis.agr.gc.ca/cansis/nsdb/ecostrat/",
                                               "province/ecoprovince_shp.zip"), dPath = paths$inputPath,
                                  fun = "terra::vect", projectTo = studyArea) |> Cache()
    b <- postProcess(a, studyArea = studyAreaLarge) |> Cache()
    ecoprovinces <- unique(b$ECOPROVINC)
    a[a$ECOPROVINC %in% ecoprovinces] # |> terra::aggregate()
  },
  #nonForestedLCCGroups = list( # these are codes on LCC -- currently NTEMS
  #  "nf_dryland" = c(50, 100, 40), # shrub, herbaceous, bryoid
  #  "nf_wetland" = c(80)), #non-treed wetland.
  cores =
    c(rep("localhost", 70), rep("n54", 10), rep("n14", 10), rep("n105", 10)),
  # c(rep("bc213", 13), rep("localhost", 13), rep("n105", 13), rep("n14", 13),
  #   rep("bc184", 14), rep("bc217", 12), rep("n68", 20)),
  #params last because one of them depends on sppEquiv fuel class names
  climateVariables = list(
    historical_CMDsm = list(
      vars = "historical_CMD_sm",
      fun = quote(calcAsIs),
      .dots = list(historical_years = 1991:2022)
    )
  ),
  climateVariablesForFire = list(ignition = "CMDsm", # This must match a layer in climateVariables (without 'historical_')
                                 spread = "CMDsm"), # This must match a layer in climateVariables (without 'historical_')
  params = list(
    .globals = list(
      # dataYear = 2011,
      .plots = "png",
      sppEquivCol = "LandR", # will get a warning if this is not here
      .useCache = c(".inputObjects", "init")),
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
      iterDEoptim = 10000,
      rep = 5, # This means that all Cache of DEoptim will now be different name
      iterStep = 1, # run this many iterations before running again; this should be
      # set to itermax if Cache is not used; it is only useful for Cache
      cores = cores, # NA, #NULL, # cores,
      NP = {if (identical(cores, unique(cores))) 100 else length(cores)}, # number of cores of machines
      trace = 1,
      mode = c("fit", "visualize"),
      strategy = 3L,
      objfunFireReps = 25L,
      # mode = c("debug"),
      # SNLL_FS_thresh = snll_thresh,
      doObjFunAssertions = FALSE
    ),
    fireSense_dataPrepFit = list(
      # missingLCCgroup = c("nf_dryland"), # must match fuel class land cover
      .useCache = c(".inputObjects", "init", "prepSpreadFitData", "prepIgnitionFitData")
    ),
    fireSense_IgnitionFit = list(
      rescalers = c("CMDsm" = 1000),
      .useCache = c(".inputObjects", "init", "prepIgnitionFitData")
    )
  )
)
packageVersion("SpaDES.project")
library(SpaDES.project)
SpaDES.core::Plots(inSim[grep("studyArea|rasterToMatch", names(inSim))],
                   title = paste0("StudyArea ", currentName),
                   fn = plotSAs, filename = paste0("studyAreas", currentName),
                   path = inSim$paths$inputPath,
                   types = c("screen", "png")) |> Cache()


#known bugs/undesirable behavior
#1 spreadFit dumps a bunch of figs in the project directory instead of outputs

# devtools::install("~/GitHub/reproducible/", upgrade = FALSE); devtools::install("~/GitHub/SpaDES.core/", upgrade = FALSE);
# devtools::install("~/GitHub/climateData/", upgrade = FALSE);
# devtools::install("~/GitHub/fireSenseUtils/", upgrade = FALSE);
# devtools::install("~/GitHub/clusters/", upgrade = FALSE);
#

if (FALSE) {
  fn <- "sim_FireSenseSpreadFit.qs"
  # saveState(filename = fn, files = FALSE)
  inSim2 <- SpaDES.core::loadSimList(fn)
  outSims <- restartSpades(inSim2)
  outSims <- restartSpades()
}

if (TRUE) {
  pkgload::load_all("~/GitHub/reproducible/");
  pkgload::load_all("~/GitHub/SpaDES.core/");
  # pkgload::load_all("~/GitHub/SpaDES.project/");
  pkgload::load_all("~/GitHub/fireSenseUtils/");
  pkgload::load_all("~/GitHub/LandR/");
}
outSims <- do.call(what = SpaDES.core::simInitAndSpades, args = inSim, quote = TRUE)
# restartSpades()
#  fn <- "sim_FireSenseSpreadFit.qs"
# outSims <- restartSpades(fn)
