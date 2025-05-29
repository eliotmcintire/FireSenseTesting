repos <- c("https://predictiveecology.r-universe.dev", getOption("repos"))
source("https://raw.githubusercontent.com/PredictiveEcology/pemisc/refs/heads/development/R/getOrUpdatePkg.R")
getOrUpdatePkg(c("Require", "SpaDES.project"), c("1.0.1.9003", "0.1.1.9037")) # only install/update if required
# Require::Install("PredictiveEcology/SpaDES.project@development (>=0.1.1.9012)")

bufferIn <- 1000

# This is running in tmux
FRU <- 26; .rep <- 3; fnForClusters = tail # On Erni et al FRU map rep 2 = 27 is running -- rep 3 = 26,

#if (!Require:::isRstudio())
  FRU <- 27; .rep <- 2; fnForClusters = head # On Erni et al FRU map rep 2 = 27 is running -- rep 3 = 26,
#if (Require:::isRstudio())
  FRU <- 27; .rep <- 2; fnForClusters = head; bufferIn <- -1000 # On Erni et al FRU map rep 2 = 27 is running -- rep 3 = 26,

# FRU <- 27; .rep <- 4; fnForClusters = head; bufferIn <- 1000 # On Erni et al FRU map rep 2 = 27 is running -- rep 3 = 26,
currentName <- paste0("FRU-", FRU)#, paste0("_minus", abs(bufferIn)))

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

# These must be specified in the ~/.ssh/config file; and all ssh keys must be in place
FORSITEmachines <- c("birds", "biomass", "camas", "carbon", "caribou", "coco",
                     "core", "dougfir", "fire", "mega", "mpb", "sbw")
FORSITEmachinesAvailable <- setdiff(FORSITEmachines, "sbw")
inSim <- SpaDES.project::setupProject(
  defaultDots = list(.strategy = 3L,
                     .cc = 0.5,
                     .objfunFireReps = 25L,
                     .rep = 1,
                     .cores = if (Require:::isRstudio()) NA else
                       # .cores = c(rep("localhost", 30), rep(c("permafrost", "caribou", "fire", "birds", "scfm"), 14)),
                       # c(rep(c("carbon", "biomass", "sbw", "birds", "firesense", "scfm"), 14), rep(c("permafrost", "caribou", "fire"), 6)),
                       sort(rep(fnForClusters(FORSITEmachines, 5), length.out = 100)),
                     FRU = 25),
  .rep = .rep,
  .strategy = .strategy,
  .cc = .cc,
  FRU = FRU,
  .objfunFireReps = .objfunFireReps,
  .cores = .cores,
  Restart = TRUE,
  # useGit= "eliotmcintire",
  paths = list(projectPath = "~/GitHub/FireSenseTesting",
               outputPath = file.path("outputs", currentName),
               cachePath = "cache",
               inputPath = "inputs",
               scratchPath = "scratch"),
  modules = c("PredictiveEcology/fireSense_dataPrepFit@development",
              "PredictiveEcology/Biomass_borealDataPrep@development",
              "PredictiveEcology/Biomass_speciesData@development",
              "PredictiveEcology/fireSense_SpreadFit@development",
              "PredictiveEcology/fireSense_dataPrepPredict@development",
              "PredictiveEcology/fireSense_SpreadPredict@development",
              # "PredictiveEcology/fireSense_IgnitionFit@biomassFuel",
              "PredictiveEcology/canClimateData@improveCache1"),
  packages = c(# "PredictiveEcology/reproducible@AI", # (HEAD)", # (HEAD)",
               "PredictiveEcology/SpaDES.core@box", # (HEAD)", # needed for the functions in
               "PredictiveEcology/scfmutils@development", # (HEAD)",
               "terra", "leaflet", "rvest", "tidyterra"), # for StudyArea visualization below
  require = "PredictiveEcology/reproducible@AI",
  # loadOrder = c("canClimateData", "fireSense_dataPrepFit", "fireSense_SpreadFit",
  #               "Biomass_speciesData", "Biomass_borealDataPrep",
  #               "fireSense_dataPrepPredict", "fireSense_SpreadPredict"),
  options = options(gargle_oauth_email = "predictiveecology@gmail.com",
                    gargle_oauth_cache = ".secret",
                    gargle_oauth_client_type = "web", # for command line
                    spades.allowInitDuringSimInit = FALSE, # TRUE
                    # reproducible.gdalwarp = TRUE,
                    repos = unique(c("predictiveecology.r-universe.dev", getOption("repos"))),
                    spades.moduleCodeChecks = FALSE,
                    Require.cloneFrom = Sys.getenv("R_LIBS_USER"),
                    # 'reproducible.gdalwarp' = TRUE,
                    reproducible.cacheSaveFormat = "qs",
                    reproducible.useMemoise = TRUE,
                    spades.useRequire = FALSE,
                    SpaDES.project.fast = FALSE,
                    reproducible.shapefileRead = "terra::vect",
                    spades.recoveryMode = 1,
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
      reproducible::Cache()
  },
  studyAreaInit = {
    homogeneousFire[homogeneousFire$FRU == FRU, 1] |>
      sf::st_buffer(dist = bufferIn)
  },
  # objectSynonyms = list(c("rstLCC", "rstLCC2011"),
  #                       c("nonForest_timeSinceDisturbance", "nonForest_timeSinceDisturbance2011"),
  #                       c("landcoverDT", "landcoverDT2011"),
  #                       c("standAgeMap", "standAgeMap2011"),
  #                       c("flammableRTM", "flammableRTM2011")),
  rasterToMatch = {reproducible::prepInputs(destinationPath = paths$inputPath,
                                            url = paste0("https://ftp.maps.canada.ca/pub/nrcan_rncan/Forests_Foret/",
                                                         "canada-forests-attributes_attributs-forests-canada/",
                                                         "2001-attributes_attributs-2001/",
                                                         "NFI_MODIS250m_2001_kNN_Structure_Stand_Age_v1.tif"),
                                            cropTo = studyAreaInit, maskTo = studyAreaInit,
                                            writeTo = NULL,
                                            method = c("near"), fun = "terra::rast") |> reproducible::Cache()},
  studyArea = reproducible::postProcessTo(studyAreaInit, projectTo = rasterToMatch),
  studyAreaLarge = sf::st_buffer(studyArea, dist = 20000) |> reproducible::Cache(),
  rasterToMatchLarge = reproducible::prepInputs(destinationPath = paths$inputPath,
                                                url = paste0("https://ftp.maps.canada.ca/pub/nrcan_rncan/Forests_Foret/",
                                                             "canada-forests-attributes_attributs-forests-canada/",
                                                             "2001-attributes_attributs-2001/",
                                                             "NFI_MODIS250m_2001_kNN_Structure_Stand_Age_v1.tif"),
                                                cropTo = studyAreaLarge, maskTo = studyAreaLarge,
                                                writeTo = NULL,
                                                method = c("near"), fun = "terra::rast") |> reproducible::Cache(),
  studyAreaReporting = studyArea,
  sppEquiv = {
    species <- LandR::speciesInStudyArea(studyArea, dPath = paths$inputPath) |> reproducible::Cache()
    spp <- grep("_Spp", species$speciesList, invert = TRUE, value = TRUE)
    column <- LandR::equivalentNameColumn(spp, LandR::sppEquivalencies_CA)
    LandR::sppEquivalencies_CA[which(LandR::sppEquivalencies_CA[[column]] %in% spp),]
  },
  studyAreaPSP = {
    a <- reproducible::prepInputs(url = paste0("https://sis.agr.gc.ca/cansis/nsdb/ecostrat/",
                                               "province/ecoprovince_shp.zip"), dPath = paths$inputPath,
                                  fun = "terra::vect", projectTo = studyArea) |> reproducible::Cache()
    b <- reproducible::postProcess(a, studyArea = studyAreaLarge) |> reproducible::Cache()
    ecoprovinces <- unique(b$ECOPROVINC)
    a[a$ECOPROVINC %in% ecoprovinces] # |> terra::aggregate()
  },
  #nonForestedLCCGroups = list( # these are codes on LCC -- currently NTEMS
  #  "nf_dryland" = c(50, 100, 40), # shrub, herbaceous, bryoid
  #  "nf_wetland" = c(80)), #non-treed wetland.
  cores = .cores,
  # c(rep("bc213", 13), rep("localhost", 13), rep("n105", 13), rep("n14", 13),
  #   rep("bc184", 14), rep("bc217", 12), rep("n68", 20)),
  #params last because one of them depends on sppEquiv fuel class names
  climateVariables = list(
    historical_CMDsm = list(
      vars = "historical_CMD_sm",
      fun = quote(calcAsIs),
      .dots = list(historical_years = 1991:2022)
    ),
    projected_CMDsm = list(
      vars = "future_CMD_sm",
      fun = quote(calcAsIs),
      .dots = list(future_years = 2011:2100)
    )
  ),
  climateVariablesForFire = list(ignition = "CMDsm", # This must match a layer in climateVariables (without 'historical_')
                                 spread = "CMDsm"), # This must match a layer in climateVariables (without 'historical_')
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
      .useCache = c(".inputObjects", "init", "prepSpreadFitData", "prepIgnitionFitData")
    ),
    fireSense_IgnitionFit = list(
      rescalers = c("CMDsm" = 1000),
      .useCache = c(".inputObjects", "init", "prepIgnitionFitData")
    )
  )
)

if (FALSE) { # other maps
  library(terra)
  library(reproducible)
  library(data.table)
  source("~/GitHub/FireSenseTesting/R/functions.R")
  ria <- reproducible::prepInputs(url = "https://drive.google.com/file/d/1h7gK44g64dwcoqhij24F2K54hs5e35Ci/view?usp=drive_link") |>
    reproducible::Cache()
  dv <- terra::vect(inSim$homogeneousFire) |> reproducible::Cache()
  boreal <- prepInputs(url = "https://d278fo2rk9arr5.cloudfront.net/downloads/boreal.zip",
                       destinationPath = inSim$paths$inputPath) |> reproducible::Cache()
  tmplboreal <- terra::rast(boreal, res = 5000)
  borealras <- {tmplboreal |>
      terra::rasterize(boreal, y = _, field = "TYPE")} |> reproducible::Cache(omitArgs = "x")

  tmpl <- terra::rast(dv, res = 5000)
  FRUras <- {tmpl |>
      terra::rasterize(dv, y = _, field = "FRU")} |> reproducible::Cache(omitArgs = "x")
  fre <- freq(FRUras)
  fre$count <- fre$count * prod(res(FRUras))

  biggestWeWant <- 2.37725e+11 # FRU 27 ... or FRU 26 is 4.02075e+11

  borealras <- postProcess(borealras, to = FRUras) |> Cache()

  needToModel <- c(1, 3:11, 13:18, 21:35, 37:38, 40:43, 45:47, 49:50, 52:53, 55:59)

  # Ecozone/province/region/district
  ecoNames <- c("zone", "region", "province", "district")
  makeEcoURLs <- function(ecoNames) {
    vapply(ecoNames, FUN.VALUE = character(1), function(en)
      paste0(paste0("https://sis.agr.gc.ca/cansis/nsdb/ecostrat/", en, "/eco", en, "_shp.zip")))
  }
  urls <- makeEcoURLs(ecoNames)
  ecosLCC <- Map(nam = ecoNames, url = urls, function(nam, url) {
    reproducible::prepInputs(url = url,
                             destinationPath = inSim$paths$inputPath,
                             projectTo = inSim$homogeneousFire) |> Cache()
  })
  tmpl <- terra::rast(ecosLCC[[1]], res = 5000)
  hf <- rasterize(inSim$homogeneousFire, tmpl, field = "FRU") |> Cache()
  ecosR <- Map(ecoLCC = ecosLCC, nam = names(ecosLCC), function(ecoLCC, nam)
    {
    rasterize(ecoLCC, tmpl, field = toupper(paste0("eco", substr(nam, 1, 7)))) |>
      postProcess(maskTo = hf)
    } |> Cache())

  ecopR <- ecosR$province
  # Some are on the edge, e.g., in the tundra --> remove if less than 100 pixels
  fre <- freq(ecopR)
  fre <- fre[fre$count > 100, ]
  ecopRseg <- terra::segregate(ecopR)
  categories <- terra::cats(ecopR)[[1]]
  names(ecopRseg) <- categories[match(names(ecopRseg), categories$ID), "ECOPROVINC"]
  ord <- order(as.numeric(intersect(categories$ECOPROVINC, fre$value)))
  ecopRseg <- ecopRseg[[names(ecopRseg) %in% fre$value]][[ord]]

  plotStack(ecopRseg)
  out <- bufferOut(dv)
  out2 <- mergeAndSplitRas(ecopRseg, ecosLCC$province)

  out3 <- lapply(out2, function(x) as.list(segregateKeepNames(x, omitClasses = 0))) |>
    unlist(recursive = FALSE)
  names(out3) <- sapply(out3, names)


  # Something failing here
  out4 <- bufferOut(spatRasSeg = out3, mask = out$rasWhole[[1]])

  out5 <- rast(out4$rasWhole)
  plotStack(out4$rasWhole)

  fre <- freq(out5) |> as.data.table()
  dt <- data.table(nam = names(out5), num = seq_len(nlyr(out5)))
  fre[, layer2 := dt$nam[match(fre$layer, dt$num)]]
  fre2 <- fre[value %in% 1:2][, .(area = sum(count)), by = "layer2"]

  fre <- Map(x = out4$rasCentered, function(x) freq(x) |> as.data.table())
  fre <- rbindlist(fre, idcol = "layer2")
  fre3 <- fre[value %in% 1:2][, .(areaCentred = sum(count)), by = "layer2"]
  stretchEffect <- fre3[fre2, on = "layer2"]
  stretchEffect[, diffAreaInHa := (areaCentred - area ) * prod(res(out5)) / 1e4][]

  # SpaDES.project::plotSAsLeaflet(list(ecoz = ecoz, ecop = ecop, ecor = ecor, ecod = ecod, FRU = inSim$homogeneousFire))
  riaBin <- !is.na(ria)
  riaShp <- as.polygons(riaBin, aggregate = TRUE, round= TRUE, values = FALSE)
  # riaBin2 <- focal(riaBin, fun = function(x) sum(x) > 7)
  terra::plot(riaBin)

}


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
  # saveState(filename = fn, files = FALSE)
  inSim2 <- SpaDES.core::loadSimList(fn)
  outSims <- restartSpades(inSim2)
  outSims <- restartSpades()
}
inSimCopy <- reproducible::Copy(inSim)


if (TRUE) {
  pkgload::load_all("~/GitHub/reproducible/");
  pkgload::load_all("~/GitHub/SpaDES.core/");
#  pkgload::load_all("~/GitHub/SpaDES.project/");
#  pkgload::load_all("~/GitHub/clusters/");
  pkgload::load_all("~/GitHub/LandR/");
#  pkgload::load_all("~/GitHub/scfmutils/");
  pkgload::load_all("~/GitHub/fireSenseUtils/");
}
message(paste0("FRU", inSim$FRU, ", .rep:", inSim$.rep, ", .strategy:", inSim$.strategy,
             " .objfunFireReps:", inSim$.objfunFireReps))

st <- Sys.time()
# debug(recoverModePre)
options(
  spades.debugPrint = NULL
    # quote({
    #   print(sim$standAgeMap); print(.robustDigest(sim$standAgeMap));
    #   print(sim$rstLCC); print(.robustDigest(sim$rstLCC));
    #   print(sim$studyArea); .robustDigest(sim$studyArea)
    # })
  , spades.debugModule = "" # "fireSense_dataPrepFit"
  , reproducible.useMemoise = FALSE
)
fn <- "simPreDispersalFit.qs"
if (F) {
  sim <- SpaDES.core::restartOrSimInitAndSpades(inSimCopy, fn)
  # options(reproducible.showSimilar = FALSE)
} else {
  outSims <- SpaDES.core::simInitAndSpades2(inSimCopy)
}
