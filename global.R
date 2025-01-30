# Install or update SpaDES.project & Require
source("https://raw.githubusercontent.com/PredictiveEcology/pemisc/refs/heads/development/R/getOrUpdatePkg.R")
getOrUpdatePkg(c("Require", "SpaDES.project"), c("1.0.1.9003", "0.1.1.9008")) # only install/update if required

FRU <- 27
currentName <- paste0("FRU-", FRU) #toggle between Skeena and Taiga
# if (currentName == "Taiga") {
#   ecoprovince <- c("4.3")
#   studyAreaPSPprov <- c("4.3", "12.3", "14.1", "9.1") #this is a weird combination
#   snll_thresh = 3100 # 4822 after running the estimator
# } else {
#   ecoprovince <- "14.1"
#   studyAreaPSPprov <- c("14.1", "14.2", "14.3", "14.4")
#   snll_thresh = 1200
# }

# if (!Sys.info()[["nodename"]] == "W-VIC-A127551") {
#   #this must be run in advance at some point -
#   # I don't know how to control the token expiry - gargle documentation is crappy
#   # mytoken <- gargle::gargle2.0_token(email = "ianmseddy@gmail.com")
#   # saveRDS(mytoken, "googlemagic.rds")
#   googledrive::drive_auth(email = "ianmseddy@gmail.com",
#                           token = readRDS("googlemagic.rds"))
# }

if (FALSE) {
  pkgload::load_all("~/GitHub/reproducible/");
  pkgload::load_all("~/GitHub/SpaDES.core/");
  # pkgload::load_all("~/GitHub/SpaDES.project/");
  pkgload::load_all("~/GitHub/fireSenseUtils/");
  pkgload::load_all("~/GitHub/LandR/");
}

setwd("~")
#TODO change the script so that ecoprovinceNum is consistently named in functinos
inSim <- SpaDES.project::setupProject(
  Restart = TRUE,
  # useGit= "eliotmcintire",
  paths = list(projectPath = "GitHub/FireSenseTesting",
               outputPath = file.path("outputs", currentName)
  ),
  modules = c("PredictiveEcology/fireSense_dataPrepFit@lccFix",
              "PredictiveEcology/Biomass_borealDataPrep@development",
              "PredictiveEcology/Biomass_speciesData@development",
              "PredictiveEcology/fireSense_SpreadFit@lccFix",
              "PredictiveEcology/fireSense_IgnitionFit@biomassFuel",
              "PredictiveEcology/canClimateData@improveCache1"
  ),
  packages = c("PredictiveEcology/reproducible@AI",
               "PredictiveEcology/SpaDES.core@box", # needed for the functions in
               "PredictiveEcology/scfmutils@development",
               "terra", "leaflet",
               "tidyterra"), # for FireRegime polygons
  # overwrite = TRUE,
  require = c("reproducible"), # for Cache used in pipe below
  options = options(gargle_oauth_email = "predictiveecology@gmail.com",
                    gargle_oauth_cache = ".secret",
                    gargle_oauth_client_type = "web", # for command line
                    spades.allowInitDuringSimInit = TRUE,
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
  functions = "ianmseddy/NEBC@main/R/studyAreaFuns.R",
  climateVariablesForFire = list(ignition = "CMDsm",
                                 spread = "CMDsm"),
  # sppEquiv = {
  #   makeSppEquiv(ecoprovinceNum = ecoprovince) |> Cache()
  # },
  #update mutuallyExlcusive Cols
  # setupSAandRTMfun = setupSAandRTM,
  studyAreaInit = {
    d <- scfmutils::prepInputsFireRegimePolys(type = "FRU", destinationPath = paths$inputPath) |>
      Cache()
    d[d$FRU == FRU,1]
    # b <- scfmutils::prepInputsFireRegimePolys(type = "FRT", destinationPath = paths$inputPath) |>
    #   Cache()
    # b[b$FRT == 8,1]
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
  studyAreaLarge = {
    sf::st_buffer(studyArea, dist = 20000) |> Cache()
  },
  rasterToMatchLarge = {reproducible::prepInputs(destinationPath = paths$inputPath,
                                                 url = paste0("https://ftp.maps.canada.ca/pub/nrcan_rncan/Forests_Foret/",
                                                              "canada-forests-attributes_attributs-forests-canada/",
                                                              "2001-attributes_attributs-2001/",
                                                              "NFI_MODIS250m_2001_kNN_Structure_Stand_Age_v1.tif"),
                                                 cropTo = studyAreaLarge, maskTo = studyAreaLarge,
                                                 writeTo = NULL,
                                                 method = c("near"), fun = "terra::rast") |> Cache()},
  # sa = setupSAandRTM(ecoprovinceNum = ecoprovince) |> Cache(),
  # studyArea = sa$studyArea,
  # rasterToMatch = sa$rasterToMatch,
  # rasterToMatchLarge = sa$rasterToMatch,
  # studyAreaLarge = sa$studyArea,
  # studyAreaReporting = sa$studyAreaReporting,
  # rasterToMatchReporting = sa$rasterToMatchReporting,
  studyAreaReporting = studyArea,
  sppEquiv = {
    species <- LandR::speciesInStudyArea(studyArea) |> Cache()
    spp <- grep("_Spp", species$speciesList, invert = TRUE, value = TRUE)
    column <- LandR::equivalentNameColumn(spp, LandR::sppEquivalencies_CA)
    LandR::sppEquivalencies_CA[which(LandR::sppEquivalencies_CA[[column]] %in% spp),]
  },
  # rasterToMatch = rasterToMatch,
  studyAreaPSP = {
    # aaaa <<- 1; on.exit(rm(aaaa, envir = .GlobalEnv))
    a <- reproducible::prepInputs(url = paste0("https://sis.agr.gc.ca/cansis/nsdb/ecostrat/",
                                               "province/ecoprovince_shp.zip"), dPath = paths$inputPath,
                                  fun = "terra::vect", projectTo = studyArea) |> Cache()
    b <- postProcess(a, studyArea = studyAreaLarge) |> Cache()
    ecoprovinces <- unique(b$ECOPROVINC)
    # studyAreaPSP <-
    a[a$ECOPROVINC %in% ecoprovinces] # |> terra::aggregate()
    #setupSAandRTM(ecoprovinceNum = studyAreaPSPprov)$studyArea |>
    # terra::aggregate(studyAreaPSP)
  },
  nonForestedLCCGroups = list(
    "nf_dryland" = c(50, 100, 40), # shrub, herbaceous, bryoid
    "nf_wetland" = c(81)), #non-treed wetland.
  # fireSense_ignitionFormula = paste0("ignitionsNoGT1 ~ (1|yearChar) + youngAge:CMDsm + nf_wetland:CMDsm",
  #                                    " + nf_dryland:CMDsm + ", paste0(unique(sppEquiv$fuel), ":CMDsm",
  #                                                                     collapse = " + ")),
  cores = {
    c(rep("localhost", 70), rep("n54", 10), rep("n14", 10), rep("n105", 10))
    # c(rep("bc213", 13),
    #   rep("localhost", 13),
    #   rep("n105", 13),
    #   rep("n14", 13),
    #   rep("bc184", 14),
    #   rep("bc217", 12),
    #   rep("n68", 20)
    # )
  },
  #params last because one of them depends on sppEquiv fuel class names
  climateVariables = list(
    historical_CMDsm = list(
      vars = "historical_CMD_sm",
      fun = quote(calcAsIs),
      .dots = list(historical_years = 1991:2022)
    )
  ),
  params = list(
    .globals = list(#.studyAreaName = studyAreaName2(studyArea, rasterToMatch, paths$projectPath),
      dataYear = 2011,
      .plots = "png",
      sppEquivCol = "LandR",
      .useCache = c(".inputObjects", "init")),
    Biomass_borealDataPrep = list(
      overrideAgeInFires = FALSE,
      overrideBiomassInFires = FALSE
    ),
    canClimateData = list(
      projectedClimateYears = 2011:2061,
      .useCloud = FALSE
    ),
    fireSense_SpreadFit = list(
      DEoptimTests = "adTest",
      # mutuallyExclusiveCols = list(
      #   youngAge = c("nf", unique(makeSppEquiv(ecoprovinceNum = ecoprovince)$fuel))
      # ),
      .useCache = FALSE,
      iterStep = 500, # run this many iterations before running again; this should be
      # set to itermax if Cache is not used; it is only useful for Cache
      cores = cores,
      #  "spades217",
      #"spades184")#, "spades213")
      #hosts <-
      #paste0("spades", c("97", "106", "184", "189", "213", "217", "220")))
      #c(hosts, "132.156.148.105", "localhost")
      NP = {if (identical(cores, unique(cores))) 100 else length(cores)}, # number of cores of machines
      # pemisc::makeIpsForNetworkCluster(
      # ipStart = "10.20.0",
      # ipEnd = c(97, 184, 189, 213, 220, 217, 106),
      # availableCores = c(28, 28, 28, 14, 14),
      # availableRAM = c(500, 500, 500, 250, 250),
      # localHostEndIp = 189,
      # proc = "cores",
      # nProcess = 10,
      # internalProcesses = 10,
      # sizeGbEachProcess = 1),
      trace = 1,
      mode = c("fit", "visualize"),
      # mode = c("debug"),
      # SNLL_FS_thresh = snll_thresh,
      doObjFunAssertions = FALSE
    ),
    fireSense_dataPrepFit = list(
      # spreadFuelClassCol = "fuel",
      # ignitionFuelClassCol = "fuel",
      missingLCCgroup = c("nf_dryland"),
      .useCache = c(".inputObjects", "init", "prepSpreadFitData", "prepIgnitionFitData")
    ),
    fireSense_IgnitionFit = list(
      rescalers = c("CMDsm" = 1000),
      .useCache = c(".inputObjects", "init", "prepIgnitionFitData")
    )
  )
)

plotSAs <- function(inSim, country = "CAN", saCols = c("purple", "blue", "green", "red"),
                    title = "Study Areas",
                    rasterToMatchLabel = "Stand Age", rasterToMatchPalette = "muted") {
  library(tidyterra)
  Canada <- {projectTo(SpaDES.project::setupStudyArea(studyArea = list(country = country)),
                       projectTo = inSim$studyArea) |>
      reproducible::postProcess(cropTo = inSim$studyAreaPSP)
  } |> Cache()
  p <- ggplot2::ggplot()
  rtms <- grep(names(inSim), pattern = "rasterToMatch", value = TRUE)
  sizesRtms <- sapply(inSim[rtms], function(rtm) terra::ncell(rtm))
  ordRtms <- order(sizesRtms)

  sas <- grep(names(inSim), pattern = "studyArea", value = TRUE)
  sizes <- sapply(sas, function(sa) areas(inSim[[sa]]))
  ord <- order(sizes)
  sizesOrdered <- rev(sizes[ord])

  saCols <- saCols[cumsum(!duplicated(sizesOrdered))]

  i <- 0
  subTitle <- character()
  for (rtm in inSim[rtms[min(ordRtms)]] ) {
    rtm[rtm[] == 0] <- NA
    p <- p + geom_spatraster(data = rtm)
    # p <- p + geom_spatvector(data = inSim[[sa]], fill = saCols[i]) #, aes(fill = tavg_04)) +
    subTitle <- c(subTitle, rtms[min(ordRtms)])
  }


  for (sa in rev(sas[ord])) {
    i <- i + 1
    p <- p + geom_spatvector(data = inSim[[sa]], fill = NA, col = saCols[i], lwd = 0.5) #, aes(fill = tavg_04)) +
    subTitle <- c(subTitle, paste0(sa, " (", saCols[i], ")"))
  }

  p <- p + geom_spatvector(data = Canada, fill = "NA")


  p +
    scale_fill_whitebox_c(
      na.value = "transparent",
      palette = rasterToMatchPalette
    ) +
    ggplot2::labs(
      fill = rasterToMatchLabel,
      title = title,
      subtitle = paste(subTitle, collapse = ", ")
    ) +
    ggplot2::theme_bw()
}

plotSAsLeaflet <- function(inSim, saCols = c("purple", "blue", "green", "red"),
                           title = "Study Areas",
                           rasterToMatchLabel = "Stand Age", rasterToMatchPalette = "muted") {
  rtmsNames <- grep(names(inSim), pattern = "rasterToMatch", value = TRUE)
  sizesRtms <- sapply(inSim[rtmsNames], function(rtm) terra::ncell(rtm))
  ordRtms <- order(sizesRtms)

  sas <- grep(names(inSim), pattern = "studyArea", value = TRUE)
  sizes <- sapply(sas, function(sa) areas(inSim[[sa]]))
  ord <- order(sizes)
  sizesOrdered <- rev(sizes[ord])

  saCols <- saCols[cumsum(!duplicated(sizesOrdered))]

  i <- 0
  subTitle <- character()
  largest <- inSim[rtmsNames[max(ordRtms)]]
  names(largest[[1]]) <- rasterToMatchLabel
  stk <- largest

  for (rtmName in rtmsNames[-max(ordRtms)] ) {
    # rtm[rtm[] == 0] <- NA
    rtm <- terra::extend(inSim[[rtmName]], largest[[1]])
    names(rtm) <- rasterToMatchLabel
    stk <- append(list(rtm) |> setNames(rtmName), stk)
  }
  rtms <- terra::rast(stk)


  rtms <- terra::project(rtms, "epsg:4326") |> Cache()
  names(rtms) <- paste0(names(rtms), "_", rasterToMatchLabel)
  a <- terra::plet(rtms, seq_len(nlyr(rtms)), collapse = FALSE)#, maxcell=5000000)
  # p <- p + geom_spatvector(data = inSim[[sa]], fill = saCols[i]) #, aes(fill = tavg_04)) +
  subTitle <- c(subTitle, rtmsNames[min(ordRtms)])

  v <- list()
  for (sa in rev(sas[ord])) {
    i <- i + 1
    if (!is(inSim[[sa]], "SpatVector")) {
      vv <- terra::vect(inSim[[sa]])
    } else {
      vv <- inSim[[sa]]
    }
    # labels = nc$NAME
    # vv <- terra::project(vv, terra::crs(rtm))
    # a <- a |> plet(map = _, vv, popup = TRUE, fill = 0, border = saCols[i], alpha = 1)
    a <- a |> polys(vv, fill = 0, border = saCols[i], col = saCols[i], alpha = 1) #|>
    #  leaflet::addLegend(colors = rep(saCols[i], length(vv)),
    #                  labels = vv$AREA)
    #sizes = 20,
    #shapes = "square",
    #borders = nc_pal(nc$NAME))
    # v <- append(v, list(vv)) # geom_spatvector(data = inSim[[sa]], fill = NA, col = saCols[i], lwd = 0.5) #, aes(fill = tavg_04)) +
    # subTitle <- c(subTitle, paste0(sa, " (", saCols[i], ")"))
  }
  # vc <- svc(v)
  a
}

areas <- function(x) {
  if (is(x, "SpatVector"))
    sum(terra::expanse(x))
  else if (is(x, "sf"))
    sum(sf::st_area(x))
  else
    NULL
}


SpaDES.core::Plots(inSim[grep("studyArea|rasterToMatch", names(inSim))],
                   title = paste0("StudyArea ", currentName),
                   fn = plotSAs, filename = paste0("studyAreas", currentName), path = inSim$paths$inputPath,
                   types = c("screen", "png")) |> Cache()

#add this after because of the quoted functions
# inSim$climateVariables <- list(
#   historical_CMDsm = list(
#     vars = "historical_CMD_sm",
#     fun = quote(calcAsIs),
#     .dots = list(historical_years = 1991:2022)
#   )
# )


#known bugs/undesirable behavior
#1 spreadFit dumps a bunch of figs in the project directory instead of outputs
#2 canClimateData occasionally fails, rather mysteriously. Unclear why
#3 Google Auth can be irritating when running via Bash

# devtools::install("~/GitHub/reproducible/", upgrade = FALSE); devtools::install("~/GitHub/SpaDES.core/", upgrade = FALSE);
# devtools::install("~/GitHub/climateData/", upgrade = FALSE);
# devtools::install("~/GitHub/fireSenseUtils/", upgrade = FALSE);
# devtools::install("~/GitHub/clusters/", upgrade = FALSE);
#
#
# pkgload::load_all("~/GitHub/climateData/");
# pkgload::load_all("~/GitHub/fireSenseUtils/");
# pkgload::load_all("~/GitHub/clusters/");
# devtools::document("~/GitHub/fireSenseUtils/");
# clearCache(ask = F)
# debug(messageDF)

if (FALSE) {
  fn <- "sim_FireSenseSpreadFit.qs"
  saveState(filename = fn, files = FALSE)
  inSim2 <- SpaDES.core::loadSimList(fn)
  outSims <- restartSpades(inSim2)
  outSims <- restartSpades()
}


if (FALSE) {
  options(
    gargle_oauth_cache = ".secret",
    gargle_oauth_email = "predictiveecology@gmail.com",
    gargle_oauth_client_type = "web"
  )
  googledrive::drive_auth(email = "predictiveecology@gmail.com", cache = "/home/emcintir/.secret/")
}

# inSim$debug <- quote(
#   {
#     b <- if(exists("spreadFirePoints", envir(sim))) {
#       cli::bg_br_cyan(cli::col_black(paste0(".robustDigest(sim$spreadFirePoints): ", .robustDigest(unlist(.robustDigest(sim$spreadFirePoints))))))
#     } else {
#       1
#     }
#     b
#   }
# )


if (FALSE) {
  pkgload::load_all("~/GitHub/reproducible/");
  pkgload::load_all("~/GitHub/SpaDES.core/");
  # pkgload::load_all("~/GitHub/SpaDES.project/");
  pkgload::load_all("~/GitHub/fireSenseUtils/");
  pkgload::load_all("~/GitHub/LandR/");
}
outSims <- do.call(what = SpaDES.core::simInitAndSpades, args = inSim, quote = TRUE)
