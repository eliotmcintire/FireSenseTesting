
if (!testthat::is_testing()) source(testthat::test_path("setup.R"))

test_that("Multi module: RIA-small with LandR 2000-2002", {

  ## Run simInit and spades ----

  testthat::skip_on_ci()

  # Set up project
  projectName <- "integration_LandRCBM_RIA-small_2000-2002"
  times       <- list(start = 2000, end = 2002)

  simInitInput <- SpaDES.project::setupProject(

    modules = c(
      paste0("PredictiveEcology/Biomass_core@", Sys.getenv("BRANCH_NAME", "development")),
      "PredictiveEcology/LandRCBM_split3pools@main",
      "CBM_core"
    ),

    times   = times,
    paths   = list(
      projectPath = spadesTestPaths$projectPath,
      modulePath  = spadesTestPaths$temp$modules,
      packagePath = spadesTestPaths$packagePath,
      inputPath   = spadesTestPaths$inputPath,
      cachePath   = spadesTestPaths$cachePath,
      outputPath  = file.path(spadesTestPaths$temp$outputs, projectName),
      testdata    = spadesTestPaths$testdata
    ),

    require = c("terra", "reproducible"),

    # Prepare input objects
    studyArea             = file.path(paths$testdata, "LandRCBM-RIA-small/input", "studyArea.shp")     |> sf::st_read(quiet = TRUE),
    rasterToMatch         = file.path(paths$testdata, "LandRCBM-RIA-small/input", "rasterToMatch.tif") |> terra::rast(),
    standDT               = file.path(paths$testdata, "LandRCBM-RIA-small/input", "standDT.qs2")       |> qs2::qs_read() |> data.table::as.data.table(),
    biomassMap            = file.path(paths$testdata, "LandRCBM-RIA-small/input", "biomassMap.tif")    |> terra::rast(),
    cohortData            = file.path(paths$testdata, "LandRCBM-RIA-small/input", "cohortData.qs2")    |> qs2::qs_read() |> data.table::as.data.table(),
    pixelGroupMap         = file.path(paths$testdata, "LandRCBM-RIA-small/input", "pixelGroupMap.tif") |> terra::rast(),
    speciesLayers         = file.path(paths$testdata, "LandRCBM-RIA-small/input", "speciesLayers.tif") |> terra::rast(),
    ecoregionMap          = file.path(paths$testdata, "LandRCBM-RIA-small/input", "ecoregionMap.tif")  |> terra::rast(),
    minRelativeB          = file.path(paths$testdata, "LandRCBM-RIA-small/input", "minRelativeB.qs2")  |> qs2::qs_read() |> data.table::as.data.table(),
    ecoregion             = file.path(paths$testdata, "LandRCBM-RIA-small/input", "ecoregion.qs2")     |> qs2::qs_read() |> data.table::as.data.table(),
    species               = file.path(paths$testdata, "LandRCBM-RIA-small/input", "species.qs2")       |> qs2::qs_read() |> data.table::as.data.table(),
    speciesEcoregion      = file.path(paths$testdata, "LandRCBM-RIA-small/input", "speciesEcoregion.qs2") |> qs2::qs_read() |> data.table::as.data.table(),
    yieldTablesCumulative = file.path(paths$testdata, "LandRCBM-RIA-small/input", "yieldTablesCumulative.qs2") |> qs2::qs_read() |> data.table::as.data.table(),
    yieldTablesId         = file.path(paths$testdata, "LandRCBM-RIA-small/input", "yieldTablesId.qs2") |> qs2::qs_read() |> data.table::as.data.table(),
    sppEquiv = {
      speciesInStudy <- LandR::speciesInStudyArea(studyArea, dPath = paths$inputPath)
      species <- LandR::equivalentName(speciesInStudy$speciesList, df = LandR::sppEquivalencies_CA, "LandR")
      sppEquiv <- LandR::sppEquivalencies_CA[LandR %in% species]
      sppEquiv <- sppEquiv[KNN != "" & LANDIS_traits != ""]
    },

    # Parameters
    params = list(
      .globals = list(
        dataYear = 2001, #will get kNN 2011 data, and NTEMS 2011 landcover
        sppEquivCol = 'LandR'
      ),
      CBM_core = list(
        .plot = FALSE,
        skipCohortGroupHandling = TRUE,
        skipPrepareCBMvars = TRUE
      ))
  )

  # Run simInit
  simTestInit <- suppressWarnings(SpaDES.core::simInit2(simInitInput))
  expect_s4_class(simTestInit, "simList")

  # Run spades
  simTest <- suppressWarnings(SpaDES.core::spades(simTestInit))
  expect_s4_class(simTest, "simList")

  # Check outputs
  expect_true(!is.null(simTest$emissionsProducts))

})


