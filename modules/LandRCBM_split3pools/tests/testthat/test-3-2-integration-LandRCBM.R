if (!testthat::is_testing()) source(testthat::test_path("setup.R"))

test_that("module runs with Biomass_core and CBM_core when dynamic", {
  
  # Set up project
  projectName <- "integration_LandRCBM"
  times <- list(start = 2000, end = 2021)
  
  simInitInput <- SpaDES.project::setupProject(
    
    modules = c(
      "PredictiveEcology/CBM_core@development",
      "PredictiveEcology/Biomass_core@development",
      "LandRCBM_split3pools"
    ),
    times = times,
    paths = list(
      projectPath = spadesTestPaths$projectPath,
      modulePath  = spadesTestPaths$temp$modules,
      packagePath = spadesTestPaths$packagePath,
      inputPath   = spadesTestPaths$inputPath,
      cachePath   = spadesTestPaths$cachePath,
      outputPath  = file.path(spadesTestPaths$temp$outputs, projectName),
      testdata    = spadesTestPaths$testdata
    ),
    params = list(
      .globals = list(
        .plots = NA,
        dataYear = 2001, #will get kNN 2011 data, and NTEMS 2011 landcover
        sppEquivCol = 'LandR'
      ),
      CBM_core = list(
        .plot = FALSE,
        skipPrepareCBMvars = TRUE
      )
    ),
    
    # Prepare input objects
    require = c("data.table", "terra", "sf"),
    
    rasterToMatch         = file.path(paths$testdata, "rasterToMatch.tif") |> terra::rast(),
    studyArea             = file.path(paths$testdata, "studyArea.shp") |> sf::st_read(quiet = TRUE),
    standDT               = file.path(paths$testdata, "CBM", "standDT.csv") |> data.table::fread(),
    biomassMap            = file.path(paths$testdata, "LandR", "biomassMap.tif") |> terra::rast(),
    cohortData            = file.path(paths$testdata, "LandR", "cohortData.csv") |> data.table::fread(stringsAsFactors = TRUE),
    pixelGroupMap         = file.path(paths$testdata, "LandR", "pixelGroupMap.tif") |> terra::rast(),
    speciesLayers         = file.path(paths$testdata, "LandR", "speciesLayers.tif") |> terra::rast(),
    ecoregionMap          = file.path(paths$testdata, "LandR", "ecoregionMap.tif") |> terra::rast(),
    minRelativeB          = file.path(paths$testdata, "LandR", "minRelativeB.csv") |> data.table::fread(stringsAsFactors = TRUE),
    ecoregion             = file.path(paths$testdata, "LandR", "ecoregion.csv") |> data.table::fread(colClasses = list(factor = c("ecoregionGroup"))),
    species               = file.path(paths$testdata, "LandR", "species.csv") |> data.table::fread(colClasses = list(factor = c("Area", "postfireregen", "hardsoft", "speciesCode"))),
    speciesEcoregion      = file.path(paths$testdata, "LandR", "speciesEcoregion.csv") |> data.table::fread(stringsAsFactors = TRUE),
    yieldTablesCumulative = file.path(paths$testdata, "LandR", "yieldTablesCumulative.csv") |> data.table::fread(),
    yieldTablesId         = file.path(paths$testdata, "LandR", "yieldTablesId.csv") |> data.table::fread()
  )
  
  # Run simInit
  simTestInit <- SpaDES.core::simInit2(simInitInput)
  expect_s4_class(simTestInit, "simList")
  
  # Run spades
  simTest <- SpaDES.core::spades(simTestInit)
  expect_s4_class(simTest, "simList")
  
  ## Check event order
  completedEvents <- completed(simTest)
  eventsToCheck <- c(
    "spinup",
    "postSpinupAdjustBiomass",
    "mortalityAndGrowth",
    "annualIncrements",
    "annual_preprocessing",
    "prepareCBMvars",
    "annual_carbonDynamics",
    "summarizeAGBPools",
    "accumulateResults",
    "plotSummaries"
  )
  
  # check event 1st year
  expectedEventOrder <- c(
    "spinup",
    "postSpinupAdjustBiomass",
    "mortalityAndGrowth",
    "annualIncrements",
    "annual_preprocessing",
    "prepareCBMvars",
    "annual_carbonDynamics",
    "summarizeAGBPools"
  )
  realizedEventOrder <- completedEvents[eventTime == start(simTest) & eventType %in% eventsToCheck]
  expect_equal(expectedEventOrder, realizedEventOrder$eventType)
  
  # check events last year
  expectedEventOrder <- c(
    "mortalityAndGrowth",
    "annualIncrements",
    "annual_preprocessing",
    "prepareCBMvars",
    "annual_carbonDynamics",
    "summarizeAGBPools",
    "plotSummaries"
  )
  realizedEventOrder <- completedEvents[eventTime == end(simTest) & eventType %in% eventsToCheck]
  expect_equal(expectedEventOrder, realizedEventOrder$eventType)
  
  # check output object structure
  check_module_outputs(simTest)
  
  # check cbm_vars
  expect_in(simTest$cbm_vars$state[speciesCode == "Abie_las", sw_hw], 0L) # SW
  expect_in(simTest$cbm_vars$state[speciesCode == "Pinu_con", sw_hw], 0L) # SW
  
  expect_true(all(simTest$cbm_vars$state$spatial_unit_id == 42))
  
})

