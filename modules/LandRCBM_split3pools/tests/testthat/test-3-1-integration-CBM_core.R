if (!testthat::is_testing()) source(testthat::test_path("setup.R"))

# Set the `pixelGroup` key to be equal to the `pixelIndex` for simplification.
{
  pixelGroupMap <- terra::rast(file.path(spadesTestPaths$testdata, "LandR", "pixelGroupMap.tif"))
  cohortData    <- data.table::fread(file.path(spadesTestPaths$testdata, "LandR", "cohortData.csv"))
  
  pixelGroupKey <- terra::values(pixelGroupMap)[,1]
  cohortData[, pixelGroup := (1:length(pixelGroupKey))[match(cohortData$pixelGroup, pixelGroupKey)]]
  data.table::setorder(cohortData, pixelGroup, speciesCode, age)
  
  pixelGroupMap <- terra::rast(pixelGroupMap, vals = 1:terra::ncell(pixelGroupMap))
  terra::set.values(pixelGroupMap, NA, cells = setdiff(1:terra::ncell(pixelGroupMap), cohortData$pixelGroup))
  
  dir.create(file.path(spadesTestPaths$temp$inputs, "intg-CBM_core"), showWarnings = FALSE)
  terra::writeRaster(pixelGroupMap, file.path(spadesTestPaths$temp$inputs, "intg-CBM_core", "pixelGroupMap.tif"), overwrite = TRUE)
  data.table::fwrite(cohortData, file.path(spadesTestPaths$temp$inputs, "intg-CBM_core", "cohortData.csv"))
  rm(pixelGroupMap)
  rm(cohortData)
}

test_that("Integration with CBM_core: spinup", {
  
  ## SIMULATE ----
  
  # Set up project
  projectName <- "integration_1-CBM_core_1-spinup"
  times <- list(start = 2000, end = 2000)
  
  simInitInput <- SpaDES.project::setupProject(
    
    modules = c(
      "LandRCBM_split3pools",
      "PredictiveEcology/CBM_core@development"
    ),
    times = times,
    paths = list(
      projectPath = spadesTestPaths$projectPath,
      modulePath  = spadesTestPaths$temp$modules,
      packagePath = spadesTestPaths$packagePath,
      inputPath   = spadesTestPaths$inputPath,
      cachePath   = spadesTestPaths$cachePath,
      outputPath  = file.path(spadesTestPaths$temp$outputs, projectName),
      testdata    = spadesTestPaths$testdata,
      tempdata    = file.path(spadesTestPaths$temp$inputs, "intg-CBM_core")
    ),
    params = list(
      .globals = list(
        .useCache = FALSE,
        .plots    = "png"
      ),
      CBM_core = list(
        skipPrepareCBMvars = TRUE
      )
    ),
    
    # Prepare input objects
    require = c("data.table", "terra"),
    
    pixelGroupMap         = file.path(paths$tempdata, "pixelGroupMap.tif") |> terra::rast(),
    cohortData            = file.path(paths$tempdata, "cohortData.csv") |> data.table::fread(stringsAsFactors = TRUE),
    
    rasterToMatch         = file.path(paths$testdata, "rasterToMatch.tif") |> terra::rast(),
    standDT               = file.path(paths$testdata, "CBM", "standDT.csv") |> data.table::fread(),
    yieldTablesCumulative = file.path(paths$testdata, "LandR", "yieldTablesCumulative.csv") |> data.table::fread(),
    yieldTablesId         = file.path(paths$testdata, "LandR", "yieldTablesId.csv") |> data.table::fread()
  )
  
  # Run simInit
  ## Suppress warnings about test modules missing metadata
  simTestInit <- suppressWarnings(SpaDES.core::simInit2(simInitInput))
  expect_s4_class(simTestInit, "simList")
  
  # Run spades
  simTest <- SpaDES.core::spades(simTestInit, events = list(
    LandRCBM_split3pools = c("init", "splitInit", "postSpinupAdjustBiomass"),
    CBM_core             = c("init", "spinup")
  ))
  expect_s4_class(simTest, "simList")
  
  
  ## CHECK ----
  
  # check output object structure
  check_module_outputs(simTest, spinup = TRUE)
  
})

test_that("Integration with CBM_core: step", {
  
  ## NOTE: this test runs for 2 years to test that the module prepares inputs 
  ## properly for CBM_core after the spinup and in a following simulation year.
  
  ## SIMULATE ----
  
  # Set up project
  projectName <- "integration_1-CBM_core_2-step"
  times <- list(start = 2000, end = 2001)
  
  simInitInput <- SpaDES.project::setupProject(
    
    modules = c(
      "test_growth",
      "LandRCBM_split3pools",
      "PredictiveEcology/CBM_core@development"
    ),
    times = times,
    paths = list(
      projectPath = spadesTestPaths$projectPath,
      modulePath  = spadesTestPaths$temp$modules,
      packagePath = spadesTestPaths$packagePath,
      inputPath   = spadesTestPaths$inputPath,
      cachePath   = spadesTestPaths$cachePath,
      outputPath  = file.path(spadesTestPaths$temp$outputs, projectName),
      testdata    = spadesTestPaths$testdata,
      tempdata    = file.path(spadesTestPaths$temp$inputs, "intg-CBM_core")
    ),
    params = list(
      .globals = list(
        .useCache = FALSE,
        .plots    = "png"
      ),
      CBM_core = list(
        skipPrepareCBMvars = TRUE
      )
    ),
    
    # Prepare input objects
    require = c("data.table", "terra"),
    
    pixelGroupMap         = file.path(paths$tempdata, "pixelGroupMap.tif") |> terra::rast(),
    cohortData            = file.path(paths$tempdata, "cohortData.csv") |> data.table::fread(stringsAsFactors = TRUE),
    
    rasterToMatch         = file.path(paths$testdata, "rasterToMatch.tif") |> terra::rast(),
    standDT               = file.path(paths$testdata, "CBM", "standDT.csv") |> data.table::fread(),
    yieldTablesCumulative = file.path(paths$testdata, "LandR", "yieldTablesCumulative.csv") |> data.table::fread(),
    yieldTablesId         = file.path(paths$testdata, "LandR", "yieldTablesId.csv") |> data.table::fread(),
    
    # Increase biomass for all cohorts by 100 g/m^2 (1 tonnes/ha)
    cohortGrowth = 100
  )
  
  # Run simInit
  ## Suppress warnings about test modules missing metadata
  simTestInit <- suppressWarnings(SpaDES.core::simInit2(simInitInput))
  expect_s4_class(simTestInit, "simList")
  
  # Run spades
  simTest <- SpaDES.core::spades(simTestInit)
  expect_s4_class(simTest, "simList")
  
  
  ## CHECK ----
  
  # check output object structure
  check_module_outputs(simTest)
  
  # gcIncrements
  ## Check that the total increase in carbon for each cohort is 0.5 tonnes/ha
  ## This is expected with an increase of biomass of 1 tonnes/ha for each cohort per year
  expect_true(all(
    round(simTest$gcIncrements[, .(inc = merch_inc + foliage_inc + other_inc)]$inc, 6) == 0.5
  ))
  
  # cohortDT
  ## Check that all cohorts are set as softwood
  expect_in(simTest$gcMeta$sw, TRUE)
  
  ## Expect that all input cohorts are still present
  inCohorts <- data.table::fread(file.path(spadesTestPaths$temp$inputs, "intg-CBM_core", "cohortData.csv"))
  expect_equal(nrow(simTest$cohortDT), nrow(inCohorts))
  
  ## Check cohort ages
  expect_equal(
    simTest$cohortDT[order(pixelIndex, speciesCode)]$age - (end(simTest) - start(simTest) + 1),
    inCohorts[order(pixelGroup, speciesCode)]$age
  )
})

test_that("Integration with CBM_core: step with new cohorts", {
  
  ## NOTE: this test runs for 1 year to check that the cohort's initial biomass
  ## leads to carbon increments with the expected value.
  
  ## SIMULATE ----
  
  # Set up project
  projectName <- "integration_1-CBM_core_4-step-new"
  times <- list(start = 2000, end = 2000)
  
  simInitInput <- SpaDES.project::setupProject(
    
    modules = c(
      "test_growth",
      "test_recruitment",
      "LandRCBM_split3pools",
      "PredictiveEcology/CBM_core@development"
    ),
    times = times,
    paths = list(
      projectPath = spadesTestPaths$projectPath,
      modulePath  = spadesTestPaths$temp$modules,
      packagePath = spadesTestPaths$packagePath,
      inputPath   = spadesTestPaths$inputPath,
      cachePath   = spadesTestPaths$cachePath,
      outputPath  = file.path(spadesTestPaths$temp$outputs, projectName),
      testdata    = spadesTestPaths$testdata,
      tempdata    = file.path(spadesTestPaths$temp$inputs, "intg-CBM_core")
    ),
    params = list(
      .globals = list(
        .useCache = FALSE,
        .plots    = "png"
      ),
      CBM_core = list(
        skipPrepareCBMvars = TRUE
      )
    ),
    
    # Prepare input objects
    require = c("data.table", "terra"),
    
    pixelGroupMap         = file.path(paths$tempdata, "pixelGroupMap.tif") |> terra::rast(),
    cohortData            = file.path(paths$tempdata, "cohortData.csv") |> data.table::fread(stringsAsFactors = TRUE),
    
    rasterToMatch         = file.path(paths$testdata, "rasterToMatch.tif") |> terra::rast(),
    standDT               = file.path(paths$testdata, "CBM", "standDT.csv") |> data.table::fread(),
    yieldTablesCumulative = file.path(paths$testdata, "LandR", "yieldTablesCumulative.csv") |> data.table::fread(),
    yieldTablesId         = file.path(paths$testdata, "LandR", "yieldTablesId.csv") |> data.table::fread(),
    
    # Increase biomass for all cohorts by 100 g/m^2 (1 tonnes/ha)
    cohortGrowth = 100,
    
    # Add a cohort to pixel 1 in year 2000
    cohortRecruit = data.table::data.table(
      year           = 2000,
      pixelGroup     = 1,
      speciesCode    = "Abie_las",
      ecoregionGroup = "1_210",
      age            = 1,
      B              = 200
    )
  )
  
  # Run simInit
  ## Suppress warnings about test modules missing metadata
  simTestInit <- suppressWarnings(SpaDES.core::simInit2(simInitInput))
  expect_s4_class(simTestInit, "simList")
  
  # Run spades
  simTest <- SpaDES.core::spades(simTestInit)
  expect_s4_class(simTest, "simList")
  
  
  ## CHECK ----
  
  # check output object structure
  check_module_outputs(simTest)
  
  # gcIncrements
  ## Check that the total increase in carbon for the new cohort is 1 tonnes/ha
  ## This is expected with an an addition of 2 tonnes/ha biomass in the first year
  gcID_new <- simTest$cohortDT[pixelIndex == 1 & age == 1, gcID]
  expect_equal(
    simTest$gcIncrements[gcID == gcID_new, sum(merch_inc, foliage_inc, other_inc)],
    1)
  
  ## Check that the total increase in carbon for the other cohorts is 0.5 tonnes/ha
  ## This is expected with an increase of biomass of 1 tonnes/ha for each cohort per year
  expect_true(all(
    round(simTest$gcIncrements[gcID != gcID_new, .(inc = merch_inc + foliage_inc + other_inc)]$inc, 6) == 0.5
  ))
  
  # cohortDT
  ## Check that all cohorts are set as softwood
  expect_in(simTest$gcMeta$sw, TRUE)
  
  ## Expect that 1 cohort has been added
  inCohorts <- data.table::fread(file.path(spadesTestPaths$temp$inputs, "intg-CBM_core", "cohortData.csv"))
  expect_equal(nrow(simTest$cohortDT[pixelIndex == 1]), 3)
  expect_equal(nrow(simTest$cohortDT), nrow(inCohorts) + 1)
  
  # check that new cohort is located in the correct pixel
  expect_equal(
    simTest$cbm_vars$key[row_idx == simTest$cbm_vars$state[gcID == gcID_new, row_idx], pixelIndex],
    1
  )
})

test_that("Integration with CBM_core: step with DOM cohorts: mortality", {
  
  ## NOTE: this test runs for 3 years to allow all cohorts experiencing mortality 
  ## to register as DOM. During the year that the cohort dies, the cohort will 
  ## have negative increments to move the existing biomass into DOM pools.
  
  ## SIMULATE ----
  
  # Set up project
  projectName <- "integration_1-CBM_core_3-step-DOM"
  times <- list(start = 2000, end = 2002)
  
  simInitInput <- SpaDES.project::setupProject(
    
    modules = c(
      "test_growth",
      "test_mortality",
      "LandRCBM_split3pools",
      "PredictiveEcology/CBM_core@development"
    ),
    times = times,
    paths = list(
      projectPath = spadesTestPaths$projectPath,
      modulePath  = spadesTestPaths$temp$modules,
      packagePath = spadesTestPaths$packagePath,
      inputPath   = spadesTestPaths$inputPath,
      cachePath   = spadesTestPaths$cachePath,
      outputPath  = file.path(spadesTestPaths$temp$outputs, projectName),
      testdata    = spadesTestPaths$testdata,
      tempdata    = file.path(spadesTestPaths$temp$inputs, "intg-CBM_core")
    ),
    params = list(
      .globals = list(
        .useCache = FALSE,
        .plots    = "png"
      ),
      CBM_core = list(
        skipPrepareCBMvars = TRUE
      )
    ),
    
    # Prepare input objects
    require = c("data.table", "terra"),    
    
    pixelGroupMap         = file.path(paths$tempdata, "pixelGroupMap.tif") |> terra::rast(),
    cohortData            = file.path(paths$tempdata, "cohortData.csv") |> data.table::fread(stringsAsFactors = TRUE),
    
    rasterToMatch         = file.path(paths$testdata, "rasterToMatch.tif") |> terra::rast(),
    standDT               = file.path(paths$testdata, "CBM", "standDT.csv") |> data.table::fread(),
    yieldTablesCumulative = file.path(paths$testdata, "LandR", "yieldTablesCumulative.csv") |> data.table::fread(),
    yieldTablesId         = file.path(paths$testdata, "LandR", "yieldTablesId.csv") |> data.table::fread(),
    
    # Increase biomass for all cohorts by 100 g/m^2 (1 tonnes/ha)
    cohortGrowth = 100,
    
    cohortMortality = rbind(
      
      # Year 2000: remove Abie_las from pixel 1
      data.table::data.table(year = 2000, pixelGroup = 1, speciesCode = "Abie_las"),
      
      # Year 2000: remove all cohorts from pixel 2
      data.table::data.table(year = 2000, pixelGroup = 2),
      
      # Year 2000: remove Abie_las from pixel 3
      # Year 2001: remove Pinu_con from pixel 3
      data.table::data.table(year = 2000, pixelGroup = 3, speciesCode = "Abie_las"),
      data.table::data.table(year = 2001, pixelGroup = 3, speciesCode = "Pinu_con"),
      
      fill = TRUE)
  )
  
  # Run simInit
  ## Suppress warnings about test modules missing metadata
  simTestInit <- suppressWarnings(SpaDES.core::simInit2(simInitInput))
  expect_s4_class(simTestInit, "simList")
  
  # Run spades
  simTest <- SpaDES.core::spades(simTestInit)
  expect_s4_class(simTest, "simList")
  
  
  ## CHECK ----
  
  # check output object structure
  check_module_outputs(simTest)
  
  # gcIncrements
  ## Check that the total increase in carbon for each cohort is 0.5 tonnes/ha
  ## This is expected with an increase of biomass of 1 tonnes/ha for each cohort per year
  expect_true(all(
    round(simTest$gcIncrements[, .(inc = merch_inc + foliage_inc + other_inc)]$inc, 6) == 0.5
  ))
  
  # cohortDT
  ## Check that all cohorts are set as softwood
  expect_in(simTest$gcMeta$sw, TRUE)
  
  ## Expect that 5 cohorts are gone
  inCohorts <- data.table::fread(file.path(spadesTestPaths$temp$inputs, "intg-CBM_core", "cohortData.csv"))
  expect_equal(nrow(simTest$cohortDT[pixelIndex == 1]), 1)
  expect_equal(nrow(simTest$cohortDT[pixelIndex == 2]), 0)
  expect_equal(nrow(simTest$cohortDT[pixelIndex == 3]), 0)
  expect_equal(nrow(simTest$cohortDT), nrow(inCohorts) - 5)
  
  # check that DOM cohorts are located in the correct pixels
  ## Pixels 2 and 3 should have 1 merged DOM cohort
  cohortState <- merge(simTest$cbm_vars$key, simTest$cbm_vars$state, by = "row_idx")
  
  expect_equal(nrow(cohortState[gcID == 0]), 3)
  
  expect_equal(nrow(cohortState[pixelIndex == 1]), 2)
  expect_equal(cohortState[pixelIndex == 1 & speciesCode == "Abie_las", gcID], 0)
  expect_false(cohortState[pixelIndex == 1 & speciesCode != "Abie_las", gcID] == 0)
  
  expect_equal(nrow(cohortState[pixelIndex == 2]), 1)
  expect_equal(cohortState[pixelIndex == 2, gcID], 0)
  
  expect_equal(nrow(cohortState[pixelIndex == 3]), 1)
  expect_equal(cohortState[pixelIndex == 3, gcID], 0)
  
})

test_that("Integration with CBM_core: step with DOM cohorts: disturbance", {
  
  ## NOTE: This test runs for 1 year to check that the disturbed cohorts
  ## Will have their disturbance type correctly registered in sim$cbm_vars
  ## and have been assigned increments == 0
  
  ## SIMULATE ----
  
  # Set up project
  projectName <- "integration_1-CBM_core_3-step-DOM"
  times <- list(start = 2000, end = 2000)
  
  simInitInput <- SpaDES.project::setupProject(
    
    modules = c(
      "test_growth",
      "test_disturbance",
      "LandRCBM_split3pools",
      "PredictiveEcology/CBM_core@development"
    ),
    times = times,
    paths = list(
      projectPath = spadesTestPaths$projectPath,
      modulePath  = spadesTestPaths$temp$modules,
      packagePath = spadesTestPaths$packagePath,
      inputPath   = spadesTestPaths$inputPath,
      cachePath   = spadesTestPaths$cachePath,
      outputPath  = file.path(spadesTestPaths$temp$outputs, projectName),
      testdata    = spadesTestPaths$testdata,
      tempdata    = file.path(spadesTestPaths$temp$inputs, "intg-CBM_core")
    ),
    params = list(
      .globals = list(
        .useCache = FALSE,
        .plots    = "png"
      ),
      CBM_core = list(
        skipPrepareCBMvars = TRUE
      )
    ),
    
    # Prepare input objects
    require = c("data.table", "terra"),    
    
    pixelGroupMap         = file.path(paths$tempdata, "pixelGroupMap.tif") |> terra::rast(),
    cohortData            = file.path(paths$tempdata, "cohortData.csv") |> data.table::fread(stringsAsFactors = TRUE),
    
    rasterToMatch         = file.path(paths$testdata, "rasterToMatch.tif") |> terra::rast(),
    standDT               = file.path(paths$testdata, "CBM", "standDT.csv") |> data.table::fread(),
    yieldTablesCumulative = file.path(paths$testdata, "LandR", "yieldTablesCumulative.csv") |> data.table::fread(),
    yieldTablesId         = file.path(paths$testdata, "LandR", "yieldTablesId.csv") |> data.table::fread(),
    
    # Increase biomass for all cohorts by 100 g/m^2 (1 tonnes/ha)
    cohortGrowth = 100,
    
    # Wildfire in pixel 3
    disturbanceMeta = data.table::data.table(
      eventID = 1,
      disturbance_type_id = 1
    ),
    disturbanceEvents = data.table::data.table(
      pixelIndex = 3,
      year       = 2000,
      eventID    = 1
    )
  )
  
  # Run simInit
  ## Suppress warnings about test modules missing metadata
  simTestInit <- suppressWarnings(SpaDES.core::simInit2(simInitInput))
  expect_s4_class(simTestInit, "simList")
  
  # Run spades
  simTest <- SpaDES.core::spades(simTestInit)
  expect_s4_class(simTest, "simList")
  
  
  ## CHECK ----
  
  # check output object structure
  check_module_outputs(simTest)
  
  # Check that disturbance_type_id registered in simTest$cbm_vars$key
  expect_equal(
    simTest$cbm_vars$key[pixelIndex == 3]$disturbance_type_id,
    rep(1, 2))
  
  # Check that disturbance_type_id registered in simTest$cbm_vars$parameters
  row_idx_dist <- simTest$cbm_vars$key[pixelIndex == 3, row_idx]
  expect_true(all(
    simTest$cbm_vars$parameters[row_idx %in% row_idx_dist, disturbance_type] == 1
  ))
  
  # Check that increments for disturbed cohorts are 0
  expect_true(all(
    simTest$cbm_vars$parameters[row_idx %in% row_idx_dist, .(merch_inc, foliage_inc, other_inc)] == 0
  ))
  
  # Check that the wildfire cleared all aboveground biomass
  expect_true(all(
    simTest$cbm_vars$pools[row_idx %in% row_idx_dist, .(Merch, Foliage, Other)] == 0
  ))
})


