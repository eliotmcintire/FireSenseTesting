
if (!testthat::is_testing()) source(testthat::test_path("setup.R"))

test_that("Module: RIA with NTEMS disturbances", {

  ## Run simInit and spades ----

  # Set up project
  projectName <- "RIA-disturbancesNTEMS"
  times       <- list(start = 2020, end = 2021)

  simInitInput <- SpaDES.project::setupProject(

    modules = "CBM_dataPrep",
    times   = times,
    paths   = list(
      projectPath = spadesTestPaths$projectPath,
      modulePath  = spadesTestPaths$modulePath,
      packagePath = spadesTestPaths$packagePath,
      inputPath   = spadesTestPaths$inputPath,
      cachePath   = spadesTestPaths$cachePath,
      outputPath  = file.path(spadesTestPaths$temp$outputs, projectName)
    ),

    # Set required packages for project set up
    require = "terra",

    # Set study area
    masterRaster = terra::rast(
      crs        = "EPSG:3979",
      extent     = c(xmin = -1653000, xmax = -1553000, ymin = 1180000, ymax = 1280000),
      resolution = 250,
      vals       = 1
    ),

    # Set disturbances
    disturbanceSource = "NTEMS"
  )

  # Run simInit
  simTestInit <- SpaDES.core::simInit2(simInitInput)
  expect_s4_class(simTestInit, "simList")

  # Run spades
  simTest <- SpaDES.core::spades(simTestInit)
  expect_s4_class(simTest, "simList")


  ## Check output 'disturbanceMeta' ----

  expect_true(!is.null(simTest$disturbanceMeta))
  expect_true(inherits(simTest$disturbanceMeta, "data.table"))

  expect_equal(nrow(simTest$disturbanceMeta), 2)

  # Check that disturbances have been matched correctly
  expect_equal(simTest$disturbanceMeta$disturbance_type_id, c(1, 204))


  ## Check output 'disturbanceEvents' ----

  expect_true(!is.null(simTest$disturbanceEvents))
  expect_true(inherits(simTest$disturbanceEvents, "data.table"))

  for (colName in c("pixelIndex", "year", "eventID")){
    expect_true(colName %in% names(simTest$disturbanceEvents))
    expect_true(is.integer(simTest$disturbanceEvents[[colName]]))
    expect_true(all(!is.na(simTest$disturbanceEvents[[colName]])))
  }

  distEventCount <- simTest$disturbanceEvents[, .(N = .N), by = c("eventID")]
  expect_equal(distEventCount, rbind(
    data.table(eventID = 1001, N = 2138),
    data.table(eventID = 1002, N = 4681)
  ), tolerance = 100, scale = 1)
})


