
if (!testthat::is_testing()) source(testthat::test_path("setup.R"))

test_that("Module: RIA", {

  ## Run simInit and spades ----

  # Set up project
  projectName <- "RIA-small"
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
    )
  )

  # Run simInit
  simTestInit <- SpaDES.core::simInit2(simInitInput)
  expect_s4_class(simTestInit, "simList")

  # Run spades
  simTest <- SpaDES.core::spades(simTestInit)
  expect_s4_class(simTest, "simList")


  ## Check output 'standDT' ----

  expect_true(!is.null(simTest$standDT))
  expect_true(inherits(simTest$standDT, "data.table"))

  for (colName in c("pixelIndex", "area", "admin_abbrev", "admin_name", "eco_id")){
    expect_true(colName %in% names(simTest$standDT))
    expect_true(all(!is.na(simTest$standDT[[colName]])))
  }
  expect_identical(data.table::key(simTest$standDT), "pixelIndex")

  expect_equal(nrow(simTest$standDT), 160000)
  expect_equal(simTest$standDT$pixelIndex, 1:160000)
  expect_in(simTest$standDT$area,              250*250)
  expect_in(simTest$standDT$admin_abbrev,      "BC")
  expect_in(simTest$standDT$admin_name,        "British Columbia")
  #expect_in(simTest$standDT$admin_boundary_id, 11) # Excluded from result
  expect_in(simTest$standDT$eco_id,            c(4, 9, 12, 14))
  #expect_in(simTest$standDT$spatial_unit_id,   c(38, 39, 40, 42)) # Excluded from result


  ## Check output 'cohortDT' ----

  expect_true(!is.null(simTest$cohortDT))
  expect_true(inherits(simTest$cohortDT, "data.table"))

  for (colName in c("cohortID", "pixelIndex")){
    expect_true(colName %in% names(simTest$cohortDT))
    expect_true(all(!is.na(simTest$cohortDT[[colName]])))
  }

  expect_identical(data.table::key(simTest$cohortDT), "cohortID")

  expect_equal(nrow(simTest$cohortDT), 160000)
  expect_equal(simTest$cohortDT$pixelIndex, 1:160000)
  expect_equal(simTest$cohortDT$cohortID, simTest$cohortDT$pixelIndex)

})


