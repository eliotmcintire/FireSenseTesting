
if (!testthat::is_testing()) source(testthat::test_path("setup.R"))

test_that("Module: SK", {

  ## Run simInit and spades ----

  # Set up project
  projectName <- "SK-small"
  times       <- list(start = 1998, end = 2000)

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
      extent     = c(xmin = -687696, xmax = -681036, ymin = 711955, ymax = 716183),
      resolution = 30,
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

  expect_equal(nrow(simTest$standDT), 31302)
  expect_equal(simTest$standDT$pixelIndex, 1:31302)
  expect_in(simTest$standDT$area,              30 * 30)
  expect_in(simTest$standDT$admin_abbrev,      "SK")
  expect_in(simTest$standDT$admin_name,        "Saskatchewan")
  #expect_in(simTest$standDT$admin_boundary_id, 9) # Excluded from result
  expect_in(simTest$standDT$eco_id,           9)
  #expect_in(simTest$standDT$spatial_unit_id,   28) # Excluded from result


  ## Check output 'cohortDT' ----

  expect_true(!is.null(simTest$cohortDT))
  expect_true(inherits(simTest$cohortDT, "data.table"))

  for (colName in c("cohortID", "pixelIndex")){
    expect_true(colName %in% names(simTest$cohortDT))
    expect_true(all(!is.na(simTest$cohortDT[[colName]])))
  }

  expect_identical(data.table::key(simTest$cohortDT), "cohortID")

  expect_equal(nrow(simTest$cohortDT), 31302)
  expect_equal(simTest$cohortDT$pixelIndex, 1:31302)
  expect_equal(simTest$cohortDT$cohortID, simTest$cohortDT$pixelIndex)

})


