
if (!testthat::is_testing()) source(testthat::test_path("setup.R"))

test_that("Module: masterRaster missing", {

  # Set up project
  projectName <- "masterRasterMissing"
  times       <- list(start = 2025, end = 2025)

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
    )
  )

  # Run simInit
  simTestInit <- SpaDES.core::simInit2(simInitInput)
  expect_s4_class(simTestInit, "simList")

  # Run spades: expect error due to master raster missing
  expect_error(SpaDES.core::spades(simTestInit))
})


