if (!testthat::is_testing()) source(testthat::test_path("setup.R"))

test_that("module runs as a standAlone when not dynamic", {

  ## Gets through simInit
  projectName <- "LandRCBM_split3pools"
  
  simInitTest <- simInit(
    modules = "LandRCBM_split3pools",
    times   = list(start = 2000, end = 2000),
    paths   = list(
      modulePath  = spadesTestPaths$modulePath,
      inputPath   = spadesTestPaths$inputPath,
      cachePath   = spadesTestPaths$cachePath,
      outputPath  = file.path(spadesTestPaths$temp$outputs, projectName)
    ),
    objects = list(
      rasterToMatch         = file.path(spadesTestPaths$testdata, "rasterToMatch.tif") |> terra::rast(),
      standDT               = file.path(spadesTestPaths$testdata, "CBM", "standDT.csv") |> data.table::fread(),
      cohortData            = file.path(spadesTestPaths$testdata, "LandR", "cohortData.csv") |> data.table::fread(),
      pixelGroupMap         = file.path(spadesTestPaths$testdata, "LandR", "pixelGroupMap.tif") |> terra::rast(),
      yieldTablesCumulative = file.path(spadesTestPaths$testdata, "LandR", "yieldTablesCumulative.csv") |> data.table::fread(),
      yieldTablesId         = file.path(spadesTestPaths$testdata, "LandR", "yieldTablesId.csv") |> data.table::fread()
    )
  )
  expect_s4_class(simInitTest, "simList")
  
  ## Get through init event:
  #  splits the cohort data and the yield tables
  simTest <- spades(simInitTest,
                    events = list("LandRCBM_split3pools" = c("init", "splitInit")))
  expect_s4_class(simTest, "simList")
  
  # check output object structure
  check_module_outputs(simTest, spinup = TRUE)
  
  # check yieldTablesCumulative
  preLandRCBM_ytc <- file.path(spadesTestPaths$testdata, "LandR", "yieldTablesCumulative.csv") |> data.table::fread()
  expect_equal(nrow(simTest$yieldTablesCumulative), nrow(preLandRCBM_ytc))

})

