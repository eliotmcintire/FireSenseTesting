
if (!testthat::is_testing()) source(testthat::test_path("setup.R"))

test_that("Module: SK 1985-2011", {

  ## Run simInit and spades ----

  # Set up project
  projectName <- "module_SK_1985-2011"
  times       <- list(start = 1985, end = 2011)

  simInitInput <- SpaDES.project::setupProject(

    modules = "CBM_core",
    times   = times,
    paths   = list(
      projectPath = spadesTestPaths$projectPath,
      modulePath  = spadesTestPaths$modulePath,
      packagePath = spadesTestPaths$packagePath,
      inputPath   = spadesTestPaths$inputPath,
      cachePath   = spadesTestPaths$cachePath,
      outputPath  = file.path(spadesTestPaths$temp$outputs, projectName),
      testdata    = spadesTestPaths$testdata
    ),

    params = list(CBM_core = list(.plot = FALSE, .saveAll = TRUE)),

    standDT           = file.path(paths$testdata, "SK/input", "standDT.qs2")           |> qs2::qs_read() |> data.table::as.data.table(),
    cohortDT          = file.path(paths$testdata, "SK/input", "cohortDT.qs2")          |> qs2::qs_read() |> data.table::as.data.table(),
    disturbanceMeta   = file.path(paths$testdata, "SK/input", "disturbanceMeta.qs2")   |> qs2::qs_read() |> data.table::as.data.table(),
    disturbanceEvents = file.path(paths$testdata, "SK/input", "disturbanceEvents.qs2") |> qs2::qs_read() |> data.table::as.data.table(),
    gcMeta            = file.path(paths$testdata, "SK/input", "gcMeta.qs2")            |> qs2::qs_read() |> data.table::as.data.table(),
    gcIncrements      = file.path(paths$testdata, "SK/input", "gcIncrements.qs2")      |> qs2::qs_read() |> data.table::as.data.table()
  )

  # Run simInit
  simTestInit <- SpaDES.core::simInit2(simInitInput)
  expect_s4_class(simTestInit, "simList")

  # Run spades
  simTest <- SpaDES.core::spades(simTestInit)
  expect_s4_class(simTest, "simList")


  ## Check outputs ----

  # emissionsProducts
  expect_true(!is.null(simTest$emissionsProducts))
  expect_equal(
    data.table::as.data.table(simTest$emissionsProducts),
    qs2::qs_read(file.path(spadesTestPaths$testdata, "SK/valid", "emissionsProducts.qs2"))[
      , .SD, .SDcols = colnames(simTest$emissionsProducts)],
    check.attributes = FALSE)

  # Cohort data
  ## There should always be the same number of total cohort groups.
  expect_true(!is.null(simTest$cbm_vars$key))
  expect_identical(simTest$cbm_vars$key$cohortID,   simTest$cohortDT$cohortID)
  expect_identical(simTest$cbm_vars$key$pixelIndex, simTest$cohortDT$pixelIndex)
  expect_equal(max(simTest$cbm_vars$key$row_idx),            4401)
  expect_equal(length(unique(simTest$cbm_vars$key$row_idx)), 4354) # Cohort groups eliminated by disturbances
  expect_equal(nrow(simTest$cbm_vars$parameters),            4354)
  expect_equal(nrow(simTest$cbm_vars$state),                 4354)
  expect_equal(nrow(simTest$cbm_vars$flux),                  4354)
  expect_equal(nrow(simTest$cbm_vars$pool),                  4354)

  # Check saved data
  testNPP <- data.table::rbindlist(lapply(times$start:times$end, function(year){
    merge(
      qs2::qd_read(file.path(simTest$spadesCBMdb, "data", paste0(year, "_key.qs2"))),
      qs2::qd_read(file.path(simTest$spadesCBMdb, "data", paste0(year, "_flux.qs2"))),
      by = "row_idx")[, .(
        year = year,
        NPP = sum(DeltaBiomass_AG, DeltaBiomass_BG,
                  TurnoverMerchLitterInput, TurnoverFolLitterInput, TurnoverOthLitterInput,
                  TurnoverCoarseLitterInput, TurnoverFineLitterInput)
      )]
  }))
  expect_equal(
    testNPP,
    qs2::qs_read(file.path(spadesTestPaths$testdata, "SK/valid", "NPP.qs2"))
  )
})


