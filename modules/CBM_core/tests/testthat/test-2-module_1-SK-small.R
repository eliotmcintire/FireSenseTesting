
if (!testthat::is_testing()) source(testthat::test_path("setup.R"))

test_that("Module: SK-small 1998-2000", {

  ## Run simInit and spades ----

  # Set up project
  projectName <- "module_SK-small_1998-2000"
  times       <- list(start = 1998, end = 2000)

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

    params = list(CBM_core = list(.saveSpinup = TRUE, .saveAll = TRUE, .plot = FALSE)),

    standDT           = file.path(paths$testdata, "SK-small/input", "standDT.qs2")           |> qs2::qs_read() |> data.table::as.data.table(),
    cohortDT          = file.path(paths$testdata, "SK-small/input", "cohortDT.qs2")          |> qs2::qs_read() |> data.table::as.data.table(),
    disturbanceMeta   = file.path(paths$testdata, "SK-small/input", "disturbanceMeta.qs2")   |> qs2::qs_read() |> data.table::as.data.table(),
    disturbanceEvents = file.path(paths$testdata, "SK-small/input", "disturbanceEvents.qs2") |> qs2::qs_read() |> data.table::as.data.table(),
    gcMeta            = file.path(paths$testdata, "SK/input",       "gcMeta.qs2")            |> qs2::qs_read() |> data.table::as.data.table(),
    gcIncrements      = file.path(paths$testdata, "SK/input",       "gcIncrements.qs2")      |> qs2::qs_read() |> data.table::as.data.table()
  )

  # Run simInit
  simTestInit <- SpaDES.core::simInit2(simInitInput)
  expect_s4_class(simTestInit, "simList")

  # Run spades
  simTest <- SpaDES.core::spades(simTestInit)
  expect_s4_class(simTest, "simList")


  ## Check inputs ----

  ## Check that input tables are not altered by module.
  refTables <- list(
    standDT           = qs2::qs_read(file.path(spadesTestPaths$testdata, "SK-small/input", "standDT.qs2")),
    cohortDT          = qs2::qs_read(file.path(spadesTestPaths$testdata, "SK-small/input", "cohortDT.qs2")),
    disturbanceMeta   = qs2::qs_read(file.path(spadesTestPaths$testdata, "SK/input",       "disturbanceMeta.qs2")),
    disturbanceEvents = qs2::qs_read(file.path(spadesTestPaths$testdata, "SK-small/input", "disturbanceEvents.qs2")),
    gcMeta            = qs2::qs_read(file.path(spadesTestPaths$testdata, "SK/input",       "gcMeta.qs2")),
    gcIncrements      = qs2::qs_read(file.path(spadesTestPaths$testdata, "SK/input",       "gcIncrements.qs2"))
  )
  outTables <- lapply(setNames(names(refTables), names(refTables)), function(table) simTest[[table]]) |>
    lapply(data.table::copy) |> lapply(data.table::setindex, NULL)

  expect_equal(outTables$standDT,           refTables$standDT)
  expect_equal(outTables$cohortDT,          refTables$cohortDT)
  expect_equal(outTables$disturbanceMeta,   refTables$disturbanceMeta)
  expect_equal(outTables$disturbanceEvents, refTables$disturbanceEvents)
  expect_equal(outTables$gcMeta,            refTables$gcMeta)
  expect_equal(outTables$gcIncrements,      refTables$gcIncrements)


  ## Check outputs ----

  # emissionsProducts
  expect_true(!is.null(simTest$emissionsProducts))
  expect_equal(
    data.table::as.data.table(simTest$emissionsProducts),
    qs2::qs_read(file.path(spadesTestPaths$testdata, "SK-small/valid", "emissionsProducts.qs2"))[
      , .SD, .SDcols = colnames(simTest$emissionsProducts)],
    check.attributes = FALSE)

  # Cohort data
  ## There should always be the same number of total cohort groups.
  expect_true(!is.null(simTest$cbm_vars$key))
  expect_identical(simTest$cbm_vars$key$cohortID,   simTest$cohortDT$cohortID)
  expect_identical(simTest$cbm_vars$key$pixelIndex, simTest$cohortDT$pixelIndex)
  expect_equal(max(simTest$cbm_vars$key$row_idx),            43)
  expect_equal(length(unique(simTest$cbm_vars$key$row_idx)), 43)
  expect_equal(nrow(simTest$cbm_vars$parameters),            43)
  expect_equal(nrow(simTest$cbm_vars$state),                 43)
  expect_equal(nrow(simTest$cbm_vars$flux),                  43)
  expect_equal(nrow(simTest$cbm_vars$pool),                  43)

  # Check sw_hw flag
  expect_equal(unique(simTest$cbm_vars$state[species_name == "Black spruce", sw_hw]), 0)
  expect_equal(unique(simTest$cbm_vars$state[species_name == "Jack pine",    sw_hw]), 0)
  expect_equal(unique(simTest$cbm_vars$state[species_name == "White birch",  sw_hw]), 1)
  expect_equal(unique(simTest$cbm_vars$state[species_name == "White spruce", sw_hw]), 0)

  # Check saved data
  outDataDir   <- file.path(simTest$spadesCBMdb, "data")
  validDataDir <- file.path(spadesTestPaths$testdata, "SK-small/valid/cbm_vars")

  for (year in times$start:times$end){
    expect_equal(
      qs2::qd_read(file.path(outDataDir,   paste0(year, "_key.qs2")))[, .(cohortID, pixelIndex, row_idx)],
      qs2::qd_read(file.path(validDataDir, paste0(year, "_key.qs2")))[, .(cohortID, pixelIndex, row_idx)]
    )
    for (table in c("parameters", "state", "flux", "pools")){
      tblValid <- qs2::qd_read(file.path(outDataDir,   paste0(year, "_", table, ".qs2")))
      expect_equal(
        qs2::qd_read(file.path(outDataDir,   paste0(year, "_", table, ".qs2")))[, .SD, .SDcols = names(tblValid)],
        tblValid
      )
    }
  }
})


