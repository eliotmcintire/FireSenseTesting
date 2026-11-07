
if (!testthat::is_testing()) source(testthat::test_path("setup.R"))

test_that("Module runs with defaults", {

  ## Run simInit and spades ----

  # Set up project
  projectName <- "1-defaults"

  simInitInput <- SpaDES.project::setupProject(

    modules = "CBM_defaults",
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

  # Run spades
  simTest <- SpaDES.core::spades(simTestInit)

  expect_s4_class(simTest, "simList")


  ## Check outputs ----

  # cbm_defaults_db
  expect_true(file.exists(simTest$cbm_defaults_db))

  outPathSpl <- strsplit(spadesTestPaths$outputPath, "(/|\\\\)")[[1]]
  expect_identical(outPathSpl[(length(outPathSpl)-2):length(outPathSpl)],
                   strsplit(simTest$cbm_defaults_db, "(/|\\\\)")[[1]][(length(outPathSpl)-2):length(outPathSpl)])

  # cbm_exn_dir
  expect_true(file.exists(simTest$cbm_exn_dir))
  expect_true(length(list.files(simTest$cbm_exn_dir)) > 0)

  outPathSpl <- strsplit(spadesTestPaths$outputPath, "(/|\\\\)")[[1]]
  expect_identical(outPathSpl[(length(outPathSpl)-2):length(outPathSpl)],
                   strsplit(simTest$cbm_exn_dir, "(/|\\\\)")[[1]][(length(outPathSpl)-2):length(outPathSpl)])

  # CBMspecies
  expect_true(!is.null(simTest$CBMspecies))
  expect_true(inherits(simTest$CBMspecies, "data.table"))

  # disturbanceMatrix
  expect_true(!is.null(simTest$disturbanceMatrix))
  expect_true(inherits(simTest$disturbanceMatrix, "data.table"))

  # cTransfers
  expect_true(!is.null(simTest$cTransfers))
  expect_true(inherits(simTest$cTransfers, "data.table"))

  # spinupSQL
  expect_true(!is.null(simTest$spinupSQL))
  expect_true(inherits(simTest$spinupSQL, "data.table"))

})


