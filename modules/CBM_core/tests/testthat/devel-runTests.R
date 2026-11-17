
## OPTIONS ----

  # Set custom directory paths
  ## Speed up tests by allowing inputs, cache, and R packages to persist between runs
  options("spades.test.paths.inputs"   = NULL) # inputPath
  options("spades.test.paths.cache"    = NULL) # cachePath
  options("spades.test.paths.packages" = NULL) # packagePath

  # Test recreating the Python virtual environment
  ## WARNING: this will slow down testing, avoid unless Python is having issues
  Sys.setenv(RETICULATE_VIRTUALENV_ROOT = file.path(tempdir(), "virtualenvs"))


## RUN ALL TESTS ----

  # Run all tests
  testthat::test_dir("tests/testthat")

  # Run all tests with different reporters
  testthat::test_dir("tests/testthat", reporter = testthat::LocationReporter)
  testthat::test_dir("tests/testthat", reporter = testthat::SummaryReporter)


## RUN TEST SUBSETS ----

  # Run function tests
  testthat::test_dir("tests/testthat", filter = "function")

  # Run module tests
  testthat::test_dir("tests/testthat", filter = "module")

  # Run integration tests
  testthat::test_dir("tests/testthat", filter = "integration")

  # Run all SK-small tests
  testthat::test_dir("tests/testthat", filter = "SK-small")

  # Run LandRCBM tests
  testthat::test_dir("tests/testthat", filter = "LandRCBM")

