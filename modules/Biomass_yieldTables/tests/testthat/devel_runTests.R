## OPTIONS ----

# Suppress warnings from calls to setupProject, simInit, and spades
options("spades.test.suppressWarnings" = TRUE)

# Set custom directory paths
## Speed up tests by allowing inputs, cache, and R packages to persist between runs
options("spades.test.paths.inputs"   = NULL) # inputPath
options("spades.test.paths.cache"    = NULL) # cachePath
options("spades.test.paths.packages" = NULL) # packagePath

## RUN ALL TESTS ----
# Run all tests
testthat::test_dir(file.path("tests", "testthat"))

## RUN INDIVIDUAL TESTS ----
testthat::test_file(file.path("tests", "testthat", "test-1-runBiomass_core.R"))
testthat::test_file(file.path("tests", "testthat", "test-2-ReadExperimentFiles.R"))
testthat::test_file(file.path("tests", "testthat", "test-3-pltfn.R"))
testthat::test_file(file.path("tests", "testthat", "test-4-updatePixelGroups.R"))
testthat::test_file(file.path("tests", "testthat", "test-5-Biomass_yieldTables.R"))
