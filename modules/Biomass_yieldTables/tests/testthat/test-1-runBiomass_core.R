test_that("function runBiomass_core works", {
  
  # This runs the function runBiomass_core with a simList created by 
  # the module biomass_borealDataPrep.
  # The study area is a ~10km^2 a located in Northeast BC
  
  # Created with:
  
  # out <- SpaDES.project::setupProject(
  #   Restart = TRUE,
  #   paths = list(projectPath = getwd()),
  #   options = options(spades.moduleCodeChecks = FALSE,
  #                     spades.recoveryMode = FALSE),
  #   modules = c("PredictiveEcology/Biomass_borealDataPrep@development"
  #   ),
  #   params = list(
  #     .globals = list(
  #       dataYear = 2011, #will get kNN 2011 data, and NTEMS 2011 landcover
  #       sppEquivCol = "LandR",
  #       .plots = c("png"),
  #       .studyAreaName = "tiny",
  #       .useCache = c(".inputObjects", "init")
  #     ),
  #     Biomass_core = list(.plotInterval = 25)
  #   ),
  #   packages = c("googledrive", 'RCurl', 'XML', "stars"),
  #   useGit = F,
  #   studyArea = {
  #     reproducible::prepInputs(url = "https://drive.google.com/file/d/1LxacDOobTrRUppamkGgVAUFIxNT4iiHU/view?usp=sharing",
  #                              destinationPath = "inputs",
  #                              fun = sf::st_read,
  #                              overwrite = TRUE) |>
  #       terra::vect() |>
  #       terra::crop(terra::ext(1127000, 1130000, 1267000, 1270000)) |>
  #       terra::aggregate()
  #   },
  #   studyAreaLarge = studyArea,
  #   sppNameVector = c("Abie_las", "Popu_tre")
  # )
  updateFactorialOutputs = FALSE
  simOut <- suppressWarnings(
    SpaDES.core::loadSimList(file.path(spadesTestPaths$testdata, "smallSimOut.zip"),
                             projectPath = spadesTestPaths$temp$projects)
  )
  
  out <- runBiomass_core(moduleNameAndBranch ="PredictiveEcology/Biomass_core@main", 
                         paths = list(
                           modulePath  = file.path(spadesTestPaths$temp$modules, "Biomass_yieldTables", "submodules"),
                           inputPath   = spadesTestPaths$temp$inputs,
                           outputPath = spadesTestPaths$temp$outputs
                         ),
                         cohortData = simOut$cohortData,
                         species = simOut$species,
                         maxAge = NA,
                         simEnv = envir(simOut))
  
  if(updateFactorialOutputs) {
    cohortDataDir <- file.path(spadesTestPaths$testdata, "smallSimOut_cohortDataYield")
    copyDirectory(
      from = file.path(spadesTestPaths$temp$outputs, "cohortDataYield"),
      to = cohortDataDir,
      overwrite = TRUE
    )
    factorialOutputs <- out$simOutputs
    factorialOutputs$file <- file.path("testdata", "smallSimOut_cohortDataYield", list.files(cohortDataDir))
    fwrite(factorialOutputs, file.path(spadesTestPaths$testdata, "factorialOutputs.csv"))
  }
  
  
  # inspect output class
  expect_is(out, "list")
  expect_named(out, c("simOutputs", "digest", "yieldPixelGroupMap"))
  expect_is(out$simOutputs, "data.frame")
  expect_is(out$digest, "list")
  expect_is(out$yieldPixelGroupMap, "SpatRaster")
  expect_named(out$digest, c("outputHash", "preDigest"))
  
  #inspect simOutputs
  expect_true(all(out$simOutputs$objectName == "cohortData"))
  expect_true(all(out$simOutputs$saveTime == c(1:nrow(out$simOutputs)-1)))
  expect_true(all(out$simOutputs$saved))
  expect_true(nrow(out$simOutputs) == max(simOut$species$longevity)+1)
  
  
  out <- runBiomass_core(moduleNameAndBranch ="PredictiveEcology/Biomass_core@main", 
                         paths = list(
                           modulePath  = file.path(spadesTestPaths$temp$modules, "Biomass_yieldTables", "submodules"),
                           inputPath   = spadesTestPaths$temp$inputs,
                           outputPath = spadesTestPaths$temp$outputs
                         ),
                         cohortData = simOut$cohortData,
                         species = simOut$species,
                         maxAge = 20,
                         simEnv = envir(simOut))
  expect_equal(nrow(out$simOutputs), 21)
  
  
  out <- runBiomass_core(moduleNameAndBranch ="PredictiveEcology/Biomass_core@main", 
                         paths = list(
                           modulePath  = file.path(spadesTestPaths$temp$modules, "Biomass_yieldTables", "submodules"),
                           inputPath   = spadesTestPaths$temp$inputs,
                           outputPath = spadesTestPaths$temp$outputs
                         ),
                         cohortData = simOut$cohortData,
                         species = simOut$species,
                         maxAge = 1000,
                         simEnv = envir(simOut))
  expect_true(nrow(out$simOutputs) == max(simOut$species$longevity)+1)
  
  
})
