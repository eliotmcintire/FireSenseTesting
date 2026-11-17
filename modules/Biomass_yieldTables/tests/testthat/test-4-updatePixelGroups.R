test_that("function updatePixelGroups works", { 
  ngroup <- 3
  nage <- 5
  nsp <- 2
  cohortData <- data.table::data.table(
    expand.grid(pixelGroup = c(1:ngroup),
                age = c(1:nage),
                ecoregionGroup= 1,
                speciesCode = as.factor(c(1:nsp))),
    B = round(runif(ngroup*nage*nsp ,min = 0, 100))
  ) |> 
    setorder(ecoregionGroup, pixelGroup, speciesCode, age)  |>
    setcolorder(c("ecoregionGroup", "pixelGroup", "speciesCode", "age", "B"))
  
  # without changing anything, should return the same number of cohorts
  out <- updatePixelGroups(cohortData, c("speciesCode", "age", "B"),
                           returnRefTable = TRUE)
  expect_equal(out$cohortData, cohortData)
  # test returnRefTable = FALSE
  out <- updatePixelGroups(cohortData, c("speciesCode", "age", "B"),
                           returnRefTable = FALSE)
  expect_equal(out, cohortData)
  
  # Let's set all B to 0, all pixels have the same species composition, so should result in 1 pixelGroups
  cohortData$B <- 0
  out <- updatePixelGroups(cohortData, c("speciesCode", "age", "B"),
                           returnRefTable = TRUE)
  expect_true(all(out$cohortData$pixelGroup == 1))
  expect_equal(nrow(out$cohortData), nsp*nage)
  expect_equal(out$pixelGroupRef$pixelGroup, c(1:ngroup))
  expect_true(all(out$pixelGroupRef$pixelGroupYield == 1))
  
  # let's rm one species in pixelGroup 3
  cohortData <- cohortData[speciesCode != nsp | pixelGroup != ngroup]
  # now, we should get 2 groups
  out <- updatePixelGroups(cohortData, c("speciesCode", "age", "B"),
                           returnRefTable = TRUE)
  expect_true(any(out$cohortData$pixelGroup != 1))
  expect_true(all(out$cohortData$pixelGroup %in% c(1,2)))
  expect_equal(nrow(out$cohortData), nsp*nage + ((nsp-1)*nage))
  expect_true(all(out$pixelGroupRef[pixelGroup %in% c(1,2)]$pixelGroupYield == 1))
  expect_true(all(out$pixelGroupRef[pixelGroup == 3]$pixelGroupYield == 2))
  
  # Let's set all ages to 1, we expect 2 pixelGroup, and 3 rows
  cohortData$age <- 1
  out <- updatePixelGroups(cohortData, c("speciesCode", "age", "B"),
                           returnRefTable = TRUE)
  expect_equal(out$cohortData$pixelGroup, c(1,1,2))
  expect_equal(out$cohortData$speciesCode, as.factor(c(1,2,1)))
  expect_true(all(out$cohortData$age == 1))
  expect_true(all(out$cohortData$B == 0))
  expect_equal(nrow(out$pixelGroupRef), 3)
  expect_equal(out$pixelGroupRef$pixelGroupYield, c(1,1,2))
})