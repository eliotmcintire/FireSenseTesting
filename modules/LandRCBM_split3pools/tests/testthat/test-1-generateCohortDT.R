if (!testthat::is_testing()) source(testthat::test_path("setup.R"))

test_that("function generateDt works", {
  
  # Create test data
  pixelGroupMap <- rast(matrix(c(10, 20, 10, NA), nrow = 2, ncol = 2, byrow = TRUE))
  cohortData <- data.table(
    pixelGroup = c(10,10,20,20),
    speciesCode = c("Abie_las", "Pinu_con", "Abie_las", "Pinu_ban"),
    B = c(10, 500, 300, 250),
    age = c(10, 10, 100, 0)
  )
  standDT <- data.table::data.table(
    pixelIndex   = 1:4,
    admin_abbrev = "BC",
    eco_id       = 1
  )
  yieldTablesId <- data.table(
    pixelIndex = c(1, 2, 3), 
    yieldTableIndex = c(1001, 1002, 1001) 
  )
  
  ## Runs with yieldTablesId
  
  # Run function
  result <- generateCohortDT(cohortData, pixelGroupMap, standDT, yieldTablesId)
  # Tests
  expect_is(result, "data.table")
  expect_equal(nrow(result), 5)
  expected_cols <- c("cohortID", "pixelIndex", "admin_abbrev", "eco_id", 
                     "speciesCode", "age", "yieldTableIndex", "gcID")
  expect_named(result, expected_cols, ignore.order = TRUE)
  expect_false(0 %in% result$age)
  
  # Pinu_con should all have the same gcID (both with Abi_las)
  gcid_pinu <- result[speciesCode == "Pinu_con", gcID]
  expect_equal(gcid_pinu[1], gcid_pinu[2])
  
  # Abie_las should have different gcID (two with Pinu_con, one alone)
  gcid_abie <- result[speciesCode == "Abie_las", gcID]
  expect_false(gcid_abie[1] == gcid_abie[2])
  
  # gcid for Abie_las and Pinu_con should be different and vice-versa
  expect_false(gcid_pinu[1] %in% gcid_abie)
  expect_false(any(gcid_abie %in% gcid_pinu))

  # Run function
  expect_error(generateCohortDT(cohortData, pixelGroupMap, standDT, yieldTablesId = NULL))

})
