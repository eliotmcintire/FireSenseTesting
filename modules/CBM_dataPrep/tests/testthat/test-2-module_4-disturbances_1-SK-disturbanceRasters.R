
if (!testthat::is_testing()) source(testthat::test_path("setup.R"))

test_that("Module: SK with disturbanceRasters", {

  ## Run simInit and spades ----

  # Set up project
  projectName <- "SK-disturbanceRasters"
  times       <- list(start = 1998, end = 2000)

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
    ),

    # Set required packages for project set up
    require = "terra",

    # Set study area
    masterRaster = terra::rast(
      crs        = "EPSG:3979",
      extent     = c(xmin = -687696, xmax = -681036, ymin = 711955, ymax = 716183),
      resolution = 30,
      vals       = 1
    ),

    # Set disturbances
    ## Test matching user disturbances with CBM-CFS3 disturbances
    disturbanceMeta = rbind(
      data.frame(eventID = 1, wholeStand = 1, name = "Wildfire",
                 sourceValue = 1, sourceDelay = 1, sourceObjectName = NA_character_),
      data.frame(eventID = 2, wholeStand = 1, name = "Clearcut harvesting without salvage",
                 sourceValue = NA_integer_, sourceDelay = NA_integer_, sourceObjectName = "clearcut")
    ),
    disturbanceRasters = list(
      `1` = list(
        `1998` = terra::rast(
          crs        = "EPSG:3979",
          extent     = c(xmin = -687696, xmax = -681036, ymin = 711955, ymax = 716183),
          resolution = 30,
          vals       = c(1, 2, rep(NA, 31300))
        ))),
    clearcut = terra::rast(
      crs        = "EPSG:3979",
      extent     = c(xmin = -687696, xmax = -681036, ymin = 711955, ymax = 716183),
      resolution = 30,
      vals       = c(1, rep(NA, 31301))
    )
  )

  # Run simInit
  simTestInit <- SpaDES.core::simInit2(simInitInput)
  expect_s4_class(simTestInit, "simList")

  # Run spades
  simTest <- SpaDES.core::spades(simTestInit)
  expect_s4_class(simTest, "simList")


  ## Check output 'disturbanceMeta' ----

  expect_true(!is.null(simTest$disturbanceMeta))
  expect_true(inherits(simTest$disturbanceMeta, "data.table"))

  expect_equal(nrow(simTest$disturbanceMeta), 2)

  # Check that disturbances have been matched correctly
  expect_equal(simTest$disturbanceMeta$disturbance_type_id, c(1, 204))


  ## Check output 'disturbanceEvents' ----

  expect_true(!is.null(simTest$disturbanceEvents))
  expect_true(inherits(simTest$disturbanceEvents, "data.table"))

  for (colName in c("pixelIndex", "year", "eventID")){
    expect_true(colName %in% names(simTest$disturbanceEvents))
    expect_true(is.integer(simTest$disturbanceEvents[[colName]]))
    expect_true(all(!is.na(simTest$disturbanceEvents[[colName]])))
  }

  distEvents <- split(simTest$disturbanceEvents, simTest$disturbanceEvents$eventID)
  expect_identical(names(distEvents), c("1", "2"))

  expect_equal(distEvents[["1"]], rbind(
    data.table(pixelIndex = 1, year = 1999, eventID = 1)
  ))

  expect_equal(distEvents[["2"]], rbind(
    data.table(pixelIndex = 1, year = 1998, eventID = 2),
    data.table(pixelIndex = 1, year = 1999, eventID = 2),
    data.table(pixelIndex = 1, year = 2000, eventID = 2)
  ))
})


