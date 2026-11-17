
## DEFINE MODULE ----

defineModule(sim, list(
  name = "CBM_dataPrep",
  description = "A data preparation module to format and prepare user-provided input to the SpaDES forest-carbon modelling family.",
  keywords = NA,
  authors = c(
    person("Céline", "Boisvenue", email = "celine.boisvenue@nrcan-rncan.gc.ca", role = c("aut", "cre")),
    person("Susan",  "Murray",    email = "murray.e.susan@gmail.com",           role = c("ctb"))
  ),
  childModules = character(0),
  version = list(CBM_dataPrep = "1.0.0.9001"),
  timeframe = as.POSIXlt(c(NA, NA)),
  timeunit = "year",
  citation = list("citation.bib"),
  documentation = list("CBM_dataPrep.Rmd"),
  reqdPkgs = list(
    "data.table", "RSQLite", "sf", "terra", "exactextractr", "gstat",
    "reproducible (>=2.1.2)", "digest", "googledrive",
    "PredictiveEcology/CBMutils@development (>=2.5.1)"
  ),
  parameters = rbind(
    defineParameter("saveRasters", "logical", FALSE, NA, NA, "Save rasters of inputs aligned to the `masterRaster`"),
    defineParameter("ageBacktrack", "list", NA, NA, NA, "Age backtracking parameters"),
    defineParameter("parallel.cores",     "integer",     NA, NA, NA, "Number of cores to use in parallel processing"),
    defineParameter("parallel.chunkSize", "integer", 25000L, NA, NA, "Chunk size to use in parallel processing"),
    defineParameter(".useCache", "character", "init", NA, NA, "Cache module events")
  ),
  inputObjects = bindrows(
    expectsInput(
      objectName = "masterRaster", objectClass = "SpatRaster|character",
      desc = paste(
        "Raster template defining the study area. NA cells will be excluded from analysis.",
        "This can be provided as a SpatRaster or URL.")),
    expectsInput(
      objectName  = "adminLocator",
      objectClass = "sf|SpatRaster|sourceID|URL|character",
      sourceID    = "StatCan-admin",
      desc = paste(
        "Canada administrative boundary name(s).",
        "This can be provided as a spatial object, a `CBMutils::CBMsources` sourceID, a URL, or a single value for all cohorts.")),
    expectsInput(
      objectName  = "ecoLocator",
      objectClass = "sf|SpatRaster|sourceID|URL|numeric",
      sourceID    = "CanSIS-ecozone",
      desc = paste(
        "Canada ecozone ID(s).",
        "This can be provided as a spatial object, a `CBMutils::CBMsources` sourceID, a URL, or a single value for all cohorts.")),
    expectsInput(
      objectName  = "ageLocator",
      objectClass = "sf|SpatRaster|sourceID|URL|numeric",
      desc = paste(
        "Cohort ages at the simulation start year.",
        "This can be provided as a spatial object, a `CBMutils::CBMsources` sourceID, a URL, or a single value for all cohorts.")),
    expectsInput(
      objectName = "ageDataYear", objectClass = "numeric",
      desc = "Year that the ages in `ageLocator` represent."),
    expectsInput(
      objectName = "ageBacktrackSplit", objectClass = "character",
      desc = "Optional. If backtracking ages, split the age layer by these `cohortDT` or `gcMeta` columns when interpolating ages."),
    expectsInput(
      objectName = "ageSpinupMin", objectClass = "numeric",
      desc = "Minimum age for cohorts during spinup. Temporary fix to CBM_core issue #1: https://github.com/PredictiveEcology/CBM_core/issues/1"),
    expectsInput(
      objectName = "gcIndexLocator", objectClass = "sf|SpatRaster|character",
      desc = paste(
        "Growth curve ID(s).",
        "This can be provided as a spatial object, a URL, or a single value for all cohorts.",
        "If provided, IDs will be added to the 'curveID' column of `cohortDT` and `curveID` will be set to 'curveID'")),
    expectsInput(
      objectName  = "cohortLocators",
      objectClass = "list",
      desc = paste(
        "Named list of data sources defining cohorts.",
        "Each item may be a spatial object, a `CBMutils::CBMsources` sourceID, a URL, or a single value for all cohorts.")),
    expectsInput(
      objectName  = "CBMsourceIDs",
      objectClass = "character",
      desc = "`CBMutils::CBMsources` sourceID(s) to use as cohort locators."),
    expectsInput(
      objectName = "curveID", objectClass = "character",
      desc = "Column(s) uniquely defining each growth curve in `cohortDT` and `userGcMeta`."),
    expectsInput(
      objectName = "userGcMeta", objectClass = "data.table",
      desc = paste(
        "Growth curve metadata. An input to CBM_vol2biomass.",
        "If provided, species names or LandR codes will be matched with known species get additional attributes.")),
    expectsInput(
      objectName = "gcMeta", objectClass = "data.table",
      desc = paste(
        "Growth curve metadata. An input to CBM_core.",
        "If provided, species names or LandR codes will be matched with known species get additional attributes.")),
    expectsInput(
      objectName = "disturbanceMeta", objectClass = "data.table",
      desc = "Table defining disturbance event types",
      columns = c(
        eventID               = "Event type ID",
        name                  = "Disturbance name (e.g. 'Wildfire').",
        disturbance_type_name = "Optional. CBM disturbance type name.",
        disturbance_type_id   = "Optional. CBM disturbance type ID.",
        sourceValue           = "Optional. Value in `disturbanceRasters` to include as events",
        sourceDelay           = "Optional. Delay (in years) of when the `disturbanceRasters` will take effect",
        sourceObjectName      = "Optional. Name of the object in the `simList` to retrieve the `disturbanceRasters` from annually."
      )),
    expectsInput(
      objectName = "disturbanceRasters", objectClass = "list",
      desc = paste(
        "Set of spatial data sources containing locations of disturbance events for each year.",
        "List items must be named by disturbance event IDs found in `disturbanceMeta`.",
        "Within each event's list, items must be named by the 4 digit year the disturbances occured in.",
        "For example, event type 1 disturbance locations for 2025 can be accessed with `disturbanceRasters[[\"1\"]][[\"2025\"]]`.",
        "Each disturbance item can be one of the following:",
        "a terra SpatRaster layer, one or more raster file paths, or sf polygons.",
        "All non-NA areas will be considered events unless the 'sourceValue' column is set."
      )),
    expectsInput(
      objectName = "disturbanceSource", objectClass = "character",
      desc = paste(
        "Names of known disturbance sources to use. Can be one or more of: 'NTEMS'.",
        "NTEMS source: CA Forest Fires 1985-2020 and CA Forest Harvest 1985-2020 GeoTIFF layers.",
          "Hermosilla, T., M.A. Wulder, J.C. White, N.C. Coops, G.W. Hobart, L.B. Campbell, 2016.",
          "Mass data processing of time series Landsat imagery: pixels to data products for forest monitoring.",
          "International Journal of Digital Earth 9(11), 1035-1054 (Hermosilla et al. 2016)."
      )),
    expectsInput(
      objectName = "cbm_defaults_db", objectClass = "character",
      sourceURL = "https://raw.githubusercontent.com/cat-cfs/libcbm_py/main/libcbm/resources/cbm_defaults_db/cbm_defaults_v1.2.9300.391.db",
      desc = "Path to the CBM-CBM3 defaults database")
  ),
  outputObjects = bindrows(
    createsOutput(
      objectName = "standDT", objectClass = "data.table",
      desc = "Table of stand attributes.",
      columns = c(
        pixelIndex   = "`masterRaster` cell index",
        area         = "`masterRaster` cell area in meters",
        admin_abbrev = "Canada administrative boundary abbreviation",
        admin_name   = "Canada administrative boundary name extracted from `adminLocator`",
        eco_id       = "Canada ecozone ID extracted from `ecoLocator`"
      )),
    createsOutput(
      objectName = "cohortDT", objectClass = "data.table",
      desc = "Table of cohort attributes.",
      columns = c(
        cohortID   = "`masterRaster` cell index",
        pixelIndex = "`masterRaster` cell index",
        age        = "Cohort ages extracted from `ageLocator`",
        ageSpinup  = "Cohort ages raised to >= `ageSpinupMin`"
      )),
    createsOutput(
      objectName = "ageDataYear", objectClass = "numeric",
      desc = paste(
        "Year that the ages in `ageLocator` represent.",
        "If `ageLocator` is a `CBMutils::CBMsources` sourceID this will be automatically set.",
        "Otherwise, if omitted, ages are assumed to represent the simulation start year.")),
    createsOutput(
      objectName = "curveID", objectClass = "character",
      desc = paste(
        "Column(s) uniquely defining each growth curve in `cohortDT` and `userGcMeta`.",
        "Defaults to the nmes of the columns created by `cohortLocators` and `CBMsourceIDs`.")),
    createsOutput(
      objectName = "userGcLocations", objectClass = "data.table",
      desc = "Table of combinations of growth curve, admin location, and ecozone ID in `cohortDT`."),
    createsOutput(
      objectName = "userGcMeta", objectClass = "data.table",
      desc = "Growth curve metadata with additional species attributes.",
      columns = list(
        LandR         = "LandR species code",
        sw            = "TRUE (softwood) or FALSE (hardwood)",
        canfi_species = "CanFI species codes",
        genus         = "Species genus"
      )),
    createsOutput(
      objectName = "gcMeta", objectClass = "data.table",
      desc = "Growth curve metadata with additional species attributes.",
      columns = list(
        LandR         = "LandR species code",
        sw            = "TRUE (softwood) or FALSE (hardwood)",
        canfi_species = "CanFI species codes",
        genus         = "Species genus"
      )),
    createsOutput(
      objectName = "disturbanceMeta", objectClass = "data.table",
      desc = "Table defining `disturbanceEvents` event types."),
    createsOutput(
      objectName = "disturbanceEvents", objectClass = "data.table",
      desc = "Table of disturbance events.")
  )
))


## MODULE EVENTS ----

doEvent.CBM_dataPrep <- function(sim, eventTime, eventType, debug = FALSE) {

  switch(
    eventType,

    init = {

      # Prepare master raster
      sim <- PrepMasterRaster(sim)

      # Prepare cohorts
      sim <- scheduleEvent(sim, start(sim), "CBM_dataPrep", "prepCohorts", eventPriority = 2)

      # Prepare species data
      sim <- scheduleEvent(sim, start(sim), "CBM_dataPrep", "matchSpecies", eventPriority = 1)

      # Prepare disturbances
      sim <- scheduleEvent(sim, start(sim), "CBM_dataPrep", "matchDisturbances", eventPriority = 8)
      sim <- scheduleEvent(sim, start(sim), "CBM_dataPrep", "readDisturbances",  eventPriority = 8)

      if ("NTEMS" %in% sim$disturbanceSource){
        sim <- scheduleEvent(sim, start(sim), "CBM_dataPrep", "readDisturbancesNTEMS", eventPriority = 1)
      }

      # CBM_vol2biomass prep
      if (!is.null(sim$curveID)){
        sim <- scheduleEvent(sim, start(sim), "CBM_dataPrep", "prepVol2Biomass", eventPriority = 4)
      }
    },

    prepCohorts = {

      # Read cohort data
      sim <- ReadCohorts(sim)

      # Pull ages for age adjustment
      ageStep <- "age" %in% names(sim$standDT) && !is.null(sim$ageDataYear) && start(sim) != sim$ageDataYear
      if (ageStep){

        if (is.null(sim$ageBacktrackSplit)){

          sim$ageTable <- sim$standDT[!is.na(age), .(pixelIndex, age)]

        }else{

          # Split age data by splitting columns
          ageTable <- sim$standDT

          colMissing <- setdiff(sim$ageBacktrackSplit, names(sim$standDT))
          if (length(colMissing) > 0){

            spsJoin <- lapply(c("userGcMeta", "gcMeta"), function(tbl){
              if (colMissing %in% names(sim[[tbl]])){
                joinCol <- intersect(c("species", "LandR"), intersect(names(sim$standDT), names(sim[[tbl]])))
                unique(sim[[tbl]][, .SD, .SDcols = c(joinCol, colMissing)])
              }
            })
            spsJoin <- spsJoin[!sapply(spsJoin, is.null)]

            if (length(spsJoin) == 0) stop(
              "ageBacktrackSplit column(s) not found: ",
              paste(shQuote(colMissing), collapse = ", "))

            joinCol <- intersect(c("species", "LandR"), intersect(names(sim$standDT), names(spsJoin[[1]])))
            ageTable[[joinCol]] <- as.character(ageTable[[joinCol]])
            ageTable <- merge(ageTable, spsJoin[[1]], by = joinCol, all.x = TRUE)
          }

          ageTable[, setdiff(names(ageTable), c("pixelIndex", "age", sim$ageBacktrackSplit)) := NULL]
          ageTable <- ageTable[rowSums(is.na(ageTable[, .SD, .SDcols = sim$ageBacktrackSplit])) == 0,]
          ageTable[, split := .GRP, by = eval(sim$ageBacktrackSplit)]
          data.table::setkey(ageTable, pixelIndex)

          sim$ageTable <- split(ageTable, ageTable$split)
          rm(ageTable)
        }
      }

      # Prep cohort tables
      sim <- PrepCohorts(sim)

      # Adjust cohort ages
      if (ageStep){

        # Read disturbances
        distYears <- sort(c(sim$ageDataYear, start(sim)))
        for (year in distYears[[1]]:(distYears[[2]]-1)){
          sim <- ReadDisturbances(sim, year = year)
        }

        # Step ages forward or backwards
        if (start(sim) > sim$ageDataYear) sim <- AgeStepForward(sim)
        if (start(sim) < sim$ageDataYear) sim <- AgeStepBackward(sim)

        rm("ageTable", envir = sim)
      }

      # Convert ages to integer; set spinup age
      if ("age" %in% names(sim$cohortDT)){

        if (!is.integer(sim$cohortDT$age)){
          sim$cohortDT[, age := as.integer(round(age))]
          sim$cohortDT[age < 0, age := 0]
        }
        if (!is.null(sim$ageSpinupMin)){
          sim$cohortDT[, ageSpinup := age]
          sim$cohortDT[ageSpinup < sim$ageSpinupMin, ageSpinup := sim$ageSpinupMin]
        }
      }
    },

    prepVol2Biomass = {
      sim <- PrepVol2Biomass(sim)
    },

    matchSpecies = {
      sim <- MatchSpecies(sim)
    },

    matchDisturbances = {
      sim <- MatchDisturbances(sim)
    },
    readDisturbances = {
      sim <- ReadDisturbances(sim)
      sim <- scheduleEvent(sim, time(sim) + 1, "CBM_dataPrep", "readDisturbances", eventPriority = 8)
    },
    readDisturbancesNTEMS = {
      sim <- ReadDisturbancesNTEMS(sim)
    },

    warning(noEventWarning(sim))
  )
  return(invisible(sim))
}

PrepMasterRaster <- function(sim){

  if (is.null(sim$masterRaster)) stop("masterRaster not found")

  if (!inherits(sim$masterRaster, "SpatRaster")){

    if (isURL(sim$masterRaster)){
      sim$masterRaster <- reproducible::prepInputs(
        destinationPath = inputPath(sim),
        url = sim$masterRaster,
        fun = terra::rast
      )

    }else{
      sim$masterRaster <- tryCatch(
        terra::rast(sim$masterRaster),
        error = function(e) stop(
          "masterRaster could not be converted to SpatRaster: ", e$message,
          call. = FALSE))
    }
  }

  if (terra::is.lonlat(sim$masterRaster))          stop("masterRaster must be in a projected CRS")
  if (terra::ncell(sim$masterRaster) > (2^31 - 1)) stop("masterRaster pixel count limited to 2^31 - 1")

  # Mask cells outside of admin boundary
  if (is.character(sim$adminLocator) && length(sim$adminLocator) == 1 &&
      !terra::global(sim$masterRaster, "anyNA")[1, 1]){

    adminBoundaries <- CBMutils::CBMsourcePrepInputs("StatCan-admin")$source
    if (sim$adminLocator %in% adminBoundaries$admin){

      adminMask <- subset(adminBoundaries, admin == sim$adminLocator) |>
        sf::st_segmentize(10000) |>
        sf::st_transform(sf::st_crs(sim$masterRaster))
      sim$masterRaster <- terra::mask(sim$masterRaster, adminMask, touches = FALSE)
    }
  }

  return(invisible(sim))
}

ReadCohorts <- function(sim){

  # Initiate pixel table
  allPixDT <- data.table::data.table(
    pixelIndex = 1:terra::ncell(sim$masterRaster),
    key = "pixelIndex")

  # Set cell area
  data.table::set(
    allPixDT, j = "area",
    value = prod(terra::res(sim$masterRaster) * terra::linearUnits(sim$masterRaster)))

  # Set cohort attributes from input sources
  colInputs <- list(
    admin_name   = sim$adminLocator,
    admin_abbrev = if (isValue(sim$adminLocator)) adminAbbrev(sim$adminLocator),
    eco_id       = sim$ecoLocator,
    age          = sim$ageLocator,
    curveID      = sim$gcIndexLocator
  )

  if (length(sim$cohortLocators) > 0){

    if (!is.list(sim$cohortLocators) || is.null(names(sim$cohortLocators))) stop(
      "'cohortLocators' must be a named list")
    if (any(is.na(names(sim$cohortLocators)))) stop("'cohortLocators' names contains NAs")

    colInputs <- c(colInputs, sim$cohortLocators)
  }

  if (length(sim$CBMsourceIDs) > 0){

    if (!all(sim$CBMsourceIDs %in% CBMutils::CBMsources$sourceID)) stop(
      "sourceID(s) not found in `CBMutils::CBMsources$sourceID`: ",
      paste(shQuote(setdiff(sim$CBMsourceIDs, CBMutils::CBMsources$sourceID)), collapse = ", "))

    colInputs <- c(
      colInputs,
      with(subset(CBMutils::CBMsources, sourceID %in% sim$CBMsourceIDs), setNames(sourceID, attr)))
  }

  colInputs <- colInputs[!sapply(colInputs, is.null)]
  for (colName in names(colInputs)){

    if (isValue(colInputs[[colName]])){

      # Set column as a single value
      if (is.character(colInputs[[colName]])) colInputs[[colName]] <- factor(colInputs[[colName]])
      data.table::set(allPixDT, j = colName, value = colInputs[[colName]])

    }else{

      if (isCBMsource(colInputs[[colName]])){

        message("Extracting CBM source '", colInputs[[colName]], "' into column '", colName, "'")

        sourceCBM <- CBMutils::CBMsourceExtractToRast(
          colInputs[[colName]], templateRast = sim$masterRaster
        ) |> reproducible::Cache(omitArgs = "templateRast", .cacheExtra = masterRasterDigest(sim))

        data.table::set(allPixDT, j = colName, value = sourceCBM$extractToRast)

        if (colName == "age") sim$ageDataYear <- sourceCBM$year

        rm(sourceCBM)

      }else{

        message("Extracting spatial input data into column '", colName, "'")

        if (isURL(colInputs[[colName]])){
          colInputs[[colName]] <- reproducible::prepInputs(
            destinationPath = inputPath(sim),
            url             = colInputs[[colName]])
        }

        data.table::set(allPixDT, j = colName, value = CBMutils::extractToRast(
          colInputs[[colName]], templateRast = sim$masterRaster) |>
            reproducible::Cache(omitArgs = "templateRast", .cacheExtra = masterRasterDigest(sim))
        )
      }

      if (P(sim)$saveRasters){
        outPath <- file.path(outputPath(sim), "CBM_dataPrep", paste0("input_", colName, ".tif"))
        message("Writing aligned raster to path: ", outPath)
        tryCatch(
          CBMutils::writeRasterWithValues(sim$masterRaster, allPixDT[[colName]], outPath, overwrite = TRUE),
          error = function(e) warning(e$message, call. = FALSE))
      }
    }
  }

  # Set admin_abbrev
  if (!"admin_abbrev" %in% names(allPixDT) & "admin_name" %in% names(allPixDT)){
    allPixDT[, admin_abbrev := adminAbbrev(admin_name)]
  }

  # Set cohort age data year if not set
  if ("age" %in% names(allPixDT) & is.null(sim$ageDataYear)){
    warning("'ageDataYear' not provided by user; `ageLocator` ages assumed to represent cohort age at simulation start")
    sim$ageDataYear <- as.numeric(start(sim))
  }

  # Return
  sim$standDT <- allPixDT
  return(invisible(sim))
}

PrepCohorts <- function(sim){

  tblCols <- list()
  tblCols$standDT  <- c("area", "admin_abbrev", "admin_name", "eco_id")
  tblCols$cohortDT <- setdiff(names(sim$standDT), c("pixelIndex", tblCols$standDT))

  # Subset stands and cohorts to cells where masterRaster is not NA
  sim$standDT <- sim$standDT[terra::cells(sim$masterRaster),]
  if (nrow(sim$standDT) == 0) stop("all masterRaster values are NA")

  # Remove cohorts that are missing key attributes
  if (length(tblCols$cohortDT) > 0){

    isNA  <- is.na(sim$standDT[, .SD, .SDcols = tblCols$cohortDT])
    hasNA <- colSums(isNA) > 0

    if (any(hasNA)){

      sim$standDT <- sim$standDT[rowSums(isNA[, hasNA, drop = FALSE]) == 0,]

      rmMsg <- paste0(
        round((1 - nrow(sim$standDT) / nrow(isNA)) * 100, 2),
        "% of pixels excluded due to NAs in one or more of: ",
        paste(shQuote(names(hasNA)[hasNA]), collapse = ", "))
      if (nrow(sim$standDT) == 0) stop(rmMsg)
      message(rmMsg)
    }
    rm(isNA)
    rm(hasNA)
  }

  # Remove pixels whose (admin_name, eco_id) pair has no CBM spatial unit
  if (all(c("admin_name", "eco_id") %in% names(sim$standDT)) && nrow(sim$standDT) > 0){

    cbmDBcon <- RSQLite::dbConnect(RSQLite::dbDriver("SQLite"), sim$cbm_defaults_db)
    spatialUnits <- data.table::as.data.table(RSQLite::dbReadTable(cbmDBcon, "spatial_unit")) |>
      merge(data.table::as.data.table(RSQLite::dbReadTable(cbmDBcon, "admin_boundary_tr"))[
        locale_id == 1, .(admin_boundary_id, admin_name = name)],
        by = "admin_boundary_id")
    RSQLite::dbDisconnect(cbmDBcon)
    data.table::setnames(spatialUnits, "eco_boundary_id", "eco_id")
    spatialUnits <- unique(spatialUnits[, paste(admin_name, eco_id)])

    noSPU <- !sim$standDT[, paste(admin_name, eco_id)] %in% spatialUnits

    if (any(noSPU)){

      rmPairs <- unique(sim$standDT[noSPU, .(admin_name, eco_id)])
      rmMsg <- paste0(
        round(sum(noSPU) / nrow(sim$standDT) * 100, 2),
        "% of pixels excluded: no CBM spatial unit for (admin_name, eco_id) pair(s): ",
        paste0("(", rmPairs$admin_name, ", ", rmPairs$eco_id, ")", collapse = ", "))

      if (!is.null(sim$cohortDT)){
        sim$cohortDT <- sim$cohortDT[!pixelIndex %in% sim$standDT[noSPU, pixelIndex],]
      }

      sim$standDT <- sim$standDT[!noSPU,]

      if (nrow(sim$standDT) == 0) stop(rmMsg)
      message(rmMsg)
    }
    rm(noSPU, spatialUnits)
  }

  if (is.null(sim$cohortDT)){
    sim$cohortDT <- sim$standDT[, .SD, .SDcols = c("pixelIndex", tblCols$cohortDT)]
    sim$cohortDT[, cohortID := pixelIndex]
    data.table::setkey(sim$cohortDT, cohortID)
    data.table::setcolorder(sim$cohortDT)
  }

  sim$standDT <- sim$standDT[, .SD, .SDcols = c("pixelIndex", tblCols$standDT)]

  return(invisible(sim))
}

AgeStepForward <- function(sim){

  # WORK IN PROGRESS
  warning("Cohort age data is from ", sim$ageDataYear, " instead of the simulation start year",
          call. = FALSE)

  return(invisible(sim))
}

AgeStepBackward <- function(sim){

  # Set cacheable function to backtrack ages
  ageStepBack <- function(ageRast, yearIn, yearOut, distEvents = NULL,
                          params = NULL, msgPrefix = NULL){

    stepRast <- withCallingHandlers(
      do.call(
        CBMutils::ageStepBackward, c(
          list(
            ageRast    = ageRast,
            yearIn     = yearIn,
            yearOut    = yearOut,
            distEvents = distEvents,
            parallel.cores     = P(sim)$parallel.cores,
            parallel.chunkSize = P(sim)$parallel.chunkSize
          ),
          params)
      ),
      message = function(m){
        message(msgPrefix, gsub("\\n", "", conditionMessage(m)))
        invokeRestart("muffleMessage")
      }
    )

    pixelIndex <- terra::cells(stepRast)
    data.table::data.table(
      pixelIndex = pixelIndex,
      age = terra::extract(stepRast, pixelIndex)[,1]
    )
  }

  sim$cohortDT[, age := NULL]
  if (is(sim$ageTable, "data.table")) sim$ageTable <- list(sim$ageTable)

  newAges <- data.table::rbindlist(lapply(sim$ageTable, function(ageTable){

    ageRast <- terra::rast(sim$masterRaster)
    terra::set.values(ageRast, ageTable$pixelIndex, ageTable$age)

    ageStepBack(
      ageRast    = ageRast,
      yearIn     = sim$ageDataYear,
      yearOut    = start(sim),
      distEvents = sim$disturbanceEvents,
      params     = if (is.list(P(sim)$ageBacktrack)) P(sim)$ageBacktrack
    ) |> reproducible::Cache()
  }))
  data.table::setkey(newAges, pixelIndex)

  sim$cohortDT <- data.table::merge.data.table(
    sim$cohortDT, newAges, by = "pixelIndex", all.x = TRUE)
  data.table::setkey(sim$cohortDT, cohortID)
  data.table::setcolorder(sim$cohortDT)

  if (P(sim)$saveRasters){

    ageRast <- terra::rast(sim$masterRaster)
    terra::set.values(ageRast, newAges$pixelIndex, newAges$age)

    outPath <- file.path(outputPath(sim), "CBM_dataPrep", paste0("input_age_", start(sim), ".tif"))
    message("Writing backtracked age raster to path: ", outPath)
    tryCatch(
      terra::writeRaster(ageRast, outPath, overwrite = TRUE),
      error = function(e) warning(e$message, call. = FALSE))
  }

  return(invisible(sim))
}

PrepVol2Biomass <- function(sim){

  if (!all(sim$curveID %in% names(sim$cohortDT))) stop("cohortDT does not contain all columns in `curveID`")

  # Define locations of existing growth curves
  userGcLocations <- cbind(sim$standDT[, .(admin_name, admin_abbrev, eco_id)],
                           sim$cohortDT[, .SD, .SDcols = sim$curveID])

  sim$userGcLocations <- unique(userGcLocations)

  return(invisible(sim))
}

MatchSpecies <- function(sim){

  for (gcMetaTable in intersect(c("gcMeta", "userGcMeta"), objects(sim))){
    if (any(!c("sw", "canfi_species", "genus") %in% names(sim[[gcMetaTable]]))){

      if (!data.table::is.data.table(sim[[gcMetaTable]])){
        sim[[gcMetaTable]] <- data.table::as.data.table(sim[[gcMetaTable]])
      }

      matchCol <- intersect(c("LandR", "species"), names(sim[[gcMetaTable]]))[1]
      if (length(matchCol) == 0) stop(
        gcMetaTable, " requires column(s) 'species' and/or 'LandR' to retrieve species metadata")

      sppMatchTable <- CBMutils::sppMatch(
        sim[[gcMetaTable]][[matchCol]],
        return     = c("EN_generic_full", "LandR", "Broadleaf", "CanfiCode", "Genus"),
        otherNames = list(
          "White birch" = "Paper birch"
        ))[, .(
          species       = EN_generic_full,
          LandR,
          sw            = !Broadleaf,
          canfi_species = CanfiCode,
          genus         = Genus
        )]

      sim[[gcMetaTable]] <- cbind(
        sim[[gcMetaTable]][, .SD, .SDcols = setdiff(names(sim[[gcMetaTable]]), names(sppMatchTable))],
        sppMatchTable)
      rm(sppMatchTable)
    }
  }

  return(invisible(sim))
}

MatchDisturbances <- function(sim){

  if (is.null(sim$disturbanceMeta)) return(invisible(sim))

  if (isURL(sim$disturbanceMeta)){
    sim$disturbanceMeta <- reproducible::prepInputs(
      destinationPath = inputPath(sim),
      url = sim$disturbanceMeta,
      fun = data.table::fread
    )
    data.table::setkey(sim$disturbanceMeta, eventID)
  }

  if (!inherits(sim$disturbanceMeta, "data.table")){
    sim$disturbanceMeta <- tryCatch(
      data.table::as.data.table(sim$disturbanceMeta),
      error = function(e) stop(
        "disturbanceMeta could not be converted to data.table: ", e$message, call. = FALSE))
  }

  # Match user disturbances with CBM disturbance types
  if (!any(c("disturbance_type_name", "disturbance_type_id") %in% names(sim$disturbanceMeta))){

    if (!"name" %in% names(sim$disturbanceMeta)) stop("disturbanceMeta requires 'name' column to set disturbance types")
    if (is.null(sim$cbm_defaults_db)) stop("cbm_defaults_db required to set disturbanceMeta disturbance types")

    askUser <- interactive() & !identical(Sys.getenv("TESTTHAT"), "true")
    if (askUser) message("Prompting user to match input disturbances with CBM disturbances:")
    distMatch <- CBMutils::distMatch(
      sim$disturbanceMeta$name,
      cbm_defaults_db = sim$cbm_defaults_db,
      ask = askUser
    ) |> reproducible::Cache()

    sim$disturbanceMeta <- cbind(
      sim$disturbanceMeta, distMatch[, .(disturbance_type_name = name, disturbance_type_id, description)]
    )
    data.table::setkey(sim$disturbanceMeta, eventID)
  }

  return(invisible(sim))
}

ReadDisturbances <- function(sim, year = time(sim)){

  # Get disturbances for the year
  distRasts <- lapply(sim$disturbanceRasters, function(d){
    if (as.character(year) %in% names(d)) d[[as.character(year)]]
  })

  # Retrieve disturbances from simList
  for (i in which(!is.na(sim$disturbanceMeta$sourceObjectName))){

    distRasts[[as.character(sim$disturbanceMeta[i,]$eventID)]] <- get(
      sim$disturbanceMeta[i,]$sourceObjectName, envir = sim)
  }

  # Summarize year events into a table
  distRasts <- distRasts[!sapply(distRasts, is.null)]
  if (length(distRasts) == 0) return(invisible(sim))

  if (is.null(names(distRasts)))    stop("disturbanceRasters list names must be disturbance event IDs")
  if (any(is.na(names(distRasts)))) stop("disturbanceRasters list names contains NAs")
  if (any(names(distRasts) == ""))  stop("disturbanceRasters list names contains empty strings")

  eventIDs <- suppressWarnings(tryCatch(
    as.integer(names(distRasts)),
    error = function(e) stop("disturbanceRasters list names must be coercible to integer")))

  newEvents <- lapply(1:length(distRasts), function(i){

    distMeta <- if (!is.null(sim$disturbanceMeta)){
      x <- as.list(subset(sim$disturbanceMeta, eventID == eventIDs[[i]]))
      x[!sapply(x, is.na)]
    }else list(eventID = eventIDs[[1]])

    with(distMeta, message(
      year, ": ",
      "Reading disturbances for eventID = ", eventID,
      if (exists("disturbance_type_id"))   paste("; CBM type ID =", disturbance_type_id),
      if (exists("disturbance_type_name")) paste("; name =", shQuote(disturbance_type_name))))

    distValues <- CBMutils::extractToRast(
      distRasts[[i]], templateRast = sim$masterRaster
    ) |> reproducible::Cache(omitArgs = "templateRast", .cacheExtra = masterRasterDigest(sim))

    if (P(sim)$saveRasters){
      outPath <- file.path(outputPath(sim), "CBM_dataPrep", sprintf("distEvents-%s_%s-%s.tif", eventIDs[[i]], year, i))
      message("Writing aligned raster to path: ", outPath)
      tryCatch(
        CBMutils::writeRasterWithValues(sim$masterRaster, outPath, values = distValues, overwrite = TRUE),
        error = function(e) warning(e$message, call. = FALSE))
    }

    if (length(na.omit(distMeta$sourceValue)) == 1){
      distValues <- which(distValues %in% distMeta$sourceValue)
    }else{
      distValues <- which(!is.na(distValues))
    }

    data.table::data.table(
      pixelIndex = distValues,
      year       = as.integer(year + c(na.omit(distMeta$sourceDelay), 0)[[1]]),
      eventID    = eventIDs[[i]]
    )
  })

  sim$disturbanceEvents <- data.table::rbindlist(c(list(sim$disturbanceEvents), newEvents)) |>
    unique()

  return(invisible(sim))
}

ReadDisturbancesNTEMS <- function(sim){

  if (any(c(1001, 1002) %in% sim$disturbanceMeta$eventID)) stop(
    "NTEMS disturbances reserve eventIDs 1001 and 1002")

  newDist <- rbind(
    data.table(
      eventID               = 1001L,
      disturbance_type_name = "Wildfire",
      disturbance_type_id   = 1,
      name                  = "NTEMS CA Forest Fires 1985-2020",
      url                   = "https://opendata.nfis.org/downloads/forest_change/CA_Forest_Fire_1985-2020.zip"
    ),
    data.table(
      eventID               = 1002L,
      disturbance_type_name = "Clearcut harvesting without salvage",
      disturbance_type_id   = 204, # Clearcut harvesting without salvage
      name                  = "NTEMS CA Forest Harvest 1985-2020",
      url                   = "https://opendata.nfis.org/downloads/forest_change/CA_Forest_Harvest_1985-2020.zip"
    )
  )

  sim$disturbanceMeta <- data.table::rbindlist(list(
    sim$disturbanceMeta, newDist[, 1:3]), fill = TRUE)
  data.table::setkey(sim$disturbanceMeta, eventID)

  newEvents <- lapply(1:nrow(newDist), function(i){

    url <- newDist[i,]$url
    sourceTIF <- reproducible::prepInputs(
      url,
      destinationPath = inputPath(sim),
      filename1   = basename(url),
      targetFile  = paste0(tools::file_path_sans_ext(basename(url)), ".tif"),
      alsoExtract = "similar",
      fun         = NA
    ) |> reproducible::Cache()

    with(newDist[i,], message(
      "Reading NTEMS disturbances for eventID = ", eventID,
      "; CBM type ID = ", disturbance_type_id,
      "; name = ", shQuote(name)))

    distValues <- data.table::data.table(year = CBMutils::extractToRast(
        sourceTIF, templateRast = sim$masterRaster
      ) |> reproducible::Cache(omitArgs = "templateRast", .cacheExtra = masterRasterDigest(sim))
    )

    if (P(sim)$saveRasters){
      outPath <- file.path(outputPath(sim), "CBM_dataPrep", paste0(newDist[i,]$name, '.tif'))
      message("Writing aligned raster to path: ", outPath)
      tryCatch(
        CBMutils::writeRasterWithValues(sim$masterRaster, outPath, values = distValues$year, overwrite = TRUE),
        error = function(e) warning(e$message, call. = FALSE))
    }

    distValues[, pixelIndex := .I]
    distValues <- distValues[!is.na(year),]
    distValues <- distValues[year != 0,]
    distValues[, year    := as.integer(year)]
    distValues[, eventID := newDist[i,]$eventID]
    data.table::setkey(distValues, pixelIndex, year)
    data.table::setcolorder(distValues)
    distValues
  })

  sim$disturbanceEvents <- data.table::rbindlist(c(list(sim$disturbanceEvents), newEvents))

  return(invisible(sim))
}

.inputObjects <- function(sim){

  # CBM defaults SQLite database
  if (!suppliedElsewhere("cbm_defaults_db", sim)){

    sim$cbm_defaults_db <- file.path(inputPath(sim), basename(extractURL("cbm_defaults_db")))

    if (!file.exists(sim$cbm_defaults_db)) reproducible::prepInputs(
      destinationPath = inputPath(sim),
      url         = extractURL("cbm_defaults_db"),
      targetFile  = basename(sim$cbm_defaults_db),
      dlFun       = download.file(extractURL("cbm_defaults_db"), sim$cbm_defaults_db, mode = "wb", quiet = TRUE),
      fun         = NA
    )
  }

  # Canada admin boundaries & ecozones
  defaultSourceIDs <- with(
    list(x = inputObjects(sim, "CBM_dataPrep")),
    sapply(split(x$sourceID, x$objectName), unlist))

  if (!suppliedElsewhere("adminLocator", sim)) sim$adminLocator <- defaultSourceIDs[["adminLocator"]]
  if (!suppliedElsewhere("ecoLocator",   sim)) sim$ecoLocator   <- defaultSourceIDs[["ecoLocator"]]

  # Growth curve ID
  if (!suppliedElsewhere("curveID", sim)){

    if (suppliedElsewhere("gcIndexLocator", sim)){
      sim$curveID <- "curveID"

    }else{
      curveID <- c(
        names(sim$cohortLocators)[!sapply(sim$cohortLocators, is.null)],
        setdiff(subset(CBMutils::CBMsources, sourceID %in% sim$CBMsourceIDs)$attr, "age")
      )
      if (length(curveID) > 0) sim$curveID <- unique(curveID)
    }
  }

  # Return simList
  return(invisible(sim))
}
