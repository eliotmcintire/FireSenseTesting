defineModule(sim, list(
  name = "CBM_core",
  description = "Modules that simulated the annual events as described in the CBM-CFS model", # "insert module description here",
  keywords = c("carbon", "CBM-CFS"),
  authors = c(
    person("Céline",  "Boisvenue", email = "celine.boisvenue@nrcan-rncan.gc.ca", role = c("aut", "cre")),
    person("Camille", "Giuliano",  email = "camsgiu@gmail.com",                  role = c("ctb")),
    person("Susan",   "Murray",    email = "murray.e.susan@gmail.com",           role = c("ctb"))
  ),
  childModules = character(0),
  version = list(CBM_core = "1.0.0.9000"),
  timeframe = as.POSIXlt(c(NA, NA)),
  timeunit = "year",
  citation = list("citation.bib"),
  documentation = list("README.txt", "CBM_core.Rmd"),
  reqdPkgs = list(
    "data.table", "reticulate", "qs2", "RSQLite", "withr",
    "PredictiveEcology/CBMutils@development (>=2.5)",
    "PredictiveEcology/libcbmr (>=0.0.1)"
  ),
  parameters = rbind(
    defineParameter(
      "default_delay_spinup", "integer", default = 0L, min = 0L, max = NA_integer_, desc = paste(
        "The default spinup delay.",
        "This can instead be set for each cohort with the cohortDT 'delaySpinup' column."
      )),
    defineParameter(
      "default_delay_regen", "integer", default = 0L, min = 0L, max = NA_integer_, desc = paste(
        "The default regeneration delay post disturbance.",
        "This can instead be set for each cohort with the cohortDT 'delayRegen' column."
      )),
    defineParameter(
      "default_historical_disturbance_type", "integer", default = 1L, NA_integer_, NA_integer_, desc = paste(
        "The default historical disturbance type ID. Examples: 1 = wildfire; 2 = clearcut.",
        "This can instead be set for each stand with the cohortDT 'historical_disturbance_type' column."
      )),
    defineParameter(
      "default_last_pass_disturbance_type", "numeric", default = 1L, NA_integer_, NA_integer_, desc = paste(
        "The default last pass disturbance type ID. Examples: 1 = wildfire; 2 = clearcut.",
        "This can instead be set for each stand with the cohortDT 'last_pass_disturbance_type' column."
      )),
    defineParameter(
      "emissionsProductsCols", "character", c("CO2", "CH4", "CO", "Emissions"), NA_character_, NA_character_,
      desc = "A vector of columns to return for emissions and products"),
    defineParameter(
      "skipPrepareCBMvars", "logical", default = FALSE, NA, NA,
      desc = "Whether the inputs for the cbm annual events are prepared by another module.E.g., LandRCBM_split3pools."),
    defineParameter(".saveInitial",  "numeric", start(sim), NA, NA, "Simulation year when the first save event should occur"),
    defineParameter(".saveInterval", "numeric", 1,          NA, NA, "Time interval between save events"),
    defineParameter(".saveSpinup",   "logical", TRUE,       NA, NA, "Save spinup results"),
    defineParameter(".saveAll",      "logical", FALSE,      NA, NA, "Save all available data"),
    defineParameter(".plot",         "logical", TRUE,       NA, NA, "Plot simulation results"),
    defineParameter(".useCache",     "logical", FALSE,      NA, NA, "Cache module events")
  ),
  inputObjects = bindrows(
    expectsInput(
      objectName = "standDT", objectClass = "data.table",
      desc = "Table of stand attributes. Stands can have 1 or more cohorts.",
      columns = c(
        pixelIndex = "Stand ID",
        area       = "Stand area in meters",
        admin_name = "Canada province or territory name",
        eco_id     = "Canada ecozone ID",
        historical_disturbance_type = "Historic CBM-CFS3 disturbance type ID. Defaults to the 'historical_disturbance_type' parameter",
        last_pass_disturbance_type  = "Last pass CBM-CFS3 disturbance type ID. Defaults to the 'last_pass_disturbance_type' parameter"
      )),
    expectsInput(
      objectName = "cohortDT", objectClass = "data.table",
      desc = "Table of cohort attributes. Must contain one or more additional classifier columns.",
      columns = c(
        cohortID    = "Cohort ID",
        pixelIndex  = "Stand ID",
        age         = "Cohort age at simulation start",
        ageSpinup   = "Optional. Alternative cohort age at the simulation start year to use in the spinup",
        delaySpinup = "Optional. Spinup delay. Defaults to the 'default_delay_spinup' parameter",
        delayRegen  = "Optional. Regeneration delay post disturbance in years. Defaults to the 'default_delay_regen' parameter"
      )),
    expectsInput(
      objectName = "gcMeta", objectClass = "data.table",
      desc = "Growth curve metadata",
      columns = c(
        gcID  = "Growth curve ID",
        sw    = "TRUE (softwood) or FALSE (hardwood)"
      )),
    expectsInput(
      objectName = "gcIncrements", objectClass = "data.table",
      desc = "Growth curve increments",
      columns = c(
        gcID        = "Growth curve ID",
        age         = "Cohort age",
        merch_inc   = "Change in carbon (MgC/ha/year) in merchantable pools",
        foliage_inc = "Change in carbon (MgC/ha/year) in foliage pools",
        other_inc   = "Change in carbon (MgC/ha/year) in other pools"
      )),
    expectsInput(
      objectName = "disturbanceEvents", objectClass = "data.table",
      desc = paste(
        "Table with disturbance events for each simulation year.",
        "Events types are defined in the 'disturbanceMeta' table.",
        "The module is indifferent to whether all events are provided as a single initial input",
        "or if they are created by another module during the simulation."),
      columns = c(
        pixelIndex = "Stand ID",
        year       = "Year of disturbance occurance",
        eventID    = "Event type ID. This associates events to metadata in the 'disturbanceMeta' table."
      )),
    expectsInput(
      objectName = "disturbanceMeta", objectClass = "data.table",
      desc = paste(
        "Table defining the disturbance event types.",
        "This associates CBM-CFS3 disturbances with the event IDs in the 'disturbanceEvents' table."),
      columns = c(
        eventID             = "Event type ID",
        disturbance_type_id = "CBM-CFS3 disturbance type ID",
        priority            = "Optional. Priority of event assignment to a pixel if more than one event occurs.",
        name                = "Optional. Disturbance name",
        description         = "Optional. Disturbance description",
        wholeStand          = "Optional. Specifies if the whole stand is disturbed (1 = TRUE; 0 = FALSE)"
      )),
    expectsInput(objectName = "masterRaster", objectClass = "raster",
                 desc = "Optional. Raster template for mapping results"),
    expectsInput(objectName = "cbm_defaults_db", objectClass = "character",
                 desc = "Optional. Path to CBM defaults SQLite database"),
    expectsInput(objectName = "cbm_exn_dir", objectClass = "character",
                 desc = "Optional. Path to CBM-EXN parameters directory")
  ),
  outputObjects = bindrows(
    createsOutput(
      objectName = "emissionsProducts", objectClass = "data.table",
      desc = paste(
        "Emissions and product totals for each simulation year.",
        "Choose which columns to return with the 'emissionsProductsCols' parameter.")),
    createsOutput(
      objectName = "spadesCBMdb", objectClass = "character",
      desc = "Path to SpaDES CBM database directory containing simulation data"),
    createsOutput(
      objectName = "cbm_vars", objectClass = "list",
      desc = paste(
        "List of 5 data tables defining active cohorts in the current year:",
        "key, parameters, pools, flux, and state.",
        "This is created initially during the spinup and updated each year."))
  )
))

doEvent.CBM_core <- function(sim, eventTime, eventType, debug = FALSE) {
  switch(
    eventType,
    init = {

      # Initiate module
      sim <- Init(sim)

      # Schedule spinup
      sim <- scheduleEvent(sim, start(sim), "CBM_core", "spinup", eventPriority = 5)

      # Schedule annual event
      sim <- scheduleEvent(sim, start(sim), "CBM_core", "annual_preprocessing", eventPriority = 8)
      sim <- scheduleEvent(sim, start(sim), "CBM_core", "annual_carbonDynamics", eventPriority = 8.5)

      # Schedule saving
      if (!is.null(P(sim)$.saveInitial)){
        sim <- scheduleEvent(sim, P(sim)$.saveInitial, "CBM_core", "save", eventPriority = 8.5)
      }

      # Schedule plotting
      if (P(sim)$.plot) sim <- scheduleEvent(sim, end(sim), "CBM_core", "plot", eventPriority = 12)
    },

    spinup = {

      sim <- spinup(sim)

      if (P(sim)$.saveSpinup){
        message("Saving spinup results to SpaDES CBM database")
        CBMutils::simCBMdbWrite(
          sim, year = 0,
          parameters = P(sim)$.saveAll, state = P(sim)$.saveAll, flux = FALSE)
      }
    },

    annual_preprocessing = {

      sim <- annual_prepDisturbances(sim)
      if (!P(sim)$skipPrepareCBMvars) sim <- annual_prepCohortGroups(sim)

      sim <- scheduleEvent(sim, time(sim) + 1, "CBM_core", "annual_preprocessing", eventPriority = 8)
    },

    annual_carbonDynamics = {

      sim <- annual_carbonDynamics(sim)

      sim <- scheduleEvent(sim, time(sim) + 1, "CBM_core", "annual_carbonDynamics", eventPriority = 8.5)
    },

    save = {

      message("Saving annual results to SpaDES CBM database")
      CBMutils::simCBMdbWrite(sim, parameters = P(sim)$.saveAll, state = P(sim)$.saveAll)

      sim <- scheduleEvent(sim, time(sim) + P(sim)$.saveInterval, "CBM_core", "save", eventPriority = 8.5)
    },

    plot = {
      sim <- plot(sim)
    },

    warning(noEventWarning(sim))
  )
  return(invisible(sim))
}

Init <- function(sim){

  # Set SpaDES CBM outputs database path
  sim$spadesCBMdb <- file.path(outputPath(sim), "spadesCBMdb")
  if (time(sim) == start(sim)){
    unlink(sim$spadesCBMdb, recursive = TRUE)
    if (file.exists(sim$spadesCBMdb)) stop(
      "Failed to remove existing SpaDES CBM database: ", sim$spadesCBMdb)
  }

  if (Sys.getenv("VIRTUAL_ENV") == ""){

    # Set up Python virtual environment
    reticulate::virtualenv_create(
      "r-spadesCBM",
      python = CBMutils::ReticulateFindPython(
        version        = ">=3.9,<=3.12.7",
        versionInstall = "3.11:latest",
        pyenvOnly      = TRUE))

    reticulate::virtualenv_install(
      "r-spadesCBM",
      pip_options = c("--upgrade", "-q"[identical(Sys.getenv("TESTTHAT"), "true")]),
      packages = c(
        "numpy<2",
        "pandas>=1.1.5,<=2.3.3",
        "scipy",
        "numexpr>=2.8.7",
        "numba",
        "pyyaml",
        "mock",
        "openpyxl",
        "libcbm"
      ))

    # Use Python virtual environment
    reticulate::use_virtualenv("r-spadesCBM")
  }

  # Return simList
  return(invisible(sim))

}

spinup <- function(sim) {

  if (!"delaySpinup" %in% names(sim$cohortDT)) message(
    "Spinup using the default delay: ", P(sim)$default_delay_spinup)
  if (!"historical_disturbance_type" %in% names(sim$standDT)) message(
    "Spinup using the default historical disturbance type ID: ", P(sim)$default_historical_disturbance_type)
  if (!"last_pass_disturbance_type"  %in% names(sim$standDT)) message(
    "Spinup using the default last pass disturbance type ID: ", P(sim)$default_last_pass_disturbance_type)

  # CBM-EXN spinup
  sim$cbm_vars <- cbmEXN_spinup(
    cohortDT        = merge(sim$cohortDT, sim$standDT, by = "pixelIndex", sort = FALSE, all.x = TRUE),
    growthMeta      = sim$gcMeta,
    growthIncr      = sim$gcIncrements,
    colname_age     = ifelse("ageSpinup"   %in% names(sim$cohortDT), "ageSpinup",   "age"),
    colname_delay   = ifelse("delaySpinup" %in% names(sim$cohortDT), "delaySpinup", "delay"),
    default_delay   = P(sim)$default_delay_spinup,
    default_historical_disturbance_type = P(sim)$default_historical_disturbance_type,
    default_last_pass_disturbance_type  = P(sim)$default_last_pass_disturbance_type,
    cbm_defaults_db = sim$cbm_defaults_db,
    cbm_exn_dir     = sim$cbm_exn_dir
  ) |> Cache()

  # 2026-03-10: Cache issue:
  ## Altering a data.table in the sim$cbm_vars list alters the cached table.
  ## Copy the tables as a temporary fix
  for (i in 1:length(sim$cbm_vars)) sim$cbm_vars[[i]] <- data.table::copy(sim$cbm_vars[[i]])

  # Set total cohort group area in cbm_vars$state table
  if (!"area" %in% names(sim$standDT)){
    warning("standDT does not have an \"area\" column; ",
            "area assumed to be 1 ha when calculating emissions and product totals.")
    sim$standDT[, area := 1]
  }
  groupAreas <- merge(sim$cbm_vars$key, sim$standDT, by = "pixelIndex")[, .(
    area = sum(area) / 10000), by = row_idx]
  data.table::setkey(groupAreas, row_idx)
  sim$cbm_vars$state$area <- groupAreas$area

  # Add regeneration delay to cbm_vars$state table
  data.table::setnames(sim$cbm_vars$state, "delayRegen", "delay", skip_absent = TRUE)
  if ("delay" %in% names(sim$cbm_vars$state)){
    sim$cbm_vars$state[is.na(delay), delay := P(sim)$default_delay_regen]
  }else{
    sim$cbm_vars$state[, delay := P(sim)$default_delay_regen]
  }

  # Return simList
  return(invisible(sim))
}

annual_prepDisturbances <- function(sim){

  if (!is.null(sim$disturbanceEvents)){

    if (!all(c("pixelIndex", "year", "eventID") %in% names(sim$disturbanceEvents))) stop(
      "'disturbanceEvents' table requires columns: 'pixelIndex', year', 'eventID'")

    distEvents <- sim$disturbanceEvents[year == as.character(time(sim)),]

  }else distEvents <- data.table()

  if (nrow(distEvents) == 0){
    message("No disturbance events for year ", time(sim))
    sim$cbm_vars$key[, disturbance_type_id := NA_integer_]
    return(invisible(sim))
  }

  # Read disturbance metadata
  if (is.null(sim$disturbanceMeta)) stop("'disturbanceMeta' input not found")
  if (!all(c("eventID", "disturbance_type_id") %in% names(sim$disturbanceMeta))) stop(
    "'disturbanceMeta' table requires columns: 'eventID', 'disturbance_type_id'")

  distMeta <- unique(sim$disturbanceMeta[, .SD, .SDcols = intersect(
    c("eventID", "disturbance_type_id", "priority"), names(sim$disturbanceMeta))])

  # Choose disturbance events by priority
  distEvents <- data.table::merge.data.table(distEvents, distMeta, by = "eventID", all.x = TRUE)

  if (any(duplicated(distEvents$pixelIndex))){

    if ("priority" %in% names(distEvents)){
      data.table::setkey(distEvents, pixelIndex, priority)

    }else stop(
      "Multiple disturbance events found in one or more pixels for year ", time(sim), ". ",
      "Use the 'disturbanceMeta' \"priority\" field to set event precendence.")
  }

  # Save disturbance events
  sim$cbm_vars$key[, disturbance_type_id := distEvents$disturbance_type_id[
    match(sim$cbm_vars$key$pixelIndex, distEvents$pixelIndex)]]

  # Return simList
  return(invisible(sim))
}

annual_prepCohortGroups <- function(sim) {

  # Set cohort group IDs for previous year
  sim$cbm_vars$key[, row_idx_prev := row_idx]

  # Set disturbance type IDs for undisturbed groups
  ## This may contain the disturbance type from the previous year
  sim$cbm_vars$parameters$disturbance_type <- 0L

  # Set data for disturbed cohorts
  distCohorts <- subset(sim$cbm_vars$key, !is.na(disturbance_type_id))

  if (nrow(distCohorts) > 0){

    # Create new groups that share attributes, events, and carbon with previous groups
    # since that changes the amount and destination of the carbon being moved.
    distCohorts <- merge(
      distCohorts,
      cbind(sim$cbm_vars$state, sim$cbm_vars$pools[, -1]),
      by.x = "row_idx_prev", by.y = "row_idx")
    data.table::setkey(distCohorts, cohortID)

    groupCols <- intersect(c(
      "disturbance_type_id", "spatial_unit_id",
      intersect(names(sim$cohortDT), names(sim$gcMeta)),
      "age", "delay",
      setdiff(names(sim$cbm_vars$pools), "row_idx")
    ), names(distCohorts))
    distCohorts[, row_idx := .GRP + max(sim$cbm_vars$state$row_idx), by = groupCols]

    # Update key
    sim$cbm_vars$key[!is.na(disturbance_type_id), row_idx := distCohorts$row_idx]

    # Prepare data for new groups
    distCohorts <- distCohorts[, .(
      row_idx_prev     = data.table::first(row_idx_prev),
      disturbance_type = data.table::first(disturbance_type_id)
    ), by = "row_idx"]
    cbm_vars_new <- list(parameters = distCohorts)

    # Set disturbed group state from data of previous group
    ## Clear information about previous disturbances
    cbm_vars_new[["state"]] <- merge(
      cbm_vars_new[["parameters"]][, .(row_idx, row_idx_prev)],
      sim$cbm_vars[["state"]],
      by.x = "row_idx_prev", by.y = "row_idx", all.x = TRUE)
    cbm_vars_new[["state"]][, age := 1L]
    cbm_vars_new[["state"]][, time_since_last_disturbance := NA_real_]
    cbm_vars_new[["state"]][, time_since_land_use_change  := NA_real_]
    cbm_vars_new[["state"]][, last_disturbance_type       := NA_real_]

    # Set disturbed group flux from data of previous group
    cbm_vars_new[["flux"]]  <- merge(
      cbm_vars_new[["parameters"]][, .(row_idx, row_idx_prev)],
      sim$cbm_vars[["flux"]],
      by.x = "row_idx_prev", by.y = "row_idx", all.x = TRUE)

    # Set disturbed group pools from data of previous group
    ## Set Input = 1
    cbm_vars_new[["pools"]] <- merge(
      cbm_vars_new[["parameters"]][, .(row_idx, row_idx_prev)],
      sim$cbm_vars[["pools"]],
      by.x = "row_idx_prev", by.y = "row_idx", all.x = TRUE)
    cbm_vars_new[["pools"]][, Input := 1L]

    # Merge new group data
    for (tableName in names(cbm_vars_new)){

      cbm_vars_new[[tableName]][, row_idx_prev := NULL]
      sim$cbm_vars[[tableName]] <- data.table::rbindlist(
        list(sim$cbm_vars[[tableName]], cbm_vars_new[[tableName]]),
        fill = TRUE)
      cbm_vars_new[[tableName]] <- NULL

      data.table::setkey(sim$cbm_vars[[tableName]], row_idx)
    }
  }

  # Set parameters from state
  sim$cbm_vars$parameters[, age                     := sim$cbm_vars$state$age]
  sim$cbm_vars$parameters[, mean_annual_temperature := sim$cbm_vars$state$mean_annual_temperature]

  # Remove rows for inactive cohort groups
  for (i in 1:length(sim$cbm_vars)){
    sim$cbm_vars[[i]] <- subset(sim$cbm_vars[[i]], row_idx %in% sim$cbm_vars$key$row_idx)
  }

  # Set growth increments: join via spinup cohort group IDs and age
  growthIncr <- sim$gcIncrements
  data.table::setkey(growthIncr, gcID, age)

  ## Extend increments to maximum age found in parameters
  ## This handles cases where the cohort ages exceed what is available in the increments
  maxIncr <- growthIncr[growthIncr[, .I[which.max(age)], by = "gcID"]$V1,]
  if (any(maxIncr$age < max(sim$cbm_vars$parameters$age))){

    warning("Cohort ages exceed growth increment ages. ",
            "Increments for the greatest available age have been applied to older cohorts.")

    growthIncr <- rbind(
      growthIncr, data.table::rbindlist(
        lapply(which(maxIncr$age < max(sim$cbm_vars$parameters$age)), function(i){
          cbind(age = (maxIncr[i,]$age + 1):(max(sim$cbm_vars$parameters$age) + 250),
                maxIncr[i,][, -("age")])
        }), use.names = TRUE))
    data.table::setkey(growthIncr, gcID, age)

    sim$gcIncrements <- growthIncr
  }

  annualIncr <- sim$cbm_vars$state |>
    merge(sim$gcMeta, by = c("admin_name", "eco_id", intersect(names(sim$cohortDT), names(sim$gcMeta)))) |>
    merge(growthIncr, by = c("gcID", "age"), all.x = TRUE)
  data.table::setkey(annualIncr, row_idx)

  sim$cbm_vars$parameters[, merch_inc   := annualIncr$merch_inc]
  sim$cbm_vars$parameters[, foliage_inc := annualIncr$foliage_inc]
  sim$cbm_vars$parameters[, other_inc   := annualIncr$other_inc]

  # Return simList
  return(invisible(sim))
}

annual_carbonDynamics <- function(sim) {

  # CBM-EXN step
  sim$cbm_vars <- cbmEXN_step(
    sim$cbm_vars,
    cbm_defaults_db = sim$cbm_defaults_db,
    cbm_exn_dir     = sim$cbm_exn_dir
  )

  # Set total cohort group area in cbm_vars$state table
  groupAreas <- merge(sim$cbm_vars$key, sim$standDT, by = "pixelIndex")[, .(
    area = sum(area) / 10000), by = row_idx]
  data.table::setkey(groupAreas, row_idx)
  sim$cbm_vars$state$area <- groupAreas$area

  # Summarize yearly emissions and products
  #Note: details of which source and sink pools goes into each of the columns in
  #cbm_vars$flux can be found here:
  #https://cat-cfs.github.io/libcbm_py/cbm_exn_custom_ops.html
  #cbm_vars$flux are in metric tonnes of carbon per ha like the rest of the
  #values produced.

  emissions <- (sim$cbm_vars$flux * sim$cbm_vars$state$area)[, lapply(.SD, sum), .SDcols = !"row_idx"]
  emissions[, CO2 := sum(DisturbanceBioCO2Emission, DecayDOMCO2Emission, DisturbanceDOMCO2Emission)]
  emissions[, CH4 := sum(DisturbanceBioCH4Emission, DisturbanceDOMCH4Emission)]
  emissions[, CO  := sum(DisturbanceBioCOEmission,  DisturbanceDOMCOEmission)]
  emissions[, Emissions := sum(CO2, CH4, CO)]

  # Summarize yearly (non-cumulative) products
  emissions[["Products"]] <- sum(sim$cbm_vars$pools$Products * sim$cbm_vars$state$area)
  if (!is.null(sim$emissionsProducts)){
    emissions[["Products"]] <- emissions[["Products"]] - sum(sim$emissionsProducts[, "Products"])
  }

  # Add to results
  sim$emissionsProducts <- rbind(
    sim$emissionsProducts,
    cbind(year = as.integer(time(sim)), emissions[, .SD, .SDcols = unique(
      c("Products", "Emissions", "CO2", "CH4", "CO", P(sim)$emissionsProductsCols))]))

  # Return simList
  return(invisible(sim))

}

plot <- function(sim){

  figPath <- file.path(outputPath(sim), "CBM_core_figures")

  cPlot <- CBMutils::simPlotEmissionsProducts(sim)
  SpaDES.core::Plots(cPlot,
                     filename = "emissionsProducts",
                     path = figPath,
                     ggsaveArgs = list(width = 14, height = 5, units = "in", dpi = 300),
                     types = "png")
  rm(cPlot)
  gc(full = FALSE, verbose = FALSE)

  if (is.null(P(sim)$.saveInitial)) return(invisible())

  saveYears <- seq(from = as.numeric(P(sim)$.saveInitial),
                   to   = as.numeric(time(sim)),
                   by   = as.numeric(P(sim)$.saveInterval))

  bPlot <- CBMutils::simPlotPoolProportions(
    sim, years = c(0[P(sim)$.saveSpinup], saveYears), useCache = FALSE)
  SpaDES.core::Plots(bPlot,
                     filename = "poolProportions",
                     path = figPath,
                     ggsaveArgs = list(width = 7, height = 5, units = "in", dpi = 300),
                     types = "png")
  rm(bPlot)
  gc(full = FALSE, verbose = FALSE)

  if (!is.null(sim$masterRaster)) for (year in range(saveYears)){

    nPlot <- CBMutils::simMapNPP(
      sim, year = year, useCache = FALSE)
    SpaDES.core::Plots(nPlot,
                       filename = paste0("NPP-", year),
                       path = figPath,
                       ggsaveArgs = list(width = 7, height = 5, units = "in", dpi = 300),
                       types = "png")
    rm(nPlot)
    gc(full = FALSE, verbose = FALSE)
  }

  if (!is.null(sim$masterRaster)) for (year in range(saveYears)){

    sPlot <- CBMutils::simMapTotalCarbon(
      sim, year = year, useCache = FALSE)
    SpaDES.core::Plots(sPlot,
                       filename = paste0("totalCarbon-", year),
                       path = figPath,
                       ggsaveArgs = list(width = 7, height = 5, units = "in", dpi = 300),
                       types = "png")
    rm(sPlot)
    gc(full = FALSE, verbose = FALSE)
  }

  # Return simList
  return(invisible(sim))
}

.inputObjects <- function(sim){

  return(invisible(sim))
}


