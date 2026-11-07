
#' CBM-EXN Spinup
cbmEXN_spinup <- function(cohortDT, growthMeta, growthIncr,
                          colname_gc      = intersect(names(cohortDT), names(growthMeta)),
                          colname_age     = "age",
                          colname_delay   = "delay",
                          default_delay   = 0L,
                          default_historical_disturbance_type = 1L,
                          default_last_pass_disturbance_type  = 1L,
                          cbm_defaults_db = NULL, cbm_exn_dir = NULL){

  ## Prepare input for spinup ----

  # Set resource paths
  if (!is.null(cbm_defaults_db)) withr::local_options(list(
    libcbmr.cbm_defaults_path = cbm_defaults_db
  ))
  if (!is.null(cbm_exn_dir)) withr::local_options(list(
    libcbmr.cbm_exn_parameters_dir = cbm_exn_dir
  ))

  # Read spatial unit parameters
  cbmDBcon <- RSQLite::dbConnect(RSQLite::dbDriver("SQLite"), libcbmr::get_cbm_defaults_path())
  spuMeta <- data.table::as.data.table(RSQLite::dbReadTable(cbmDBcon, "spatial_unit")) |>
    merge(data.table::as.data.table(RSQLite::dbReadTable(cbmDBcon, "spinup_parameter")),
          by.x = "spinup_parameter_id", by.y = "id") |>
    merge(data.table::as.data.table(RSQLite::dbReadTable(cbmDBcon, "admin_boundary_tr"))[
      locale_id == 1, .(admin_boundary_id, admin_name = name)],
      by = "admin_boundary_id")
  data.table::setnames(spuMeta, "id", "spatial_unit_id")
  RSQLite::dbDisconnect(cbmDBcon)

  # Read input tables
  reqCols <- list(
    cohortDT   = c("cohortID", colname_gc, colname_age),
    growthMeta = c("gcID", colname_gc, "sw"),
    growthIncr = c("gcID", "age", "merch_inc", "foliage_inc", "other_inc")
  )
  cohortDT   <- readDataTable(cohortDT,   "cohortDT",   colRequired = reqCols$cohortDT)
  growthMeta <- readDataTable(growthMeta, "growthMeta", colRequired = reqCols$growthMeta)
  growthIncr <- readDataTable(growthIncr, "growthIncr", colRequired = reqCols$growthIncr)

  if (!"spatial_unit_id" %in% names(cohortDT) &
      !all(c("admin_name", "eco_id") %in% names(cohortDT))) stop(
        "cohortDT must have either 'spatial_unit_id' or 'admin_name' and 'eco_id' columns")

  # Create cohort groups: groups of cohorts with the same attributes
  ## Allow all cohortDT attributes to be considered in unique groupings
  groupCols <- setdiff(names(cohortDT), c("cohortID", "pixelIndex", "area"))
  cohortDT[, row_idx := .GRP, by = groupCols]
  on.exit(cohortDT[, row_idx := NULL])

  # Isolate unique groups and join with parameters
  cohortGroups <- unique(cohortDT[, .SD, .SDcols = c("row_idx", groupCols)])

  if ("spatial_unit_id" %in% names(cohortGroups)){
    spuMetaJoin <- "spatial_unit_id"
  }else{
    data.table::setnames(spuMeta, "eco_boundary_id", "eco_id")
    spuMetaJoin <- c("admin_name", "eco_id")
  }

  cohortGroups <- cohortGroups |>
    merge(growthMeta, by = colname_gc,  suffixes = c("", ".y"), all.x = TRUE) |>
    merge(spuMeta,    by = spuMetaJoin, suffixes = c("", ".y"), all.x = TRUE)
  cohortGroups[, which(grepl("\\.y$", names(cohortGroups))) := NULL]
  data.table::setkey(cohortGroups, row_idx)

  # Set area to 1ha
  cohortGroups[, area := 1L] # 1ha

  # Set species ID
  speciesCBM <- data.table::fread(file.path(libcbmr::get_cbm_exn_parameters_dir(), "species.csv"))
  speciesIDs <- c(
    sw = speciesCBM[species_name == "Unspecified softwood species", species_id],
    hw = speciesCBM[species_name == "Unspecified hardwood species", species_id]
  )
  cohortGroups[, species := data.table::fifelse(sw, speciesIDs[["sw"]], speciesIDs[["hw"]])]

  # Prepare sw_hw column for Python
  cohortGroups[, sw_hw := as.integer(!sw)]
  cohortGroups[, sw    := NULL]

  # Set column names for Python
  if (colname_age != "age"){
    data.table::setnames(
      cohortGroups, c(colname_age, "age"), c("age", "age_in"), skip_absent = TRUE)
  }
  if (colname_delay != "delay" && colname_delay %in% names(cohortGroups)){
    data.table::setnames(cohortGroups, colname_delay, "delay")
  }

  # Set defaults
  if ("delay" %in% names(cohortGroups)){
    cohortGroups[is.na(delay), delay := default_delay]
  }else{
    cohortGroups[, delay := default_delay]
  }
  if ("historical_disturbance_type" %in% names(cohortGroups)){
    cohortGroups[is.na(historical_disturbance_type),
                 historical_disturbance_type := default_historical_disturbance_type]
  }else{
    cohortGroups[, historical_disturbance_type := default_historical_disturbance_type]
  }
  if ("last_pass_disturbance_type" %in% names(cohortGroups)){
    cohortGroups[is.na(last_pass_disturbance_type),
                 last_pass_disturbance_type := default_last_pass_disturbance_type]
  }else{
    cohortGroups[, last_pass_disturbance_type := default_last_pass_disturbance_type]
  }

  # Join growth increments with cohort group IDs
  ## Drop growth increments age <= 0
  growthIncrGroups <- data.table::merge.data.table(
    cohortGroups[, .SD, .SDcols = c("row_idx", "gcID")],
    subset(growthIncr, age > 0),
    by = "gcID", allow.cartesian = TRUE)[, gcID := NULL]
  data.table::setkey(growthIncrGroups, row_idx, age)


  ## Spinup ----

  spinup_input <- list(
    parameters = cohortGroups,
    increments = growthIncrGroups
  )

  mod$libcbm_default_model_config <- libcbmr::cbm_exn_get_default_parameters()
  spinup_op_seq <- libcbmr::cbm_exn_get_spinup_op_sequence()

  spinup_ops <- libcbmr::cbm_exn_spinup_ops(
    spinup_input, mod$libcbm_default_model_config
  )

  cbm_vars <- libcbmr::cbm_exn_spinup(
    spinup_input,
    spinup_ops,
    spinup_op_seq,
    mod$libcbm_default_model_config
  )

  for (i in 1:length(cbm_vars)){
    cbm_vars[[i]] <- data.table::data.table(
      row_idx = 1:nrow(cbm_vars[[i]]),
      cbm_vars[[i]],
      key = "row_idx")
  }

  # Add cohort group attributes to state table
  cohortGroups <- cohortGroups[, .SD, .SDcols = intersect(
    names(cohortGroups), c(
      #"age_in",
      setdiff(groupCols, names(cbm_vars$state)),
      "mean_annual_temperature"
    ))]
  cbm_vars$state <- cbind(cbm_vars$state, cohortGroups)

  # Return results
  cohortKey <- cohortDT[, .SD, .SDcols = intersect(
    c("cohortID", "pixelIndex", "row_idx"), names(cohortDT))]
  data.table::setkey(cohortKey, cohortID)

  c(list(key = cohortKey), cbm_vars)
}

# Helper function: read as data.table and check for required columns
readDataTable <- function(table, tableName = NULL, colRequired = NULL, copy = FALSE){

  if (is.null(table)) stop(c(tableName, "table")[[1]], " not found")

  if (!data.table::is.data.table(table)){
    table <- tryCatch(
      data.table::as.data.table(table),
      error = function(e) stop(
        c(tableName, "table")[[1]],
        " failed to be read as data.table: ", e$message,
        call. = FALSE))
  }

  if (!is.null(colRequired)){

    if (!all(colRequired %in% names(table))) stop(
      c(tableName, "table")[[1]], " missing column(s): ",
      paste(shQuote(setdiff(colRequired, names(table))), collapse = ", "))
  }

  if (copy) table <- data.table::copy(table)

  return(table)
}



