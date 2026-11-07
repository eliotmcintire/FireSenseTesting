
#' CBM-EXN Step
cbmEXN_step <- function(cbm_vars, cbm_defaults_db = NULL, cbm_exn_dir = NULL){

  # Set resource paths
  if (!is.null(cbm_defaults_db)) withr::local_options(list(
    libcbmr.cbm_defaults_path = cbm_defaults_db
  ))
  if (!is.null(cbm_exn_dir)) withr::local_options(list(
    libcbmr.cbm_exn_parameters_dir = cbm_exn_dir
  ))

  # Set spatial_unit_id and mean_annual_temperature
  if (!all(c("spatial_unit_id", "mean_annual_temperature") %in% names(cbm_vars$state)) |
      !"mean_annual_temperature" %in% names(cbm_vars$parameters)){

    cbmDBcon <- RSQLite::dbConnect(RSQLite::dbDriver("SQLite"), libcbmr::get_cbm_defaults_path())
    spuMeta <- data.table::as.data.table(merge(
      RSQLite::dbReadTable(cbmDBcon, "spatial_unit"),
      RSQLite::dbReadTable(cbmDBcon, "admin_boundary_tr"),
      by = "admin_boundary_id"))[locale_id == 1, .(
        spatial_unit_id = id.x, admin_name = name, eco_id = eco_boundary_id, mean_annual_temperature)]
    RSQLite::dbDisconnect(cbmDBcon)

    if (!"spatial_unit_id" %in% names(cbm_vars$state)){
      if (!all(c("admin_name", "eco_id") %in% names(cbm_vars$state))) stop(
        "cbm_vars$state must have either 'spatial_unit_id' or 'admin_name' and 'eco_id' columns")
      cbm_vars$state <- cbm_vars$state[spuMeta, spatial_unit_id := spatial_unit_id, on = c("admin_name", "eco_id")]
    }

    spuMeta <- merge(cbm_vars$state[, .(row_idx, spatial_unit_id)], spuMeta, by = "spatial_unit_id", all.x = TRUE)
    data.table::setkey(spuMeta, row_idx)

    if (!"mean_annual_temperature" %in% names(cbm_vars$state)){
      cbm_vars$state[, mean_annual_temperature := spuMeta$mean_annual_temperature]
    }
    if (!"mean_annual_temperature" %in% names(cbm_vars$parameters)){
      cbm_vars$parameters[, mean_annual_temperature := spuMeta$mean_annual_temperature]
    }
  }

  # Prepare sw_hw column for Python
  if ("sw" %in% names(cbm_vars$state) & !"sw_hw" %in% names(cbm_vars$state)){
    cbm_vars$state[, sw_hw := as.integer(!sw)]
    cbm_vars$state[, sw    := NULL]
  }

  # Set species ID
  if (!"species" %in% names(cbm_vars$state)){
    speciesCBM <- data.table::fread(file.path(libcbmr::get_cbm_exn_parameters_dir(), "species.csv"))
    speciesIDs <- c(
      sw = speciesCBM[species_name == "Unspecified softwood species", species_id],
      hw = speciesCBM[species_name == "Unspecified hardwood species", species_id]
    )
    cbm_vars$state[, species := data.table::fifelse(sw_hw == 0L, speciesIDs[["sw"]], speciesIDs[["hw"]], 0L)]
  }

  # Set sw_hw flag where increments==0
  if (anyNA(cbm_vars$state$sw_hw)){
    if (any(cbm_vars$parameters[is.na(cbm_vars$state$sw_hw), .(merch_inc, foliage_inc, other_inc)] != 0)){
      stop("Cohorts with increments != 0 must have the cbm_vars$state$sw_hw flag set")
    }
    cbm_vars$state[is.na(sw_hw), sw_hw := 0L]
  }

  # Temporarily remove row_idx column
  row_idx <- cbm_vars$parameters$row_idx
  for (i in 2:length(cbm_vars)) cbm_vars[[i]][, row_idx := NULL]

  # Call Python
  mod$libcbm_default_model_config <- libcbmr::cbm_exn_get_default_parameters()
  step_ops <- libcbmr::cbm_exn_step_ops(cbm_vars, mod$libcbm_default_model_config)

  cbm_vars[-1] <- libcbmr::cbm_exn_step(
    cbm_vars[-1],
    step_ops,
    libcbmr::cbm_exn_get_step_disturbance_ops_sequence(),
    libcbmr::cbm_exn_get_step_ops_sequence(),
    mod$libcbm_default_model_config
  )

  # Implement delay
  delayRows <- with(cbm_vars$state, is.na(time_since_last_disturbance) | time_since_last_disturbance <= delay)
  if (any(delayRows)) {
    cbm_vars$state$age[delayRows] <- 0
    delayGrowth <- c("age", "merch_inc", "foliage_inc", "other_inc")
    cbm_vars$parameters[delayRows, delayGrowth] <- 0
  }

  # Prepare output data for next annual event
  for (i in 2:length(cbm_vars)){
    cbm_vars[[i]] <- data.table::data.table(row_idx = row_idx, cbm_vars[[i]], key = "row_idx")
  }

  return(cbm_vars)
}



