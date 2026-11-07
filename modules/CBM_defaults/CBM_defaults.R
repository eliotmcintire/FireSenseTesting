defineModule(sim, list(
  name = "CBM_defaults",
  description = "Provides CBM-CFS3 defaults for Canada",
  keywords = c("CBM-CFS3", "forest carbon", "Canada parameters"),
  authors = c(
    person("Céline",  "Boisvenue", email = "celine.boisvenue@nrcan-rncan.gc.ca", role = c("aut", "cre")),
    person("Camille", "Giuliano",  email = "camsgiu@gmail.com",                  role = c("ctb")),
    person("Susan",   "Murray",    email = "murray.e.susan@gmail.com",           role = c("ctb"))
  ),
  childModules = character(0),
  version = list(CBM_defaults = "1.0.0.9000"),
  timeframe = as.POSIXlt(c(NA, NA)),
  timeunit = "year",
  citation = list("citation.bib"),
  documentation = list("README.txt", "CBM_defaults.Rmd"),
  reqdPkgs = list("reproducible", "data.table", "DBI", "RSQLite"),
  parameters = bindrows(
    defineParameter("cbm_defaults_db_version", "character", "1.2.9300.391", NA, NA,
                    desc = "DBM defaults SQLite database version"),
    defineParameter(".useCache", "logical", FALSE, NA, NA, NA)
  ),
  inputObjects = bindrows(
    expectsInput(
      objectName = "libcbm_resources", objectClass = "character", desc = "Path to libcbm resources directory",
      sourceURL = "https://raw.githubusercontent.com/cat-cfs/libcbm_py/main/libcbm/resources")
  ),
  outputObjects = bindrows(
    createsOutput(objectName = "cbm_defaults_db", objectClass = "character",
                  desc = "Path to CBM defaults SQLite database in the project outputs directory"),
    createsOutput(objectName = "cbm_exn_dir",     objectClass = "character",
                  desc = "Path to CBM-EXN parameters directory in the project outputs directory"),
    createsOutput(objectName = "CBMspecies",        objectClass = "data.table",
                  desc = "Species table with names and associated species id"),
    createsOutput(objectName = "disturbanceMatrix", objectClass = "data.table",
                  desc = "Disturbance table with disturbance names associated with disturbance_matrix_id, disturbance_type_id"),
    createsOutput(objectName = "cTransfers",        objectClass = "data.table",
                  desc = "Carbon transfer values table with associated disturbance names and IDs"),
    createsOutput(objectName = "spinupSQL",         objectClass = "data.table",
                  desc = "Table containing many necesary spinup parameters used in CBM_core")
  )
))

doEvent.CBM_defaults <- function(sim, eventTime, eventType, debug = FALSE) {
  switch(
    eventType,
    init = {
      sim <- Init(sim)
    },
    warning(noEventWarning(sim))
  )
  return(invisible(sim))
}

Init <- function(sim) {

  # Copy CBM resources to outputs directory
  resPath <- file.path(outputPath(sim), "CBM_defaults", "resources")
  if (file.exists(resPath)) unlink(resPath, recursive = TRUE)
  if (file.exists(resPath)) stop("Could not remove CBM_defaults resources directory: ", resPath)

  if (is.null(sim$cbm_defaults_db)){

    dbPath <- paste0("cbm_defaults_db/cbm_defaults_v", P(sim)$cbm_defaults_db_version, ".db")

    sim$cbm_defaults_db <- file.path(resPath, dbPath)
    dir.create(dirname(sim$cbm_defaults_db), recursive = TRUE, showWarnings = FALSE)
    file.copy(file.path(sim$libcbm_resources, dbPath), sim$cbm_defaults_db)
  }

  if (is.null(sim$cbm_exn_dir)){

    exnPath <- "cbm_exn"

    sim$cbm_exn_dir <- file.path(resPath, exnPath)
    dir.create(dirname(sim$cbm_exn_dir), recursive = TRUE, showWarnings = FALSE)
    file.copy(file.path(sim$libcbm_resources, exnPath), dirname(sim$cbm_exn_dir), recursive = TRUE)
  }

  # Connect to database
  archiveIndex <- dbConnect(dbDriver("SQLite"), sim$cbm_defaults_db)
  on.exit(dbDisconnect(archiveIndex))

  # dbListTables(archiveIndex)
  # [1] "admin_boundary"                       "admin_boundary_tr"
  # [3] "afforestation_initial_pool"           "afforestation_pre_type"
  # [5] "afforestation_pre_type_tr"            "biomass_to_carbon_rate"
  # [7] "composite_flux_indicator"             "composite_flux_indicator_category"
  # [9] "composite_flux_indicator_category_tr" "composite_flux_indicator_tr"
  # [11] "composite_flux_indicator_value"       "decay_parameter"
  # [13] "disturbance_matrix"                   "disturbance_matrix_association"
  # [15] "disturbance_matrix_tr"                "disturbance_matrix_value"
  # [17] "disturbance_type"                     "disturbance_type_tr"
  # [19] "dom_pool"                             "eco_boundary"
  # [21] "eco_boundary_tr"                      "flux_indicator"
  # [23] "flux_indicator_sink"                  "flux_indicator_source"
  # [25] "flux_process"                         "forest_type"
  # [27] "forest_type_tr"                       "genus"
  # [29] "genus_tr"                             "growth_multiplier_series"
  # [31] "growth_multiplier_value"              "land_class"
  # [33] "land_class_tr"                        "land_type"
  # [35] "locale"                               "pool"
  # [37] "pool_tr"                              "random_return_interval"
  # [39] "root_parameter"                       "slow_mixing_rate"
  # [41] "spatial_unit"                         "species"
  # [43] "species_tr"                           "spinup_parameter"
  # [45] "stump_parameter"                      "turnover_parameter"
  # [47] "vol_to_bio_factor"                    "vol_to_bio_forest_type"
  # [49] "vol_to_bio_genus"                     "vol_to_bio_species"

  # This table, matrices6, has all the names associated with
  # disturbance_type_id. disturbance_type_id, along with spatial_unit_id and a
  # sw_hw flag is how libcbm connects (internally) to the proportions. In the
  # libcbm stepping function libcbmr::cbm_exn_step(), disturbance_type_id is in
  # cbm_vars$parameters$disturbance_type column and in the libcbm spinup
  # function libcbmr::cbm_exn_spinup(), the disturbance_type_id is passed in the
  # historical/lastpass values.
  matrices6 <- as.data.table(dbGetQuery(archiveIndex, "SELECT * FROM disturbance_type_tr"))
  # matrices6 has French, Spanish and Russian disturbance translations, here we
  # only keep one copy in English.
  matrices6 <- matrices6[locale_id <= 1,]
  # only keep the columns we need, disturbance_type_id and its associated name.
  matrices6 <- matrices6[,.(disturbance_type_id, name, description)]

  # $disturbanceMatrix links together spatial_unit_id disturbance_type_id
  # disturbance_matrix_id, the disturbance names and descriptions.
  dMatrixAssociation <- data.table::fread(file.path(sim$cbm_exn_dir, "disturbance_matrix_association.csv"))
  disturbanceMatrix <- dMatrixAssociation[matrices6, on = .(disturbance_type_id = disturbance_type_id), allow.cartesian = TRUE]
  sim$disturbanceMatrix <- disturbanceMatrix

  # here we want to match sim$dMatrixValue with disturbanceMatrix so that sim$CTransfers has disturbance names.
  # CTransfers will have to be subset to the regions' spatial_unit_id.
  dMatrixValue <- data.table::fread(file.path(sim$cbm_exn_dir, "disturbance_matrix_value.csv"))
  disturbanceNames <- unique(disturbanceMatrix[, .(disturbance_type_id, spatial_unit_id, sw_hw, disturbance_matrix_id, name, description)])
  cTransfers <- dMatrixValue[disturbanceNames, on = .(disturbance_matrix_id = disturbance_matrix_id), allow.cartesian=TRUE]
  sim$cTransfers <- cTransfers

  # Get location and mean_annual_temperature for each spatial_unit, along with
  # other spinup parameters. These are used in the CBM_core to create the
  # spinup_parameters.
  spatialUnitIds <- as.data.table(dbGetQuery(archiveIndex, "SELECT * FROM spatial_unit"))
  spinupParameter <-  as.data.table(dbGetQuery(archiveIndex, "SELECT * FROM spinup_parameter"))
  #create $spinupSQL for use in other modules
  sim$spinupSQL <- spatialUnitIds[spinupParameter, on = .(spinup_parameter_id = id)]

  #extract species.csv
  sim$CBMspecies <- data.table::fread(file.path(sim$cbm_exn_dir, "species.csv"))

  # Return simList
  return(invisible(sim))
}

.inputObjects <- function(sim) {

  if (!suppliedElsewhere("libcbm_resources", sim)){

    sim$libcbm_resources <- file.path(inputPath(sim), "libcbm_resources")

    dlFiles <- c(
      paste0("cbm_defaults_db/cbm_defaults_v", P(sim)$cbm_defaults_db_version, ".db"),
      "cbm_exn/decay_parameters.csv",
      "cbm_exn/disturbance_matrix_association.csv",
      "cbm_exn/disturbance_matrix_value.csv",
      "cbm_exn/root_parameters.csv",
      "cbm_exn/slow_mixing_rate.csv",
      "cbm_exn/species.csv",
      "cbm_exn/turnover_parameters.csv",
      "cbm_exn/flux.json",
      "cbm_exn/pools.json"
    )

    for (dlFile in dlFiles){
      dlURL  <- file.path(extractURL("libcbm_resources"), dlFile)
      dlPath <- file.path(sim$libcbm_resources, dlFile)
      dir.create(dirname(dlPath), recursive = TRUE, showWarnings = FALSE)
      prepInputs(
        destinationPath = dirname(dlPath),
        url         = dlURL,
        targetFile  = basename(dlPath),
        dlFun       = download.file(dlURL, dlPath, mode = "wb", quiet = TRUE),
        fun         = NA
      )
    }
  }

  # Return sim
  return(invisible(sim))

}
