
# Helper function: check structure of output objects
# @param spinup logical. Checks match expected state after the spinup.
check_module_outputs <- function(simTest, spinup = FALSE){
  
  # abovegroundbiomass
  expect_is(simTest$aboveGroundBiomass, "data.table")
  expect_in(c("pixelIndex", "speciesCode", "age", "merch", "foliage", "other"), 
            names(simTest$aboveGroundBiomass))
  
  ## check that total biomass per species match cohortData
  expectedSpeciesB <- simTest$cohortData[, .(total_biomass = sum(B)/200), by = speciesCode]
  resultSpeciesB <- copy(simTest$aboveGroundBiomass)[, B := merch + foliage + other]
  resultSpeciesB <- resultSpeciesB[, .(total_biomass = sum(B)), by = speciesCode]
  expect_equal(expectedSpeciesB[order(speciesCode)], resultSpeciesB[order(speciesCode)])
  
  # gcMeta
  if (spinup){
    expect_in(c("gcID", "admin_abbrev", "eco_id", "speciesCode", "canfi_species", "sw"),
              names(simTest$gcMeta))
    expect_in("yieldTableIndex", names(simTest$gcMeta))
    expect_true(all(simTest$gcMeta$yieldTableIndex %in% simTest$yieldTablesId$yieldTableIndex))
  }else{
    expect_in(c("gcID", "speciesCode", "sw"),
              names(simTest$gcMeta))
  }
  
  # gcIncrements
  expect_is(simTest$gcIncrements, "data.table")
  expect_in(
    c("gcID", "age", "merch_inc", "foliage_inc", "other_inc"),
    names(simTest$gcIncrements))
  expect_setequal(simTest$gcIncrements$gcID, simTest$gcMeta$gcID)
  
  if (spinup){
    expect_equal(nrow(simTest$gcIncrements), nrow(simTest$yieldTablesCumulative))
  }else{
    expect_setequal(simTest$gcIncrements$gcID, simTest$gcMeta$gcID)
    expect_equal(nrow(simTest$gcIncrements), nrow(simTest$gcMeta))
  }
  
  # cohortDT
  expect_is(simTest$cohortDT, "data.table")
  expect_in(c("cohortID", "pixelIndex", "age", "speciesCode", "gcID"),
            names(simTest$cohortDT))
  
  expect_true(all(simTest$cohortDT$gcID %in% simTest$gcMeta$gcID))
  
  # summaryAGB
  if (!spinup){
    
    expect_is(simTest$summaryAGB, "data.table")
    expect_in(
      c("speciesCode", "merch", "foliage", "other", "year"),
      names(simTest$summaryAGB))
    expect_equal(simTest$summaryAGB$year, do.call(c, lapply(start(simTest):end(simTest), rep, 2)))
  }
  
  # cbm_vars
  if ("CBM_core" %in% modules(simTest)){
    check_cbm_vars(simTest)
  }
}


# Helper function: check cbm_vars
check_cbm_vars <- function(simTest){
  
  expect_is(simTest$cbm_vars, "list")
  expect_setequal(names(simTest$cbm_vars), c("key", "parameters", "state", "pools", "flux"))
  expect_equal(data.table::key(simTest$cbm_vars$key), "cohortID")
  for (table in c("parameters", "state", "pools", "flux")){
    expect_equal(data.table::key(simTest$cbm_vars[[table]]), "row_idx")
  }
  
  expect_is(simTest$cbm_vars$key, "data.table")
  expect_in(c("cohortID", "pixelIndex", "row_idx"), names(simTest$cbm_vars$key))
  
  # check table row counts
  NcohortGroups <- length(unique(simTest$cbm_vars$key$row_idx))
  expect_equal(nrow(simTest$cbm_vars$parameters), NcohortGroups)
  expect_equal(nrow(simTest$cbm_vars$state),      NcohortGroups)
  expect_equal(nrow(simTest$cbm_vars$pools),      NcohortGroups)
  expect_equal(nrow(simTest$cbm_vars$flux),       NcohortGroups)
  
  # checks for "active" cohorts
  row_idx_active <- simTest$cbm_vars$state[gcID != 0, row_idx]
  if ("disturbance_type_id" %in% names(simTest$cbm_vars$key)){
    row_idx_active <- intersect(row_idx_active, simTest$cbm_vars$key[is.na(disturbance_type_id), row_idx])
  }
  ActiveCohortGroups <- simTest$cbm_vars$key[row_idx %in% row_idx_active, row_idx]
  
  expect_equal(
    simTest$cbm_vars$state[ActiveCohortGroups, age],
    simTest$aboveGroundBiomass$age
  )
  expect_equal(
    simTest$cbm_vars$pools[ActiveCohortGroups, .(Merch, Foliage, Other)],
    simTest$aboveGroundBiomass[,.(Merch = merch, Foliage = foliage, Other = other)]
  )

  # checks for DOM cohorts
  DOMCohortGroups <- simTest$cbm_vars$key[!row_idx %in% row_idx_active, row_idx]

  ## DOM cohort groups have 0 above ground biomass
  expect_true(
    all(round(simTest$cbm_vars$pools[DOMCohortGroups, .(Merch, Foliage, Other)], 10^-12) == 0)
  )

  ## There can't be more than 1 DOM cohort groups per pixel
  expect_equal(
    length(DOMCohortGroups),
    nrow(simTest$cbm_vars$key[row_idx %in% DOMCohortGroups])
  )
}

