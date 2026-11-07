getSpatialInformation <- function(newCohorts_cbm_state, cbm_key, standDT){
  
  # get the admin_id and eco_id of the new cohorts
  newCohortMeta <- merge(data.table(row_idx = newCohorts_cbm_state$row_idx), cbm_key[,.(row_idx, pixelIndex)], by = "row_idx")
  newCohortMeta <- merge(newCohortMeta, standDT, by = "pixelIndex")
  
  # get the spatial_unit_id
  newCohortMeta <- unique(newCohortMeta[,.(row_idx, admin_name, eco_id)])
  
  # There should be only 1 spatial unit id per cohortgroup
  if(any(duplicated(newCohortMeta$row_idx)) | nrow(newCohorts_cbm_state) != nrow(newCohortMeta)){
    stop("Error in assigning spatial unit to cohort group.")
  }
  newCohorts_cbm_state[, admin_name := NULL]
  newCohorts_cbm_state[, eco_id := NULL]
  
  newCohorts_cbm_state <- merge(newCohorts_cbm_state, newCohortMeta, by = "row_idx")
  
  return(newCohorts_cbm_state)
}
