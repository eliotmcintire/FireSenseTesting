updateSpinupCohortGroups <- function(spinupOut, key){

  # Step 1: Combine pools and state columns into a single data.table
  combinedOutputs <- cbind(
    spinupOut$pools,
    spinupOut$state[, c("spatial_unit_id", "age", "speciesCode")]
  ) |> as.data.table()
  
  # Step 2: Create a unique cohort group ID for identical rows
  combinedOutputs[, cohortGroupID := .GRP, by = names(combinedOutputs)]
  
  # Step 3: Update the cohortGroupID in the key table
  key$row_idx <- combinedOutputs$cohortGroupID
  
  # Step 4: Update spinup outputs
  spinupOut <- lapply(spinupOut, function(tbl){
    tbl <- as.data.table(tbl)
    tbl[, row_idx := combinedOutputs$cohortGroupID]
    tbl <- unique(tbl, by = "row_idx")
    data.table::setkey(tbl, row_idx)
    tbl
  })
  
  return(c(spinupOut, list(key = key)))
}