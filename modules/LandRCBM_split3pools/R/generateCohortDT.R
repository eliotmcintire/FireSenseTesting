generateCohortDT <- function(cohortData, pixelGroupMap, standDT, yieldTablesId){
  
  # get the pixelGroup for each pixelIndex
  # `na.omit` removes pixels not forested.
  cohortDT <- data.table(
    pixelGroup = as.integer(pixelGroupMap[])
  ) 
  cohortDT <- cohortDT[, pixelIndex := .I] |> na.omit()
  
  # add cohort information for each pixelIndex
  cohortDT <- merge(cohortDT,
                    cohortData[age > 0,.(speciesCode, age, pixelGroup)],
                    by = "pixelGroup",
                    allow.cartesian = TRUE)
  
  if (!is.null(yieldTablesId)){
    # add the yield table index
    # Merge yield table index into cohortDT
    cohortDT <- merge(cohortDT, yieldTablesId, by = "pixelIndex", all.x = TRUE)
    
    # Get the species combinations per yieldTableIndex from the original data
    species_combos <- unique(cohortDT[, .(yieldTableIndex, speciesCode)])
    
    # Find pixels in yieldTablesId that are not in original cohortDT
    missing_pixels <- setdiff(yieldTablesId[yieldTableIndex != 0, pixelIndex], unique(cohortDT$pixelIndex))
    
    if (length(missing_pixels) > 0) {
      # For each missing pixel, get its yieldTableIndex
      missing_info <- yieldTablesId[pixelIndex %in% missing_pixels]
      
      # Join with species combinations for that yieldTableIndex
      new_rows <- merge(missing_info, species_combos, by = "yieldTableIndex", allow.cartesian = TRUE)
      
      # Add age = 0
      new_rows[, age := 0L]
      
      # Bind to cohortDT
      cohortDT <- rbindlist(list(cohortDT, new_rows), use.names = TRUE, fill = TRUE)
    }
  } else {
    stop("yieldTablesId needs to cannot be NULL")
  }
  
  # Create a new, unique ID for each unique combination of original yield 
  # table index and CBM spatial units. This handles cases
  # where one yield curve spans multiple ecozones/jurisdictions.
  cohortDT <- merge(cohortDT, standDT[, .(pixelIndex, admin_abbrev, eco_id)], by = "pixelIndex")
  setcolorder(cohortDT, c("pixelIndex", "speciesCode", "age"))
  cohortDT[, gcID := .GRP, by = .(admin_abbrev, eco_id, yieldTableIndex, speciesCode)]
  
  # add the index for individual cohorts
  cohortDT[, cohortID := .I]
  setkey(cohortDT, cohortID)
  
  # Keep final columns in desired order
  cohortDT <- cohortDT[, .(cohortID, pixelIndex, admin_abbrev, eco_id, speciesCode, age, yieldTableIndex, gcID)]
  
  # Ensure cohort generation worked
  if (is.null(cohortDT) || nrow(cohortDT) == 0) {
    stop("generateCohortDT failed.")
  }
  
  return(cohortDT)
}
