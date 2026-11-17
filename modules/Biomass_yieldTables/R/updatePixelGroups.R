updatePixelGroups <- function(
    cohortData, 
    pixelGroupDefinitionCols = c("speciesCode", "age", "B"),
    returnRefTable = TRUE) {
  dt <- Copy(cohortData)
  # Create a species composition signature for each (ecoregionGroup, pixelGroup)
  new_groups <- dt[, .(pixelGroupSignature = paste(.SD, collapse = ",")), 
                           by = .(ecoregionGroup, pixelGroup), 
                           .SDcols = pixelGroupDefinitionCols]
  
  # Assign new pixelGroups based on (ecoregionGroup, pixelGroupSignature)
  new_groups[, pixelGroupYield := .GRP, by = .(ecoregionGroup, pixelGroupSignature)]
  
  # Keep only the mapping columns
  new_groups <- new_groups[, .(pixelGroup, pixelGroupYield)]
  
  # Merge back to original cohortData
  out <- merge(dt, new_groups, by = "pixelGroup")

  # Remove old pixelGroup and rename newPixelGroup
  out[, pixelGroup := NULL]
  setnames(out, "pixelGroupYield", "pixelGroup")
  out <- unique(out[, .(speciesCode, ecoregionGroup, age, B, pixelGroup)], by = c("pixelGroup", "speciesCode", "age", "B"))
  
  out <- setorder(out, ecoregionGroup, pixelGroup, speciesCode, age) |>
    setcolorder(c("ecoregionGroup", "pixelGroup", "speciesCode", "age", "B"))
  if (returnRefTable) {
    return(list(cohortData = out, pixelGroupRef = new_groups))
  } else {
    return(out)
  }
}

