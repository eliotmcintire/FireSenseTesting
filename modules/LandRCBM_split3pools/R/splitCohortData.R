splitCohortData <- function(cohortData, pixelGroupMap, standDT, table6tb, table7tb, tableMerch,
                            sppEquiv = NULL){
  # Prepare cohort data for biomass splitting-----------------------------------
  # Match pixel group with jurisdiction and CBM spatial units
  spatialDT <- data.table(
    pixelGroup = as.integer(pixelGroupMap[])
  ) 
  spatialDT <- spatialDT[, pixelIndex := .I]
  spatialDT <- merge(
    standDT,
    spatialDT) |> na.omit()
  # New pixel group for unique combination of pixelGroup and CBM spatial units
  spatialDT[, newPixelGroup := .GRP, by = .(pixelGroup, juris_id, ecozone)]
  
  # Add spatial information to cohortData
  # note that the new pixelGroup column is the unique combination of pixelGroup and CBM spatial units
  spatialUnits <- unique(spatialDT, by = "newPixelGroup")[, !("pixelIndex")]
  allInfoCohortData <- merge(cohortData, spatialUnits, by = "pixelGroup", allow.cartesian = TRUE)
  allInfoCohortData[, pixelGroup := NULL]
  
  # Add CanFI species code
  allInfoCohortData$canfi_species <- CBMutils::sppMatch(
    allInfoCohortData$speciesCode, sppEquiv = sppEquiv,
    match = "LandR", return = "CanfiCode")$CanfiCode
  
  # Convert biomass units from g/m^2 to tonnes/ha: 1 g/m^2 = 0.01 tonnes/ha
  allInfoCohortData[, B := B/100]
  
  # Split above ground biomass of current year.---------------------------------
  
  CBMutils::cumPoolsCreateAGB(
    allInfoCohortData,
    pixGroupCol = "newPixelGroup",
    bTable6tb   = table6tb,
    bTable7tb   = table7tb,
    tableMerch  = tableMerch
  )
  
  # Get pixel-level biomass data.-----------------------------------------------
  allInfoCohortData <- merge(
    spatialDT[, .(pixelIndex, newPixelGroup)],  # The pixel-pixelGroup reference
    allInfoCohortData,  # Cohort Biomass data
    by = "newPixelGroup", 
    allow.cartesian = TRUE) # There are multiple cohorts per pixelGroup and multiple pixels per pixelGroup
  
  # Only keep needed columns
  allInfoCohortData <- allInfoCohortData[, .(pixelIndex, speciesCode, age, merch, foliage, other)]
  setorderv(allInfoCohortData, c("pixelIndex", "speciesCode", "age"))
  return(allInfoCohortData)
}

