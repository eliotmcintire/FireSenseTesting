pltfn <- function(AGB, numPlots) {
  numGroups <- length(unique(AGB$yieldTableIndex))
  
  if (numPlots <= 0){
    stop("numPlots needs to be a positive integer")
  } else if (numPlots > numGroups) {
    message("numPlots is greater than the number of pixel groups, ",
            "plotting all pixelgroups.")
    numPlots <- numGroups
  }
  pullOutId <- sample(unique(AGB$yieldTableIndex), size = numPlots)
  id2 <- AGB[yieldTableIndex %in% pullOutId]
  setnames(id2, "biomass", "AGB")
  gg <- ggplot(id2, aes(age, AGB, color = speciesCode)) + geom_line() + theme_bw() +
    facet_wrap(~yieldTableIndex)
  return(invisible(gg))
}

mapGcId <- function(yieldTablesId, rasterToMatch) {
  mapRast <- rast(rasterToMatch)
  mapRast[] <- NA
  mapRast[yieldTablesId$pixelIndex] <- yieldTablesId$yieldTableIndex
  return(mapRast)
}
