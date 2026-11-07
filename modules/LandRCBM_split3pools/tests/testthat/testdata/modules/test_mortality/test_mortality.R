
defineModule(sim, list(
  name = "test_mortality",
  description = "Test module mimicking **Biomass_core** to simulate cohort mortality in `cohortData`",
  timeunit = "year",
  reqdPkgs = list("data.table"),
  inputObjects = bindrows(
    expectsInput(objectName = "cohortData", objectClass = "data.table", desc = NA, sourceURL = NA),
    expectsInput(objectName = "cohortMortality", objectClass = "data.table", desc = NA, sourceURL = NA)
  ),
  outputObjects = bindrows(
    createsOutput(objectName = "cohortData", objectClass = "data.table", desc = NA)
  )
))

doEvent.test_mortality = function(sim, eventTime, eventType) {
  switch(
    eventType,
    
    init = {
      sim <- scheduleEvent(sim, start(sim), "test_mortality", "mortality", eventPriority = 6.5)
    },
    
    mortality = {
      
      if (time(sim) %in% sim$cohortMortality$year){
        
        m <- sim$cohortMortality[year == time(sim)]
        if (!"speciesCode" %in% names(m)) m[, speciesCode := NA]
        
        sim$cohortData <- sim$cohortData[!(pixelGroup %in% m[is.na(speciesCode), pixelGroup])]
        
        for (i in which(!is.na(m$speciesCode))){
          sim$cohortData <- sim$cohortData[!(pixelGroup %in% m[i, pixelGroup] & speciesCode %in% m[i, speciesCode])]
        }
      }
      
      sim <- scheduleEvent(sim, time(sim) + 1, "test_mortality", "mortality", eventPriority = 6.5)
      
    },
    warning(noEventWarning(sim))
  )
  return(invisible(sim))
}

.inputObjects <- function(sim) return(invisible(sim))

