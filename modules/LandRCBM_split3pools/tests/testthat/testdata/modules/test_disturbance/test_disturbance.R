
defineModule(sim, list(
  name = "test_disturbance",
  description = "Test module mimicking **Biomass_regeneration** to simulate cohort disturbance in `cohortData`",
  timeunit = "year",
  reqdPkgs = list("data.table"),
  inputObjects = bindrows(
    expectsInput(objectName = "cohortData", objectClass = "data.table", desc = NA, sourceURL = NA),
    expectsInput(objectName = "disturbanceEvents", objectClass = "data.table", desc = NA, sourceURL = NA)
  ),
  outputObjects = bindrows(
    createsOutput(objectName = "cohortData", objectClass = "data.table", desc = NA)
  )
))

doEvent.test_disturbance = function(sim, eventTime, eventType) {
  switch(
    eventType,
    
    init = {
      sim <- scheduleEvent(sim, start(sim), "test_disturbance", "disturbance", eventPriority = 6.5)
    },
    
    disturbance = {
      
      # Remove cohorts from disturbed pixels
      ## Note that this would be done using input rstCurrentBurn
      distEvents <- sim$disturbanceEvents[year == time(sim)]
      sim$cohortData <- sim$cohortData[!pixelGroup %in% distEvents$pixelIndex]
      
      sim <- scheduleEvent(sim, time(sim) + 1, "test_disturbance", "disturbance", eventPriority = 6.5)
      
    },
    warning(noEventWarning(sim))
  )
  return(invisible(sim))
}

.inputObjects <- function(sim) return(invisible(sim))

