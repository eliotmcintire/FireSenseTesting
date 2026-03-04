defineModule(sim, list(
  name = "climateYear",
  description = paste("this is a very simplistic module designed to give users more control over how climate layers", 
                      "are sampled and supplied in simulations concerned with some form of NRV, ie where the simulation",
                      "length necessitates sampling climate layers instead of writing them to disk and annually retrieving them.", 
                      "It provides a measure of control over the sampling protocol used to select a given year, and ensures",
                      "consistent use of years across multiple modules when sampling is involved"),
  keywords = c(),
  authors = c(person(c("Ian", "Eddy", role = c("aut", "cre"), email = "ian.eddy@nrcan-rncan.gc.ca"))),
  childModules = character(0),
  version = list(climateYear = "0.0.0.9000"),
  timeframe = as.POSIXlt(c(NA, NA)),
  timeunit = "year",
  citation = list("citation.bib"),
  documentation = list("NEWS.md", "README.md", "climateYear.Rmd"),
  reqdPkgs = list("SpaDES.core (>= 2.1.8.9999)", "ggplot2"),
  parameters = bindrows(
    #defineParameter("paramName", "paramClass", value, min, max, "parameter description"),
    defineParameter(".studyAreaName", "character", NA, NA, NA,
                    "Human-readable name for the study area used - e.g., a hash of the study",
                    "area obtained using `reproducible::studyAreaName()`"),
    ## .seed is optional: `list('init' = 123)` will `set.seed(123)` for the `init` event only.
    defineParameter(".seed", "list", list(), NA, NA,
                    "Named list of seeds to use for each event (names)."),
    defineParameter(".useCache", "logical", FALSE, NA, NA,
                    "Should caching of events or module be used?"), 
    defineParameter("samplingEndYear", "numeric", end(sim), NA, NA, 
                    desc = paste("if randomly sampling the year for a given climate,", 
                                 "the simulation year at which to end this process, if applicable")),
    defineParameter("samplingRange", "numeric", NA, NA, NA, 
                    "Vector giving years from which to sample. The default is all years in projectedClimateRasters"),
    defineParameter("samplingStartYear", "numeric", NA, NA, NA, 
                    desc = paste("if randomly sampling the year for a given climate,", 
                                 "the simulation year at which to begin this process"))
  ),
  inputObjects = bindrows(
    #expectsInput("objectName", "objectClass", "input object description", sourceURL, ...),
    expectsInput(objectName = "projectedClimateRasters", objectClass = "list", 
                 desc = paste("a list of SpatRasters, with layers corresponding to years.",
                              "Each list element should be a different variable with corresponding names.",
                              "Each layer should be named following the convention 'year<year>`, e.g. year2009.",
                              "The object is used solely to determine the available years from which to sample"))
  ),
  outputObjects = bindrows(
    createsOutput(objectName = "climateYear", objectClass = "numeric", 
                  desc = "a year from projectedClimateRasters, updated annually"), 
    createsOutput(objectName = "climateYearRecord", objectClass = "data.table", 
                  desc = "record of which climate year was used for which simulation year")
  )
))

doEvent.climateYear = function(sim, eventTime, eventType) {
  switch(
    eventType,
    init = {
    
      # do stuff for this event
      sim <- Init(sim)

      # schedule future event(s)
      sim <- scheduleEvent(sim, start(sim), "climateYear", "getClimate")
    },
    getClimate = {
      sim$climateYear <- sampleYear(Time = time(sim), 
                                    Available = names(sim$projectedClimateRasters[[1]]),
                                    Starting = P(sim)$samplingStartYear,
                                    Ending = P(sim)$samplingEndYear,
                                    Range = P(sim)$samplingRange)
      sim$climateYearRecord <- rbind(sim$climateYearRecord, 
                                     data.table(simYear = time(sim), 
                                                climateYear = sim$climateYear))
      
      sim <- scheduleEvent(sim, time(sim) + 1, "climateYear", "getClimate")
    },
    warning(noEventWarning(sim))
  )
  return(invisible(sim))
}

### template initialization
Init <- function(sim) {

 #make climateYearRecord
 sim$climateYearRecord <- data.table(simYear = numeric(0), climateYear = numeric(0)) 
 
 return(invisible(sim))
}
### template for save events
Save <- function(sim) {
  # ! ----- EDIT BELOW ----- ! #
  # do stuff for this event
  sim <- saveFiles(sim)

  # ! ----- STOP EDITING ----- ! #
  return(invisible(sim))
}

sampleYear <- function(Range, Starting, Ending, Time, Available) {
  Available <- as.numeric(gsub("[^0-9]", "", Range))
  Range <- Range[Range %in% Available]
  if (!is.na(Starting)){
    if (Starting <= Time & Time <= Ending) {
      theYear <- sample(Range, size = 1)
    } else if (Time %in% Available) {
      theYear <- Time
    } else {
      stop("climateYear does not have any available years?")
    }
  } else {
    theYear <- Time
  }
  
  return(theYear)
}

.inputObjects <- function(sim) {
 
  #cacheTags <- c(currentModule(sim), "function:.inputObjects") ## uncomment this if Cache is being used
  dPath <- asPath(getOption("reproducible.destinationPath", dataPath(sim)), 1)
  message(currentModule(sim), ": using dataPath '", dPath, "'.")

  if (!suppliedElsewhere("projectedClimateRasters", sim)) {
    Range <- start(sim):end(sim)
    sim$projectedClimateRasters <- list("fooVar" = terra::rast(nlyrs = Range))
    names(sim$projectedClimateRasters[[1]]) <- paste0("year",Range)
  }
  
  return(invisible(sim))
}
