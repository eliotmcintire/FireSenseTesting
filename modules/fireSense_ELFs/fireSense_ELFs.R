## Everything in this file and any files in the R directory are sourced during `simInit()`;
## all functions and objects are put into the `simList`.
## To use objects, use `sim$xxx` (they are globally available to all modules).
## Functions can be used inside any function that was sourced in this module;
## they are namespaced to the module, just like functions in R packages.
## If exact location is required, functions will be: `sim$.mods$<moduleName>$FunctionName`.
defineModule(sim, list(
  name = "fireSense_ELFs",
  description = "Create ELFs for fireSense family of modules",
  keywords = "",
  authors = structure(list(list(given = c("First", "Middle"), family = "Last", role = c("aut", "cre"), email = "email@example.com", comment = NULL)), class = "person"),
  childModules = character(0),
  version = list(fireSense_ELFs = "0.0.0.9000"),
  timeframe = as.POSIXlt(c(NA, NA)),
  timeunit = "year",
  citation = list("citation.bib"),
  documentation = list("NEWS.md", "README.md", "fireSense_ELFs.Rmd"),
  reqdPkgs = list("SpaDES.core (>= 3.0.1)", "terra", "reproducible (>=3.0.0)",
                  "PredictiveEcology/scfmutils@development",
                  "PredictiveEcology/fireSenseUtils@development (>= 0.0.6.9008)",
                  "PredictiveEcology/SpaDES.project@development (>= 0.1.4)"),
  parameters = bindrows(
    #defineParameter("paramName", "paramClass", value, min, max, "parameter description"),
    defineParameter(".plots", "character", "screen", NA, NA,
                    "Used by Plots function, which can be optionally used here"),
    defineParameter(".plotInitialTime", "numeric", start(sim), NA, NA,
                    "Describes the simulation time at which the first plot event should occur."),
    defineParameter(".plotInterval", "numeric", NA, NA, NA,
                    "Describes the simulation time interval between plot events."),
    defineParameter(".saveInitialTime", "numeric", NA, NA, NA,
                    "Describes the simulation time at which the first save event should occur."),
    defineParameter(".saveInterval", "numeric", NA, NA, NA,
                    "This describes the simulation time interval between save events."),
    defineParameter(".studyAreaName", "character", NA, NA, NA,
                    "Human-readable name for the study area used - e.g., a hash of the study",
                          "area obtained using `reproducible::studyAreaName()`"),
    ## .seed is optional: `list('init' = 123)` will `set.seed(123)` for the `init` event only.
    defineParameter(".seed", "list", list(), NA, NA,
                    "Named list of seeds to use for each event (names)."),
    defineParameter(".useCache", "logical", FALSE, NA, NA,
                    "Should caching of events or module be used?")
  ),
  inputObjects = bindrows(
    #expectsInput("objectName", "objectClass", "input object description", sourceURL, ...),
    expectsInput(".runName", "character", "Some descriptive, short name for this fitting, e.g., ELF14.1")
  ),
  outputObjects = bindrows(
    #createsOutput("objectName", "objectClass", "output object description", ...),
    createsOutput("rastTemplate", objectClass = "SpatRaster", desc = NA),
    createsOutput("homogeneousFire", objectClass = "SpatRaster", desc = NA),
    createsOutput("ELFs", objectClass = "SpatRaster", desc = NA),
    createsOutput("rasterToMatchLarge", objectClass = "SpatRaster", desc = NA),
    createsOutput("rasterToMatch", objectClass = "SpatRaster", desc = NA),
    createsOutput("studyArea", objectClass = "SpatVector", desc = NA),
    createsOutput("studyAreaELF", objectClass = "SpatVector", desc = NA),
    createsOutput("studyAreaReporting", objectClass = "SpatVector", desc = NA),
    createsOutput("sppEquiv", objectClass = "data.table", desc = NA),
    createsOutput("studyAreaPSP", objectClass = "SpatVector", desc = NA)
  )
))

doEvent.fireSense_ELFs = function(sim, eventTime, eventType) {
  switch(
    eventType,
    init = {
      ### check for more detailed object dependencies:
      ### (use `checkObject` or similar)

      # do stuff for this event
      sim <- Init(sim)

      # schedule future event(s)
      # sim <- scheduleEvent(sim, P(sim)$.plotInitialTime, "fireSense_ELFs", "plot")
      # sim <- scheduleEvent(sim, P(sim)$.saveInitialTime, "fireSense_ELFs", "save")
    },
    plot = {
      plotFun(sim) # example of a plotting function
    },
    save = {},
    warning(noEventWarning(sim))
  )
  return(invisible(sim))
}

### template initialization
Init <- function(sim) {
  inputPath <- inputPath(sim)
  ELF <- sim$.runName
  # ll <- list(
  rastTemplate <- {
    # check the hash once per week
    templateURL <- "https://ftp.maps.canada.ca/pub/nrcan_rncan/Forests_Foret/SCANFI/v1/SCANFI_sps_douglasFir_SW_2020_v1.2.tif"
    hash <- reproducible:::getRemoteMetadata(isGDurl = FALSE, url = templateURL) |>
      Cache(notOlderThan = Sys.time() - 60*60*24*7)
    out <- {
      prepInputs(url = templateURL, destinationPath = inputPath) |>
        (\(x) {message("Aggregating to 240m..."); x})() |>
        terra::aggregate(fact = 8, filename = file.path(inputPath, "rastTemplate_Canada.tif"),
                         overwrite = TRUE)} |>
      Cache(omitArgs = "x", .cacheExtra = list(hash$remoteHash),
            # dryRun = TRUE,
            .functionName = "rasterTemplate_aggregate")
    out
  }
  homogeneousFire <- {
    # bbbb <<- 1#; on.exit(rm(bbbb, envir = .GlobalEnv))
    {
      scfmutils::prepInputsFireRegimePolys(type = "FRU", destinationPath = inputPath) |>
        reproducible::Cache(cacheSaveFormat = "rds")
    }}
  ELFs <- {
    # fireSenseUtils::makeELFs(homogeneousFire, desiredBuffer = 20000, destinationPath = inputPath) |>
    fireSenseUtils::makeELFs(rastTemplate, desiredBuffer = 20000, destinationPath = inputPath) |>
      Cache(omitArgs = "nationalForestPolygon",
            .cacheExtra = list(rt = attr(rastTemplate, "tags"),
                               bufferOutFn = fireSenseUtils:::bufferOut))
  }
  
  rasterToMatchLarge <- {
    rtml <- ELFs$rasWhole[[ELF]]
    if (identical(1, terra::freq(is.na(rtml))$value))
      stop("This ELF has no data")
    rtml[rtml[] == 0] <- NA
    {
      postProcess(rtml, projectTo = rastTemplate, method = "near",
                  writeTo = file.path(inputPath, paste0("rtml_", ELF,".tif"))) |>
        terra::trim() } |>
      Cache(omitArgs = c("x"),
            .functionName = paste0("rasterToMatchLarge"),
            .cacheExtra = list(ELFs = attr(ELFs, "tags"),
                               ELFind = ELF,
                               rastTemplate = attr(rastTemplate, "tags")))
  }
  rastTemplate <- { # This is HUGE 2+GB
    { postProcess(rastTemplate, to = rasterToMatchLarge,
                  writeTo = file.path(inputPath, paste0("rasterTemplate_", ELF,".tif")))} |>
      Cache(omitArgs = c("x"), .cacheExtra = attr(rastTemplate, "tags"))
  }
  studyAreaLarge <- {
    {
      terra::as.polygons(rasterToMatchLarge > 0) # |>
      #  terra::buffer(width = d1) |>
      #  terra::buffer(width = -d1)
    } |> Cache(omitArgs = c("x"), .functionName = "studyAreaLarge",
               .cacheExtra = list(rtml = attr(rasterToMatchLarge, "tags")))
  }
  rasterToMatch <- {
    {
      rasterToMatchLarge |>
        replace(list = rasterToMatchLarge != 2, NA) |>
        terra::trim()
    } |> Cache(omitArgs = c("x"),
               .functionName = "rasterToMatch",
               .cacheExtra = list(rtml = attr(rasterToMatchLarge, "tags")))
  }
  studyAreaELF <- {
    terra::as.polygons(rasterToMatch) |>
      #  terra::buffer(width = d1) |>
      #  terra::buffer(width = -d1)
      Cache(omitArgs = c("x"), .functionName = "studyArea",
            .cacheExtra = list(rtm = attr(rasterToMatch, "tags")))
  }
  studyAreaReporting <- studyAreaELF
  sppEquiv <- {
    species <- LandR::speciesInStudyArea(studyAreaELF, dPath = inputPath) |>
      reproducible::Cache(omitArgs = "studyArea", .cacheExtra = list(sa = attr(studyAreaELF, "tags")))
    spp <- grep("_Spp", species$speciesList, invert = TRUE, value = TRUE)
    column <- LandR::equivalentNameColumn(spp, LandR::sppEquivalencies_CA)
    sppEquiv <- LandR::sppEquivalencies_CA[which(LandR::sppEquivalencies_CA[[column]] %in% spp),]
    sppEquiv[LANDIS_traits != "",]
  }
  studyAreaPSP <- {
    a <- reproducible::prepInputs(url = paste0("https://sis.agr.gc.ca/cansis/nsdb/ecostrat/",
                                               "province/ecoprovince_shp.zip"), dPath = inputPath,
                                  fun = "terra::vect", projectTo = studyAreaELF) |>
      reproducible::Cache(.functionName = "prepInputs_ecoprovince",
                          omitArgs = "projectTo", .cacheExtra = list(sa = attr(studyAreaELF, "tags")))
    b <- reproducible::postProcess(a, studyArea = studyAreaLarge) |>
      reproducible::Cache(omitArgs = c("x", "studyArea"), .cacheExtra = list(sa = attr(studyAreaLarge, "tags"),
                                                                             sa = attr(a, "tags")))
    ecoprovinces <- unique(b$ECOPROVINC)
    a[a$ECOPROVINC %in% ecoprovinces] # |> terra::aggregate()
  }
  
  if (is.null(sim$studyArea)) # conditional; can't put it in metadata or this will not be run first
    studyArea <- studyAreaELF
  # Put them all in the sim
  objsHere <- depends(sim)@dependencies[[currentModule(sim)]]@outputObjects$objectName
  list2env(mget(objsHere, envir = environment()), envir = envir(sim))
  
  Plots(as.list(sim[grep("studyArea|rasterToMatch", names(sim), value = TRUE)]),
                     title = paste0("StudyArea ", sim$.runName),
                     fn = SpaDES.project::plotSAs,
                     filename = paste0("studyAreas", sim$.runName),
                     path = inputPath,
                     types = c("screen", "png")) |>
    reproducible::Cache(.functionName = "Plots_studyAreas",
                        useCache = !identical(names(dev.cur()), "null device"))
  
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

### template for plot events
plotFun <- function(sim) {
  # ! ----- EDIT BELOW ----- ! #
  # do stuff for this event
  sampleData <- data.frame("TheSample" = sample(1:10, replace = TRUE))
  Plots(sampleData, fn = ggplotFn) # needs ggplot2

  # ! ----- STOP EDITING ----- ! #
  return(invisible(sim))
}

ggplotFn <- function(data, ...) {
  ggplot2::ggplot(data, ggplot2::aes(TheSample)) +
    ggplot2::geom_histogram(...)
}

