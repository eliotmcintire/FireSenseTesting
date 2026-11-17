## Everything in this file and any files in the R directory are sourced during `simInit()`;
## all functions and objects are put into the `simList`.
## To use objects, use `sim$xxx` (they are globally available to all modules).
## Functions can be used inside any function that was sourced in this module;
## they are namespaced to the module, just like functions in R packages.
## If exact location is required, functions will be: `sim$.mods$<moduleName>$FunctionName`.
defineModule(sim, list(
  name = "Biomass_yieldTables",
  description = "",
  keywords = "",
  authors = c(
    person("Celine", "Boisvenue", email = "cboivenue@gmail.com", role = c("aut")),
    person("Dominique", "Caron", email = "dominique.caron@nrcan-rncan.gc.ca", role = c("aut")),
    person("Camille", "Giuliano",  email = "camsgiu@gmail.com", role = c("ctb")),
    person("Eliot", "McIntire", email = "eliot.mcintire@nrcan-rncan.gc.ca", role = c("aut", "cre"))
  ),
  childModules = character(0),
  version = list(Biomass_yieldTables = "0.0.8"),
  timeframe = as.POSIXlt(c(NA, NA)),
  timeunit = "year",
  citation = list("citation.bib"),
  documentation = deparse(list("README.md", "Biomass_yieldTables.Rmd")), ## same file
  reqdPkgs = list("data.table", "PredictiveEcology/SpaDES.core@development (>= 1.0.9.9008)", "LandR", "terra"),
  parameters = rbind(
    defineParameter(".useCache", "character", c("generateData", "generateYieldTables"), NA, NA,
                    "Should caching of events or module be used?"),
    defineParameter(".plots", "character", "screen", NA, NA,
                    "Used by Plots function, which can be optionally used here"),
    defineParameter("numPlots", "integer", 40L, NA, NA,
                    "Number of pixel groups that will be randomly selected and ",
                    "for which yield curves will be plotted."),
    defineParameter("maxAge", "integer", NA, NA, NA,
                    "The number of years for which the yield tables are created. If not provided, ",
                    "the yield tables will be created to the largest species longevity."),
    defineParameter("moduleNameAndBranch", "character", "PredictiveEcology/Biomass_core@development (>= 1.3.9)", NA, NA,
                    "The branch and version number required for Biomass_core. This will be downloaded ",
                    "into the 'submodules' folder of this module, so it does not ",
                    "interact with the main user's modules."),
    defineParameter(".studyAreaName", "character", NA, NA, NA,
                    "Human-readable name for the study area used. If NA, a hash of studyArea will be used.")
  ),
  inputObjects = bindrows(
    expectsInput("cohortData", "data.table",
                 desc = paste("`data.table` with cohort-level information on age and biomass, by `pixelGroup` and ecolocation",
                              "(i.e., `ecoregionGroup`) with the following columns: `pixelGroup` (integer),",
                              "`ecoregionGroup` (factor), `speciesCode` (factor), `B` (integer in $g/m^2$), `age`",
                              "(integer in years). Must be supplied by the user or created in another module",
                              "like `Biomass_BiomassDataPrep`.")),
    expectsInput("species", "data.table",
                 desc = paste("A table of invariant species traits with the following trait colums:",
                              "'species', 'Area', 'longevity', 'sexualmature', 'shadetolerance',",
                              "'firetolerance', 'seeddistance_eff', 'seeddistance_max', 'resproutprob',",
                              "'mortalityshape', 'growthcurve', 'resproutage_min', 'resproutage_max',",
                              "'postfireregen', 'wooddecayrate', 'leaflongevity' 'leafLignin',",
                              "'hardsoft'. The last seven traits are not used in *Biomass_core*,",
                              "and may be ommited. However, this may result in downstream issues with",
                              "other modules. Must be supplied by the user or created in another module",
                              "like `Biomass_BiomassDataPrep`.")),
    expectsInput(
      objectName = "rasterToMatch", objectClass =  "SpatRaster",
      desc = "template raster to use for simulations; defaults to RIA study area")
  ),
  outputObjects = bindrows(
    createsOutput(objectName = "yieldTablesCumulative", objectClass = "data.table",
                  paste("Yield Tables intended to supply the requirements for a CBM spinup.",
                        "Columns are `yieldTableIndex`, `age`, `speciesCode`, `biomass`.",
                        "`yieldTableIndex` is the growth curve identifier that depends",
                        "on species combination. `biomass` is the biomass for the",
                        "given species at the pixel age.")),
    createsOutput(objectName = "yieldTablesId", objectClass = "data.table",
                  "A data.table linking spatially the `yieldTableIndex`. Columns are `pixelIndex` and `yieldTableIndex`")
  )
))


doEvent.Biomass_yieldTables = function(sim, eventTime, eventType) {
  switch(
    eventType,
    init = {
      mod$paths <- paths(sim)
      if (!is.null(Par$moduleNameAndBranch)) {
        mod$paths$modulePath <- file.path(modulePath(sim)[1], currentModule(sim), "submodules")
      }
      sim <- GenerateData(sim)
      
      sim <- GenerateYieldTables(sim)
      
      sim <- PlotYieldTables(sim)
    },
    warning(paste("Undefined event type: \'", current(sim)[1, "eventType", with = FALSE],
                  "\' in module \'", current(sim)[1, "moduleName", with = FALSE], "\'", sep = ""))
  )
  return(invisible(sim))
}

GenerateData <- function(sim) {
  message("Running simulations for all PixelGroups")
  biomassCoresOuts <-  Cache(runBiomass_core, moduleNameAndBranch = Par$moduleNameAndBranch,
                             paths = mod$paths, cohortData = sim$cohortData, maxAge = Par$maxAge,
                             species = sim$species, simEnv = envir(sim),
                             omitArgs = "simEnv")
  mod$yieldOutputs <- biomassCoresOuts$simOutputs
  sim$yieldTablesId <- data.table(
    yieldTableIndex = as.integer(biomassCoresOuts$yieldPixelGroupMap[])
  )
  ####
  windowSize <- 3L
  i <- 1L
  while(any(sim$yieldTablesId$yieldTableIndex == 0, na.rm = T) && windowSize <= 10){
    Npix <- sum(sim$yieldTablesId$yieldTableIndex == 0, na.rm = T)
    message("Filling empty forest pixels: ", Npix, " pixels to fill.")
    message("Using window size = ", windowSize)
    # replace 0 by NA
    x <- biomassCoresOuts$yieldPixelGroupMap
    x[x == 0] <- NA
    focaledYldPixGrMap <- focal(x, w = windowSize, fun = "modal", na.rm = TRUE, na.policy="only")
    newClasses <- focaledYldPixGrMap[sim$yieldTablesId$yieldTableIndex == 0]
    idToReplace <- !is.na(sim$yieldTablesId$yieldTableIndex) & sim$yieldTablesId$yieldTableIndex == 0 & !is.na(focaledYldPixGrMap[])
    sim$yieldTablesId[idToReplace, ] <- newClasses[!is.na(newClasses)]
    biomassCoresOuts$yieldPixelGroupMap[idToReplace] <- focaledYldPixGrMap[idToReplace]
    if(i %% 3 == 0) {windowSize <- windowSize + 2}
    i <- i + 1L
  }
  ####
  sim$yieldTablesId <- sim$yieldTablesId[, pixelIndex := .I] |> na.omit()
  setcolorder(sim$yieldTablesId, c("pixelIndex", "yieldTableIndex"))
  mod$digest <- biomassCoresOuts$digest
  return(sim)
}

GenerateYieldTables <- function(sim) {
  message("Simulation done! Loading in cohortData files")
  cohortDataAll <- Cache(ReadExperimentFiles, omitArgs = "factorialOutputs",
                         .cacheExtra = mod$digest$outputHash, as.data.table(mod$yieldOutputs)[saved == TRUE])
  # Because LandR biomass will lump all age < 11 into age 0
  if ((sum(cohortDataAll$age[cohortDataAll$yieldTableIndex == cohortDataAll$yieldTableIndex[1]] == 0) %% 11) == 0) {
    cohortDataAll[age == 0, age := 0:10, by = c("yieldTableIndex", "speciesCode")]
  }
  sim$yieldTablesCumulative <- cohortDataAll
  setcolorder(sim$yieldTablesCumulative, c("yieldTableIndex", "speciesCode", "age", "biomass"))
  rm(cohortDataAll)
  gc()
  return(sim)
}

PlotYieldTables <- function(sim) {
  fname = paste("Yield Curves from", Par$numPlots,
                "random plots -", gsub(":", "_", sim$._startClockTime))
  Plots(data = sim$yieldTablesCumulative, usePlot = FALSE, fn = pltfn,
        numPlots = Par$numPlots,
        ggsaveArgs = list(width = 10, height = 7),
        filename = fname)
  mapRast <- rast(sim$rasterToMatch)
  mapRast[sim$yieldTablesId$pixelIndex] <- sim$yieldTablesId$yieldTableIndex
  Plots(mapRast, usePlot = TRUE, deviceArgs = list(width = 700, height = 500),
        filename = "yieldTableIdMap")
  return(sim)
}


## .inputObjects ------------------------------------------------------------------------------
.inputObjects <- function(sim) {
  cacheTags <- c(currentModule(sim), "function:.inputObjects")
  dPath <- asPath(getOption("reproducible.destinationPath", dataPath(sim)), 1)
  message(currentModule(sim), ": using dataPath '", dPath, "'.")
  
  if (!suppliedElsewhere("rasterToMatch", sim)) {
    stop("Please provide a 'rasterToMatch' object")
  }
  
  if (!suppliedElsewhere("cohortData", sim)) {
    stop("Please provide a 'cohortData' table or use a module like Biomass_borealDataPrep")
  }
  
  if (!suppliedElsewhere("species", sim)) {
    stop("Please provide a 'species' table or use a module like Biomass_borealDataPrep")
  }
  
  return(invisible(sim))
}

