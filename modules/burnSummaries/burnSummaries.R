defineModule(sim, list(
  name = "burnSummaries",
  description = "Extract simulated fire sizes to generate fire regime statistics",
  keywords = "",
  authors = c(
    person("Alex M", "Chubaty", email = "achubaty@for-cast.ca", role = c("aut", "cre"),
           comment = c(ORCID = "0000-0001-7146-8135"))
  ),
  childModules = character(0),
  version = list(burnSummaries = "1.0.1"),
  timeframe = as.POSIXlt(c(NA, NA)),
  timeunit = "year",
  citation = list("citation.bib"),
  documentation = list("NEWS.md", "README.md", "burnSummaries.Rmd"),
  loadOrder = list(after = c("fireSense", "LandMine", "scfmSpread")),
  reqdPkgs = list("data.table", "dplyr", "ggplot2", "ggspatial", "kSamples", "patchwork",
                  "reproducible", "SpaDES.core", "stringr", "terra", "tidyterra"),
  parameters = bindrows(
    defineParameter("fireTimestep", "integer", 1L, NA, NA,
                    "simulation time interval between burn events"),
    defineParameter("mode", "character", "single", NA, NA,
                    paste("use 'single' to run part of a simulation;",
                          "use 'multi' to run as part of postprocessing multiple runs.")),
    defineParameter("reps", "integer", 1L:10L, 1L, NA_integer_,
                    paste("in single mode, should be length 1 indicating the replicate id;",
                          "in multi mode, a vector of replicate IDs.")),
    defineParameter("simOutputPath", "character", outputPath(sim), NA, NA,
                    "Directory specifying the location of the simulation outputs."),
    defineParameter("simTimes", "numeric", c(NA, NA), NA, NA,
                    "Simulation start and end times when running in 'multi' mode."),
    defineParameter("summaryInterval", "integer", 100L, NA, NA,
                    "simulation time interval at which to take 'snapshots' used for summary analyses"),
    defineParameter("summaryPeriod", "integer", c(700L, 1000L), NA, NA,
                    "lower and upper end of the range of simulation times used for summary analyses"),
    defineParameter(".plots", "character", "png", NA, NA,
                    "Used by `Plots` function, which can be optionally used here."),
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
    expectsInput("burnMap", "SpatRaster",
                 desc = paste("Cumulative burn map.",
                              "Required in single mode.")),
    expectsInput("burnSummary", "data.table",
                 desc = paste("Fire summary table from `fireSense` or `scfm`.",
                              "One of `burnSummary` or `fireSizes` is required in single mode.")),
    expectsInput("fireSizes", "list",
                 desc = paste("Fire sizes summary tables from LandMine.",
                              "One of `burnSummary` or `fireSizes` is required in single mode.")),
    expectsInput("flammableMap", "SpatRaster",
                 desc = paste("Binary flammability map.", "Required in single mode.")),
    expectsInput("rstCurrentBurn", "SpatRaster",
                 desc = "Binary raster of fires, 1 meaning 'burned', 0 or NA is non-burned"),
    expectsInput("rstTimeSinceFire", "SpatRaster",
                 desc = "map of time since last burn, with non-flammable pixels receiving `NA`.")
  ),
  outputObjects = bindrows(
    createsOutput("fireSizes", "data.table", 
                  desc = "summary fire sizes table"),
    createsOutput("rstTimeSinceFire", "SpatRaster",
                  desc = "map of time since last burn, with non-flammable pixels receiving `NA`.")
  )
))

## event types
#   - type `init` is required for initialization

doEvent.burnSummaries = function(sim, eventTime, eventType) {
  switch(
    eventType,
    init = {
      mod$analysesOutputsTimes <- analysesOutputsTimes(P(sim)$summaryPeriod, P(sim)$summaryInterval)

      if (P(sim)$mode == "single") {
        sim <- InitSingle(sim)

        sim <- scheduleEvent(sim, start(sim), "burnSummaries", "update_tsf")
        sim <- scheduleEvent(sim, end(sim), "burnSummaries", "create_fireSizes", .last())

        sim <- scheduleEvent(sim, start(sim), "burnSummaries", "save_single", .last())
        sim <- scheduleEvent(sim, P(sim)$summaryPeriod[1], "burnSummaries", "save_single", .last())
        sim <- scheduleEvent(sim, end(sim), "burnSummaries", "save_single", .last())
      } else if (P(sim)$mode == "multi") {
        sim <- InitMulti(sim)

        ## schedule future event(s)
        sim <- scheduleEvent(sim, start(sim), "burnSummaries", "summary")
        sim <- scheduleEvent(sim, start(sim), "burnSummaries", "plot")
      }
    },
    update_tsf = {
      sim$rstTimeSinceFire[] <- as.integer(sim$rstTimeSinceFire[]) + as.integer(P(sim)$fireTimestep) ## preserves NAs
      sim$rstTimeSinceFire[which(sim$rstCurrentBurn[] == 1)] <- 0L

      ## schedule next event
      sim <- scheduleEvent(sim, time(sim) + P(sim)$fireTimestep, "burnSummaries", "update_tsf")
    },
    create_fireSizes = {
      ## try to guess the replicate being run
      if (length(P(sim)$reps) == 1) {
        repID <- P(sim)$reps
      } else if (grepl("rep", outputPath(sim))) {
        repID <- stringr::str_extract(outputPath(sim), "rep[0-9].*") |>
          stringr::str_remove("rep") |>
          as.integer()
      } else {
        repID <- NA_integer_
      }
      if (!is.null(sim[["burnSummary"]])) {
        fs <- copy(sim[["burnSummary"]])
        fs[, `:=`(
          simArea = P(sim)$.studyAreaName,
          size = N, ## use number of pixels (N) instead of areaBurned
          maxSize = NA_integer_, ## NOTE: no "target" or "expected" fire size w/ scfm nor fS
          rep = repID
        )]
        set(fs, NULL, c("areaBurned", "igLoc", "grp", "PolyID"), NULL)
        setcolorder(fs, c("simArea", "rep", "year", "size", "maxSize"))
        setnames(fs, old = c("size", "maxSize"), new = c("simSize", "expSize"))
      } else if (!is.null(sim[["fireSizes"]])) {
        fs <- rbindlist(sim[["fireSizes"]], idcol = "year")
        fs[, `:=`(simArea = P(sim)$.studyAreaName, rep = repID)]
        setcolorder(fs, c("simArea", "rep", "year", "size", "maxSize"))
        setnames(fs, old = c("size", "maxSize"), new = c("simSize", "expSize"))
      } else {
        stop("cumulative fire summary object not found")
      }

      ffs <- file.path(outputPath(sim), "burnSummaries_fireSizes.csv")
      data.table::fwrite(fs, file = ffs)

      sim <- registerOutputs(ffs, sim)
    },
    summary = {
      sim <- FireSummaries(sim)
    },
    plot = {
      plotFun(sim)
    },
    save_single = {
      padYear <- paddedFloatToChar(time(sim), padL = ceiling(log10(end(sim) + 1)))

      ## objects to save during simulation --------------------------------------------------------
      times_during <- c(start(sim), end(sim), mod$analysesOutputsTimes) |> unique() |> sort()

      ## fmt: skip
      if (time(sim) %in% times_during) {
        f_rstTimeSinceFire <- file.path(outputPath(sim), paste0("rstTimeSinceFire_year", padYear, ".tif"))
        terra::writeRaster(sim$rstTimeSinceFire, f_rstTimeSinceFire, datatype = "INT2U", overwrite = TRUE)
        sim <- registerOutputs(f_rstTimeSinceFire, sim)

        if (time(sim) >= P(sim)$summaryPeriod[1] && time(sim) < P(sim)$summaryPeriod[2]) {
          sim <- scheduleEvent(sim, time(sim) + P(sim)$summaryInterval, "burnSummaries", "save_single", .last())
        }
      }

      ## objects to save at end of simulation -----------------------------------------------------
      if (time(sim) == end(sim)) {
        f_burnMap <- file.path(outputPath(sim), paste0("burnMap_year", padYear, ".tif"))
        terra::writeRaster(sim$burnMap, f_burnMap, datatype = "INT2U", overwrite = TRUE)
        sim <- registerOutputs(f_burnMap, sim)

        f_flammableMap <- file.path(outputPath(sim), paste0("flammableMap_year", padYear, ".tif"))
        terra::writeRaster(sim$flammableMap, f_flammableMap, datatype = "INT2U", overwrite = TRUE)
        sim <- registerOutputs(f_flammableMap, sim)
      }
    },
    ## fmt: skip
    warning(paste(
      "Undefined event type: \'", current(sim)[1, "eventType", with = FALSE],
      "\' in module \'", current(sim)[1, "moduleName", with = FALSE], "\'", sep = ""
    ))
  )
  return(invisible(sim))
}

## event functions ------------------------------------------------------------------

InitSingle <- function(sim) {
  ## sanity check
  terra::compareGeom(
    sim$rstCurrentBurn,
    sim$flammableMap,
    sim$rstTimeSinceFire,
    crs = TRUE,
    ext = TRUE,
    rowcol = TRUE,
    res = TRUE
  )

  return(invisible(sim))
}

InitMulti <- function(sim) {
  ## check for necessary output files -----------------------------------------------
  allReps <- sprintf("rep%02d", P(sim)$reps)
  padL <- ceiling(log10(P(sim)$simTimes[2] + 1))
  padYearStart <- paddedFloatToChar(P(sim)$simTimes[1], padL = padL)
  padYearEnd <- paddedFloatToChar(P(sim)$simTimes[2], padL = padL)

  ## all reps have same flammable map
  flm <- file.path(outputPath(sim), allReps[1], paste0("flammableMap_year", padYearEnd, ".tif"))

  stopifnot(file.exists(flm))

  flammableMap <- terra::rast(flm)
  pixelSize <- terra::res(flammableMap) ## keep both x and y dimensions

  burnMaps <- lapply(allReps, function(rep) {
    message(paste("Loading burn maps for rep", rep, "..."))
    fbm <- file.path(outputPath(sim), rep, paste0("burnMap_year", padYearEnd, ".tif"))

    stopifnot(file.exists(fbm))

    cumulBurnMap <- terra::rast(fbm)

    ## sanity check
    terra::compareGeom(cumulBurnMap, flammableMap, res = TRUE)

    ## mean annual cumulative burn map
    cumulBurnMap / (P(sim)$simTimes[2] - P(sim)$simTimes[1])
  }) |>
    terra::rast() |>
    terra::app(sum, na.rm = TRUE)

  meanAnnualCumulBurnMap <- burnMaps / length(allReps)

  firePolys <- prepInputs(
    url = "https://cwfis.cfs.nrcan.gc.ca/downloads/nfdb/fire_poly/current_version/NFDB_poly.zip",
    fun = "terra::vect",
    destinationPath = inputPath(sim)
  ) |>
    reproducible::Cache() |>
    terra::project(flammableMap)

  fireYears <- tidyterra::filter(firePolys, YEAR > 0) |> dplyr::pull("YEAR") |> unique() |> sort()
  meanAnnualCumulBurnMapHistoric <- terra::rasterize(
    firePolys,
    flammableMap,
    field = "YEAR",
    fun = "count"
  )
  meanAnnualCumulBurnMapHistoric <- meanAnnualCumulBurnMapHistoric / length(fireYears)

  nonFlammable <- which(
    is.na(terra::values(flammableMap, mat = FALSE)) | terra::values(flammableMap, mat = FALSE) == 0
  )
  if (length(nonFlammable) > 0) {
    meanAnnualCumulBurnMap[nonFlammable] <- NA
    meanAnnualCumulBurnMapHistoric[nonFlammable] <- NA
    flammableMap[nonFlammable] <- NA
  }

  mod$flammableMap <- flammableMap
  mod$meanAnnualCumulBurnMap <- meanAnnualCumulBurnMap
  mod$meanAnnualCumulBurnMapHistoric <- meanAnnualCumulBurnMapHistoric
  mod$pixelSize <- pixelSize

  # ! ----- STOP EDITING ----- ! #

  return(invisible(sim))
}

FireSummaries <- function(sim) {
  allReps <- sprintf("rep%02d", P(sim)$reps)
  padL <- ceiling(log10(P(sim)$simTimes[2] + 1))
  padYearStart <- paddedFloatToChar(P(sim)$simTimes[1], padL = padL)
  padYearEnd <- paddedFloatToChar(P(sim)$simTimes[2], padL = padL)

  studyAreaName <- P(sim)$.studyAreaName

  sim$fireSizes <- lapply(allReps, function(rep) {
    f_fireSizes <- file.path(outputPath(sim), rep, "burnSummaries_fireSizes.csv")

    stopifnot(file.exists(f_fireSizes))

    data.table::fread(f_fireSizes)
  }) |>
    rbindlist()

  f_out <- file.path(outputPath(sim), paste0("burnSummaries_fireSizes_allReps.csv"))
  fwrite(sim$fireSizes, f_out)

  ## TODO: add this file to list of outputs
  sim <- registerOutputs(f_out, sim)

  return(invisible(sim))
}

### template for plot events
plotFun <- function(sim) {
  # ! ----- EDIT BELOW ----- ! #
  studyAreaName <- P(sim)$.studyAreaName

  ## cumulative burn maps
  use_palette = "muted" # "bl_yl_rd"

  ggCumulBurnMapExp <- ggplot() +
    tidyterra::geom_spatraster(data = mod$meanAnnualCumulBurnMapHistoric) +
    tidyterra::scale_fill_whitebox_c(palette = use_palette) +
    theme_bw() +
    ggspatial::annotation_north_arrow(
      location = "bl",
      which_north = "true",
      pad_x = unit(0.25, "in"),
      pad_y = unit(0.25, "in"),
      style = north_arrow_fancy_orienteering
    ) +
    xlab("Longitude") +
    ylab("Latitude") +
    ggtitle(paste("Historic mean annual cumulative burn map for", studyAreaName))

  ggCumulBurnMapSim <- ggplot() +
    tidyterra::geom_spatraster(data = mod$meanAnnualCumulBurnMap) +
    tidyterra::scale_fill_whitebox_c(palette = use_palette) +
    theme_bw() +
    ggspatial::annotation_north_arrow(
      location = "bl",
      which_north = "true",
      pad_x = unit(0.25, "in"),
      pad_y = unit(0.25, "in"),
      style = north_arrow_fancy_orienteering
    ) +
    xlab("Longitude") +
    ylab("Latitude") +
    ggtitle(paste("Simulated mean annual cumulative burn map for", studyAreaName))

  if ("png" %in% P(sim)$.plots) {
    fggCumulBurnMap <- file.path(figurePath(sim), "cumulative_burn_maps.png")
    ggCumulBurnMap <- (ggCumulBurnMapExp | ggCumulBurnMapSim)
    ggsave(fggCumulBurnMap, ggCumulBurnMap, height = 10, width = 20, type = "cairo")
    sim <- registerOutputs(fggCumulBurnMap, sim)
  }

  ## fire size histograms w/ median fire sizes
  pixelSizeHa <- prod(mod$pixelSize) / 10^4

  subsetDT <- sim$fireSizes[simArea == studyAreaName & (expSize > 0 | simSize > 0), ]

  ## scfm and fireSense don't set target fire sizes, but LandMine does
  fireModelUsesTargetSize <- isFALSE(all(is.na(subsetDT$expSize)))

  if (isTRUE(fireModelUsesTargetSize)) {
    subsetDT[, expSizeHa := expSize * pixelSizeHa]
    subsetDT[, logExpSize := log(expSize)]
    subsetDT[, logExpSizeHa := log(expSize * pixelSizeHa)]
  }

  subsetDT[, simSizeHa := simSize * pixelSizeHa]
  subsetDT[, logSimSize := log(simSize)]
  subsetDT[, logSimSizeHa := log(simSize * pixelSizeHa)]

  ## per Dave's original email:
  ## > What I would like is both the number of disturbances on the y axis,
  ## > and the area of disturbances on a second y-axis graph.
  ## Per Eliot: x-axis uses same bins as histogram, with y-axis of median area burned per bin

  if (isTRUE(fireModelUsesTargetSize)) {
    maxLogExpSizeHa <- max(subsetDT$logExpSizeHa, na.rm = TRUE)
    ## fmt: skip
    breaks <- seq(0.0, ceiling(max(maxLogExpSizeHa, maxLogSimSizeHa, na.rm = TRUE) / 0.5) * 0.5, 0.5)
    hexp <- hist(subsetDT$logExpSizeHa, breaks = breaks, plot = FALSE)
    countsExp <- hexp$counts
    subsetDT[, binIDexp := cut(logExpSizeHa, hexp$breaks)]
    ## fmt: skip
    summaryExpDT <- subsetDT[ , lapply(.SD, stats::median, na.rm = TRUE), by = binIDexp, .SDcols = "expSizeHa"]
    setnames(summaryExpDT, "expSizeHa", "medExpSizeHa")
    summaryExpDT <- summaryExpDT[, medLogExpSizeHa := log(medExpSizeHa)]

    midsExp <- cbind(
      as.numeric(sub("\\((.+),.*", "\\1", summaryExpDT$binIDexp)),
      as.numeric(sub("[^,]*,([^]]*)\\]", "\\1", summaryExpDT$binIDexp))
    ) |>
      rowMeans()
    summaryExpDT <- summaryExpDT[, midsExp := midsExp]
    scaleFactorExp <- max(countsExp) / maxLogExpSizeHa
  }

  maxLogSimSizeHa <- max(subsetDT$logSimSizeHa, na.rm = TRUE)

  breaks <- seq(0.0, ceiling(max(maxLogSimSizeHa, na.rm = TRUE) / 0.5) * 0.5, 0.5)
  hsim <- hist(subsetDT$logSimSizeHa, breaks = breaks, plot = FALSE)
  countsSim <- hsim$counts
  subsetDT[, binIDsim := cut(logSimSizeHa, hsim$breaks)]
  ## fmt: skip
  summarySimDT <- subsetDT[, lapply(.SD, stats::median, na.rm = TRUE), by = binIDsim, .SDcols = "simSizeHa"]
  setnames(summarySimDT, "simSizeHa", "medSimSizeHa")
  summarySimDT <- summarySimDT[, medLogSimSizeHa := log(medSimSizeHa)]

  midsSim <- cbind(
    as.numeric(sub("\\((.+),.*", "\\1", summarySimDT$binIDsim)),
    as.numeric(sub("[^,]*,([^]]*)\\]", "\\1", summarySimDT$binIDsim))
  ) |>
    rowMeans()
  summarySimDT <- summarySimDT[, midsSim := midsSim]
  scaleFactorSim <- max(countsSim) / maxLogSimSizeHa

  y1col <- "grey20"
  y2col <- "darkred"
  y1lab <- "Total number of fires across all simulated years"
  y2lab <- "Median log[fireSize] (ha)"
  x_lab <- "log[fireSize] (ha)"

  if (isTRUE(fireModelUsesTargetSize)) {
    ggHistExp <- ggplot(subsetDT, aes(x = logExpSizeHa)) +
      geom_histogram(breaks = breaks, alpha = 0.5, fill = y1col) +
      stat_summary_bin(
        data = summaryExpDT,
        mapping = aes(x = midsExp, y = medLogExpSizeHa * scaleFactorExp),
        fun = "identity",
        geom = "point",
        breaks = breaks,
        col = y2col
      ) +
      scale_y_continuous(y1lab, sec.axis = sec_axis(~ . / scaleFactorExp, name = y2lab)) +
      xlab(x_lab) +
      ggtitle(paste("Total expected number and size of fires in", studyAreaName)) +
      theme_bw() +
      theme(
        axis.title.y.left = element_text(color = y1col),
        axis.text.y.left = element_text(color = y1col),
        axis.title.y.right = element_text(color = y2col),
        axis.text.y.right = element_text(color = y2col)
      )
  }

  ggHistSim <- ggplot(subsetDT, aes(x = logSimSizeHa)) +
    geom_histogram(breaks = breaks, alpha = 0.5, fill = y1col) +
    stat_summary_bin(
      data = summarySimDT,
      mapping = aes(x = midsSim, y = medLogSimSizeHa * scaleFactorSim),
      fun = "identity",
      geom = "point",
      breaks = breaks,
      col = y2col
    ) +
    scale_y_continuous(y1lab, sec.axis = sec_axis(~ . / scaleFactorSim, name = y2lab)) +
    xlab(x_lab) +
    ggtitle(paste("Total simulated number and size of fires in", studyAreaName)) +
    theme_bw() +
    theme(
      axis.title.y.left = element_text(color = y1col),
      axis.text.y.left = element_text(color = y1col),
      axis.title.y.right = element_text(color = y2col),
      axis.text.y.right = element_text(color = y2col)
    )

  if ("png" %in% P(sim)$.plots) {
    if (isTRUE(fireModelUsesTargetSize)) {
      fggHistExp <- file.path(figurePath(sim), "expected_number_size_fires.png")
      ggsave(fggHistExp, ggHistExp, height = 10, width = 10, type = "cairo")
      sim <- registerOutputs(fggHistExp, sim)
    }

    fggHistSim <- file.path(figurePath(sim), "simulated_number_size_fires.png")
    ggsave(fggHistSim, ggHistSim, height = 10, width = 10, type = "cairo")
    sim <- registerOutputs(fggHistSim, sim)
  }

  if (isTRUE(fireModelUsesTargetSize)) {
    ## exp vs sim fire sizes
    ggExpVsSim <- ggplot(subsetDT, aes(x = expSizeHa, y = simSizeHa)) +
      geom_smooth(method = lm) +
      scale_x_continuous(limits = c(0, NA)) +
      scale_y_continuous(limits = c(0, NA)) +
      xlab("Expected fire size (ha)") +
      ylab("Simulated fire size (ha)") +
      ggtitle(paste("Expected vs. simulated fire sizes in", studyAreaName)) +
      theme_bw() +
      geom_abline(slope = 1, lty = "dotted")

    ggExpVsSimHex <- ggplot(subsetDT, aes(x = expSizeHa, y = simSizeHa)) +
      geom_hex(bins = 50) +
      xlab("Expected fire size (ha)") +
      ylab("Simulated fire size (ha)") +
      ggtitle(paste("Expected vs. simulated fire sizes in", studyAreaName)) +
      theme_bw() +
      geom_abline(slope = 1, lty = "dotted")

    if ("png" %in% P(sim)$.plots) {
      ## NOTE: keep 1:1 aspect ratio on these plots
      fggExpVsSim <- file.path(figurePath(sim), "exp_vs_sim_fire_sizes.png")
      ggsave(filename = fggExpVsSim, plot = ggExpVsSim, height = 10, width = 10, type = "cairo")
      sim <- registerOutputs(fggExpVsSim, sim)

      fggExpVsSimHex <- file.path(figurePath(sim), "exp_vs_sim_fire_sizes_hex.png")
      ggsave(
        filename = fggExpVsSimHex,
        plot = ggExpVsSimHex,
        height = 10,
        width = 10,
        type = "cairo"
      )
      sim <- registerOutputs(fggExpVsSimHex, sim)
    }
  }

  ## TODO: is it worth testing fire size distributions? (very slow, and plots show they're bang-on)
  # kSamples::ad.test(subsetDT$simSize, subsetDT$expSize) ## TODO: output this somewhere...

  ## TODO: registerOutputs
  # registerOutputs(c(fggCumulBurnMap, fggHistExp, fggHistSim, fggExpVsSim, fggExpVsSimHex), sim)

  # ! ----- STOP EDITING ----- ! #
  return(invisible(sim))
}

.inputObjects <- function(sim) {
  ## NOTE: fireSize table can be derived from 'burnSummary' or 'fireSize' objects,
  ## so don't implement a hard requirement for either here.
  if (P(sim)$mode == "single") {
    stopifnot(suppliedElsewhere("burnMap", sim), suppliedElsewhere("flammableMap", sim))

    if (!suppliedElsewhere("rstTimeSinceFire", sim)) {
      sim$rstTimeSinceFire <- LandR::prepInputsStandAgeMap(
        dataSource = "SCANFI",
        dataYear = P(sim)$dataYear,
        ageFun = "terra::rast",
        cropTo = sim$flammableMap,
        maskTo = sim$flammableMap,
        destinationPath = outputPath(sim)
      )

      sim$rstTimeSinceFire[sim$flammableMap[] == 0L] <- NA ## non-flammable areas are permanent
      sim$rstTimeSinceFire[] <- as.integer(sim$rstTimeSinceFire[])
    }
  } else if (P(sim)$mode == "multi") {
    stopifnot(suppliedElsewhere("reportingPolygons", sim))
  }

  return(invisible(sim))
}
