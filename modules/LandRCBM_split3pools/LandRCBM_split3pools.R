defineModule(sim, list(
  name = "LandRCBM_split3pools",
  description = paste("Takes total aboveground biomass provided by LandR and divides",
                      "it into the 3 required CBM pools."),
  keywords = "",
  authors = c(
    person("Celine", "Boisvenue", email = "cboivenue@gmail.com", role = c("aut", "cre")),
    person("Dominique", "Caron", email = "dominique.caron@nrcan-rncan.gc.ca", role = c("aut")),
    person("Susan",   "Murray",    email = "murray.e.susan@gmail.com",           role = c("ctb")),
    person("Camille", "Giuliano",  email = "camsgiu@gmail.com",                  role = c("ctb")),
    person("Alex M", "Chubaty", email = "achubaty@for-cast.ca", role = "ctb")
  ),
  childModules = character(0),
  version = list(LandRCBM_split3pools = "0.0.0.9000"),
  timeframe = as.POSIXlt(c(NA, NA)),
  timeunit = "year",
  citation = list("citation.bib"),
  documentation = list("README.md", "LandRCBM_split3pools.Rmd"), ## same file
  reqdPkgs = list("PredictiveEcology/SpaDES.core", "reproducible (>= 2.1.2)", "data.table", "ggplot2", "terra",
                  "SpaDES.tools (>= 1.0.0.9001)", "PredictiveEcology/CBMutils@development (>= 2.5.6)"),
  parameters = bindrows(
    defineParameter("minMerchantableAge", "integer", 15L, NA, NA,
                    "Minimum age for which a cohort can have wood considered merchantable."),
    defineParameter(".plots", "character", "screen", NA, NA,
                    "Used by Plots function, which can be optionally used here"),
    defineParameter(".plotInitialTime", "numeric", start(sim), NA, NA,
                    "Describes the simulation time at which the first plot event should occur."),
    defineParameter(".plotInterval", "numeric", NA, NA, NA,
                    "Describes the simulation time interval between plot events."),
    defineParameter(".plotNumPixGroup", "integer", 10L, NA, NA,
                    "When plotting the yield curves, this is how many unique pixel groups will ",
                    "be randomly selected and plotted."),
    defineParameter(".plotMaps", "logical", TRUE, NA, NA,
                    "Controls whether maps should be plotted or not. Set to `FALSE` if `P(sim)$.plots == NA`"),
    defineParameter(".useCache", "character", "postSpinupAdjustBiomass", NA, NA,
                    "Should caching of events or module be used?")
  ),
  inputObjects = bindrows(
    expectsInput(
      objectName = "cohortData", objectClass = "data.table",
      desc = "Total above ground biomass (g/m^2) of each cohorts by pixel groups.",
      columns = c(
        speciesCode = "Species code used by LandR",
        ecoregionGroup = "The LandR spatial units (i.e., ecoregion).",
        age = "Age of the cohort.",
        B = "Total above ground biomass in (g/m^2).",
        pixelGroup = "Id of the group of pixels sharing the same cohort composition and ecoregion, used in LandR.",
        totalBiomass = "Total above ground biomass in the pixel group."
      )
    ),
    expectsInput(
      objectName = "cbm_vars",
      objectClass = "list",
      desc = paste("List of 5 data tables defining active cohorts in the current year:",
                   "key, parameters, pools, flux, and state.",
                   "This is created initially during the spinup and updated each year."),
    ), 
    expectsInput(
      objectName = "pixelGroupMap", objectClass = "SpatRaster",
      desc = paste("Map of pixel group from LandR. Group of pixels that shares the same ",
                   "cohort composition.")
    ),
    expectsInput(
      objectName = "rasterToMatch", objectClass =  "SpatRaster",
      desc = "Template raster to use for simulations; defaults is the RIA study area."
    ),
    expectsInput(
      objectName = "standDT", objectClass = "data.table",
      desc = "Table of stand attributes.",
      columns = c(
        pixelIndex   = "Stand ID",
        admin_name   = "Canada province or territory name",
        admin_abbrev = "Canada province or territory 2-character abbreviation",
        eco_id       = "Canada ecozone ID"
      )
    ),
    expectsInput(
      objectName = "table6tb", objectClass = "data.table",
      desc = paste(
        "Boudewyn et al. (2007) an alternative set of proportion model parameters", 
        "to use when total biomass per hectare in tonnes (tb = stem wood + stem bark + branches + foliage)", 
        "is the independent variable instead of gross merchantable volume per hectare."),
      sourceURL = "https://nfi.nfis.org/resources/biomass_models/appendix2_table6_tb.csv"
    ),
    expectsInput(
      objectName = "table7tb", objectClass = "data.table",
      desc = paste(
        "Boudewyn et al. (2007) an alternative set of caps on proportion models",
        "to use in conjunction with the alternative set of proportion models",
        "when total biomass per hectare in tonnes (tb = stem wood + stem bark + branches + foliage)", 
        "is the independent variable instead of gross merchantable volume per hectare."),
      sourceURL = "https://nfi.nfis.org/resources/biomass_models/appendix2_table7_tb.csv"
    ),
    expectsInput(
      objectName = "tableMerch", objectClass = "data.table",
      desc = paste(
        "Parameters to estimate the proportion of stemwood biomass that is merchantable", 
        "estimated by approximating the relationship between stemwood biomass and",
        "nonmerchfactor predicted by equation 2 of Boudewyn et al. (2007)."),
      sourceURL = "https://drive.google.com/file/d/1wa2QMd7Eo-bPpfigchdpPPPxo7NVpPiC"
    ),
    expectsInput(
      objectName = "yieldTablesCumulative", objectClass = "data.table",
      desc = paste("Yield Tables intended to supply the requirements for a CBM spinup.",
                   "Columns are `yieldTableIndex`, `age`, `speciesCode`, `biomass`. `yieldTableIndex` is the",
                   "growth curve identifier that depends on species combination.",
                   "`biomass` is the biomass for the given species at the pixel age."),
      columns = c(
        yieldTableIndex = "Id of the group of pixels sharing yield tables.",
        age = "Age of species going from 0 to their longevity.",
        speciesCode = "Species code used by LandR.",
        biomass = "Above ground biomass in g/m^2."
      )
    ),
    expectsInput(
      objectName = "yieldTablesId", objectClass = "data.table",
      desc = paste("A data.table linking spatially the `yieldTableIndex`. Columns are `pixelIndex` and `yieldTableIndex`."),
      columns = c(
        pixelIndex = "Integer id of the pixel.",
        yieldTableIndex = "Id of the group of pixels sharing yield tables."
      )
    ),
    expectsInput(
      objectName = "sppEquiv", objectClass = "data.table",
      desc = "Optional. Table of species equivalencies. See `LandR::sppEquivalencies_CA`."
    ),
  ),
  outputObjects = bindrows(
    createsOutput(
      objectName = "aboveGroundBiomass",
      objectClass = "data.table",
      desc = paste("Above ground biomass (metric tonnes of carbon/ha) in each pool",
                   "for each pixel and cohort. Gets updated at each timestep.",
                   "Columns are `pixelIndex`, `speciesCode`, `age`, `merch`, `foliage`, and `other`.")
    ),
    createsOutput(
      objectName = "cbm_vars", objectClass = "list",
      desc = paste(
        "List of 5 data tables defining active cohorts in the current year:",
        "key, parameters, pools, flux, and state.",
        "This is created initially during the spinup and updated each year.")
    ),
    createsOutput(
      objectName = "cohortDT",
      objectClass = "data.table",
      desc = paste("Cohort-level information.",
                   "Columns are `cohortID`, `pixelIndex`, `age`, and `gcID`.")
    ),
    createsOutput(
      objectName = "gcIncrements",
      objectClass = "data.table",
      desc = paste("Increments (metric tonnes of carbon/ha) in each pool",
                   "for each pixel and cohort. Gets updated at each timestep.",
                   "Columns are `gcID`, `age`,`merch_inc`, `foliage_inc`, and `other_inc`.")
    ),
    createsOutput(
      objectName = "gcMeta",
      objectClass = "data.table",
      desc = paste("Growth curve-level information.",
                   "Columns are `gcID`, `speciesCode`, and `sw`")
    ),
    createsOutput(
      objectName = "summaryAGB",
      objectClass = "data.table",
      desc = paste("Sum of carbon mass for each species and above ground", 
                   "pool at each timestep across the landscape. Columns are `year`,",
                   "`speciesCode`, `merch`, `foliage`, `other`.")
    )
  )
))

doEvent.LandRCBM_split3pools = function(sim, eventTime, eventType) {
  switch(
    eventType,
    init = {
      
      # split initial cohortData and yield tables
      sim <- scheduleEvent(sim, start(sim), "LandRCBM_split3pools", "splitInit", eventPriority = 3)
      
      # adjust that the live biomass post-CBM spinup with the biomass in LandR
      sim <- scheduleEvent(sim, start(sim), "LandRCBM_split3pools", "postSpinupAdjustBiomass", eventPriority = 5.5)
      
      # split AGB of cohorts into pools 
      sim <- scheduleEvent(sim, start(sim), "LandRCBM_split3pools","annualIncrements", eventPriority = 7)
      
      # prepare inputs for CBM annual event (cbm_vars)
      sim <- scheduleEvent(sim, start(sim), "LandRCBM_split3pools","prepareCBMvars", eventPriority = 8.25)
      
      # summarize simulation 
      sim <- scheduleEvent(sim, start(sim), "LandRCBM_split3pools","summarizeAGBPools", eventPriority = 10)
      # plots
      if (anyPlotting(P(sim)$.plots)) {
        sim <- scheduleEvent(sim, P(sim)$.plotInitialTime,
                             "LandRCBM_split3pools", "plotYC", eventPriority = 5)
        if (P(sim)$.plotMaps) {
          sim <- scheduleEvent(sim, P(sim)$.plotInitialTime,
                               "LandRCBM_split3pools", "plotMaps", eventPriority = 11)
        }
        sim <- scheduleEvent(sim, end(sim),
                             "LandRCBM_split3pools", "plotSummaries", eventPriority = 12)
      }
    },
    splitInit = {
      
      # split initial above ground biomass
      sim$aboveGroundBiomass <- splitCohortData(
        cohortData    = sim$cohortData,
        pixelGroupMap = sim$pixelGroupMap,
        standDT       = sim$standDT[, .(pixelIndex, juris_id = admin_abbrev, ecozone = eco_id)],
        table6tb      = sim$table6tb,
        table7tb      = sim$table7tb,
        tableMerch    = sim$tableMerch,
        sppEquiv      = sim$sppEquiv
      )
      
      # split yield tables into AGB pools
      sim <- SplitYieldTables(sim)
    },
    plotYC = {
      
      # plot the yield tables
      sim <- PlotYieldTables(sim)
      
      # plot the yield tables with pools seperated
      sim <- PlotYieldTablesPools(sim)
    },
    postSpinupAdjustBiomass = {
      spinupOut <- sim$cbm_vars[c("pools", "flux", "parameters", "state")]
      
      # 1. Expand spinup output to have 1 row per cohort
      spinupOut <- lapply(spinupOut, function(tbl) {
        tbl <- tbl[sim$cbm_vars$key$row_idx, ]
      })
      
      # 2. Replace above ground pools with the LandR biomass.
      nonAge0 <- spinupOut$state$age > 0
      spinupOut$pools[nonAge0, c("Merch", "Foliage", "Other")] <- sim$aboveGroundBiomass[, .(merch, foliage, other)]
      
      # 3. Update below ground live pools.
      rootsC <- CBMutils::calcRootC(cbind(spinupOut$pools, sw = spinupOut$state$sw_hw == 0))[, .(
        CoarseRoots = SoftwoodCoarseRoots + HardwoodCoarseRoots,
        FineRoots   = SoftwoodFineRoots   + HardwoodFineRoots
      )]
      spinupOut$pools[, c("CoarseRoots", "FineRoots")] <- rootsC
      
      # 4. Update cbm_vars
      sim$cbm_vars <- updateSpinupCohortGroups(spinupOut, sim$cbm_vars$key)
      
    }, 
    annualIncrements = {
      
      # split AGB of cohorts into pools
      sim <- AnnualIncrements(sim)
      
      # prepare cohort groups
      sim <- UpdateCohortGroups(sim)
      
      # do this for each timestep
      sim <- scheduleEvent(sim, time(sim) + 1, eventPriority = 7, "LandRCBM_split3pools", "annualIncrements")
    },
    prepareCBMvars = {
      
      # split AGB of cohorts into pools
      sim <- PrepareCBMvars(sim)
      
      # do this for each timestep
      sim <- scheduleEvent(sim, time(sim) + 1, eventPriority = 8.25, "LandRCBM_split3pools", "prepareCBMvars")
    },
    
    summarizeAGBPools = {
      sumBySpecies <- sim$aboveGroundBiomass[, lapply(.SD, sum, na.rm = TRUE), by = speciesCode, .SDcols = c("merch", "foliage", "other")]
      sumBySpecies$year <- time(sim)[1]
      
      if (time(sim) == start(sim)){
        sim$summaryAGB <- sumBySpecies
      } else {
        sim$summaryAGB <- rbind(
          sim$summaryAGB,
          sumBySpecies
        )
      }
      
      # do this for each timestep
      sim <- scheduleEvent(sim, time(sim) + 1, eventPriority = 10, "LandRCBM_split3pools", "summarizeAGBPools")
    },
    plotMaps = {
      
      # get the sum of each pool per pixelGroups
      poolSum <- sim$aboveGroundBiomass[, lapply(.SD, sum, na.rm = TRUE), by = pixelIndex, .SDcols = c("merch", "foliage", "other")]
      # rasterize
      merchRast <- rast(sim$rasterToMatch, names = "merchantable")
      merchRast[poolSum$pixelIndex] <- poolSum$merch
      foliageRast <- rast(sim$rasterToMatch, names = "foliage")
      foliageRast[poolSum$pixelIndex] <- poolSum$foliage
      otherRast <- rast(sim$rasterToMatch, names = "other")
      otherRast[poolSum$pixelIndex] <- poolSum$other
      
      # plot
      Plots(merchRast,
            fn = gg_agbpools,
            types = P(sim)$.plots,
            filename = paste0("merch", "_year_", round(time(sim))),
            title = paste("Total merchantable biomass", "year", round(time(sim))))
      Plots(foliageRast,
            fn = gg_agbpools,
            types = P(sim)$.plots,
            filename = paste0("foliage", "_year_", round(time(sim))),
            title = paste("Foliage biomass", "year", round(time(sim))))
      Plots(otherRast,
            fn = gg_agbpools,
            types = P(sim)$.plots,
            filename = paste0("other", "_year_", round(time(sim))),
            title = paste("Other above ground biomass", "year", round(time(sim))))
      
      # map increments
      if (time(sim) != start(sim)){
        increments <- sim$cohortDT[sim$gcIncrements, on = c("gcID", "age")]
        incrementSum  <- increments[, lapply(.SD, sum, na.rm = TRUE), by = pixelIndex, .SDcols = c("merch_inc", "foliage_inc", "other_inc")]
        # rasterize
        merchIncRast <- rast(sim$rasterToMatch, names = "merchantable increments")
        merchIncRast[incrementSum$pixelIndex] <- incrementSum$merch_inc
        foliageIncRast <- rast(sim$rasterToMatch, names = "foliage increments")
        foliageIncRast[incrementSum$pixelIndex] <- incrementSum$foliage_inc
        otherIncRast <- rast(sim$rasterToMatch, names = "other increments")
        otherIncRast[incrementSum$pixelIndex] <- incrementSum$other_inc
        
        # plot
        Plots(merchIncRast,
              fn = gg_agbpools,
              types = P(sim)$.plots,
              filename = paste0("merchInc", "_year_", round(time(sim))),
              title = paste("Total merchantable increment", "year", round(time(sim))))
        Plots(foliageIncRast,
              fn = gg_agbpools,
              types = P(sim)$.plots,
              filename = paste0("foliageInc", "_year_", round(time(sim))),
              title = paste("Foliage increment", "year", round(time(sim))))
        Plots(otherIncRast,
              fn = gg_agbpools,
              types = P(sim)$.plots,
              filename = paste0("otherInc", "_year_", round(time(sim))),
              title = paste("Other above increment", "year", round(time(sim))))
      }
      
      
      # schedule next maps
      sim <- scheduleEvent(sim, time(sim) + P(sim)$.plotInterval,
                           "LandRCBM_split3pools", "plotMaps", eventPriority = 11)
    },
    plotSummaries = {
      if (time(sim) > start(sim)){
        summaryAGBPoolsLandscape <- sim$summaryAGB[, lapply(.SD, sum, na.rm = TRUE), by = year, .SDcols = c("merch", "foliage", "other")]
        # Landscape summary
        Plots(summaryAGBPoolsLandscape,
              fn = gg_landscapesummary,
              types = P(sim)$.plots,
              filename = paste0("LandscapeAGBPoolSummary")
        )
        
        # Species summary
        Plots(sim$summaryAGB,
              fn = gg_speciessummary,
              types = P(sim)$.plots,
              filename = paste0("SpeciesAGBPoolSummary")
        )
      }
    },
    warning(paste("Undefined event type: \'", current(sim)[1, "eventType", with = FALSE],
                  "\' in module \'", current(sim)[1, "moduleName", with = FALSE], "\'", sep = ""))
  )
  return(invisible(sim))
}

PlotYieldTables <- function(sim){
  nPixGroups <- length(unique(sim$yieldTablesId$yieldTableIndex))
  nPlots <- P(sim)$.plotNumPixGroup
  if (nPlots <= 0){
    stop("numPlots needs to be a positive integer")
  } else if (nPlots > nPixGroups) {
    message(".plotNumPixGroup is greater than the number of pixel groups, ",
            "plotting all pixelgroups.")
    nPlots <- nPixGroups
  } 
  pixGroupToPlot <- sample(unique(sim$yieldTablesId$yieldTableIndex), nPlots)
  
  mod$yieldTableIndexPlotted <- pixGroupToPlot
  
  # plot
  Plots(sim$yieldTablesCumulative[yieldTableIndex %in% pixGroupToPlot], 
        fn = gg_yieldCurves,
        types = P(sim)$.plots,
        filename = paste("yieldCurves"),
        title = paste("Yield curves for", nPlots, "randomly selected pixel groups")
  )
  return(invisible(sim))
}

SplitYieldTables <- function(sim) {
  # Step 1: Spatial Matching and Cohort/Stand Data Preparation -----------------
  # Link yield curve IDs (yieldTableIndex) to CBM spatial units 
  # and generate initial cohort/stand data structures.
  
  # 1.1. Generate the cohort-level attributes (`cohortDT`).
  #      This links individual cohorts (pixelGroup x species combinations)
  #      to their corresponding growth curve IDs (`gcID`).
  cohortDT <- generateCohortDT(sim$cohortData, sim$pixelGroupMap, sim$standDT, sim$yieldTablesId)
  
  # 1.2. Create and store metadata about growth curves (`sim$gcMeta`).
  #      Links gcID to species information.
  sim$gcMeta <- unique(cohortDT[, .(gcID, admin_abbrev, eco_id, yieldTableIndex, speciesCode)])
  sim$gcMeta <- cbind(
    sim$gcMeta,
    CBMutils::sppMatch(
      sim$gcMeta$speciesCode, sppEquiv = sim$sppEquiv,
      match = "LandR", return = c("Broadleaf", "CanfiCode"))[
        , .(sw = !Broadleaf, canfi_species = CanfiCode)]
  )
  setkey(sim$gcMeta, gcID)
  setcolorder(sim$gcMeta)
  
  # 1.3. Store essential cohort information in simList.
  cohortDT <- cohortDT[, .(cohortID, pixelIndex, age, speciesCode, gcID)]
  sim$cohortDT <- cohortDT
  
  # Step 2: Splitting AGB Curves into CBM Pools --------------------------------
  # Convert the total Above-Ground Biomass (AGB) yield curves into cumulative biomass
  # for the three CBM above ground pools: Merchantable (merch), Foliage, and Other.
  
  # 2.1. Prepare table for CBM pool splitting function.
  #      Rename the primary biomass column to 'B' as expected by CBMutils.
  cumPools <- merge(
    sim$gcMeta, 
    sim$yieldTablesCumulative[, .(yieldTableIndex, speciesCode, age, biomass)], 
    by = c("speciesCode", "yieldTableIndex"),
    allow.cartesian = TRUE)
  cumPools[, speciesCode := NULL]
  setnames(cumPools,
           old = c("admin_abbrev", "eco_id",  "biomass"),
           new = c("juris_id",     "ecozone", "B"))
  
  # Convert biomass units from g/m^2 to tonnes/ha: 1 g/m^2 = 0.01 tonnes/ha
  cumPools[, B := B / 100]
  
  # 2.2. Split AGB ('B') into cumulative CBM pools (merch, foliage, other).
  #      Uses equations from Boudewyn et al. 2007 adjusted to use total above
  #      ground biomass as input, implemented in CBMutils.
  cumPools[age == 0 & B <= 0.01, B := 0]
  CBMutils::cumPoolsCreateAGB(
    cumPools,
    pixGroupCol = "gcID",
    bTable6tb   = sim$table6tb,
    bTable7tb   = sim$table7tb,
    tableMerch  = sim$tableMerch
  )
  
  # Step 3: Calculating Annual Increments --------------------------------------
  # Calculate the year-to-year increment in biomass for each above ground 
  # biomass pool. These increments drive the spinup dynamics.
  
  # 3.1. Define pool and increment column names.
  poolCols <- c("merch", "foliage", "other")
  incCols <- c("merch_inc", "foliage_inc", "other_inc")
  
  # 3.2. Calculate increments using `diff`.
  setkey(cumPools, gcID, age)
  cumPools[, (incCols) := lapply(.SD, function(x) c(0, diff(x))), .SDcols = poolCols, by = "gcID"]
  
  # 3.3. Final selection and ordering of columns for `sim$gcIncrements`.
  sim$gcIncrements <- cumPools[,.(gcID, age, merch, foliage, other, merch_inc, foliage_inc, other_inc)]
  
  return(invisible(sim))
}


# Plot the curves that are directly out of the Boudewyn-translation
PlotYieldTablesPools <- function(sim){
  
  # We want to plot the same cohorts across figures
  pixGroupToPlot <- mod$yieldTableIndexPlotted
  yieldTablePools <- sim$gcIncrements[sim$gcMeta, on = "gcID"][yieldTableIndex %in% pixGroupToPlot]
  
  # plot total yield curves
  plot_dt <- melt(
    yieldTablePools, 
    id.vars = c("yieldTableIndex", "speciesCode", "age"),
    measure.vars = c("merch", "foliage", "other"),
    variable.name = "pool",
    value.name = "B"
  )
  
  Plots(plot_dt, 
        fn = gg_yieldCurvesPools,
        types = P(sim)$.plots,
        filename = "yieldCurvePools",
        title = paste("Yield curves for", length(unique(plot_dt$yieldPixelGroup)), "randomly selected pixel groups")
  )
  
  # plot increments
  plot_dt <- melt(
    yieldTablePools, 
    id.vars = c("yieldTableIndex", "age", "speciesCode", "gcID"),
    measure.vars = c("merch_inc", "foliage_inc", "other_inc"),
    variable.name = "pool",
    value.name = "B"
  )
  plot_dt <- plot_dt[plot_dt$age > 0,]
  Plots(plot_dt, 
        fn = gg_yieldCurvesPools,
        types = P(sim)$.plots,
        filename = "yieldCurveIncrements",
        title = "Increments merch fol other by species and pixel groups"
  )
  message(crayon::red("User: please inspect figures of the raw translation of your increments in: ",
                      figurePath(sim)))
  
  return(invisible(sim))
}

# Process yearly vegetation inputs
AnnualIncrements <- function(sim){
  # Step 1: Store the above ground biomass of the previous time step.-----------
  biomassTminus1 <- copy(sim$aboveGroundBiomass)
  # Increment age to match the *current* age for joining later
  biomassTminus1[, age := age + 1L]
  # Rename cols to indicate they are from the previous timestep
  setnames(biomassTminus1, old = c("merch", "foliage", "other"), 
           new = c("merchTminus1", "foliageTminus1", "otherTminus1"))
  # Keep only necessary columns for merging
  biomassTminus1 <- biomassTminus1[, .(pixelIndex, speciesCode, age,
                                       merchTminus1, foliageTminus1, otherTminus1)]
  setkey(biomassTminus1, pixelIndex, speciesCode, age)
  
  # Step 2: Split current total above ground.-----------------------------------
  sim$aboveGroundBiomass <- splitCohortData(
    cohortData    = sim$cohortData,
    pixelGroupMap = sim$pixelGroupMap,
    standDT       = sim$standDT[, .(pixelIndex, juris_id = admin_abbrev, ecozone = eco_id)],
    table6tb      = sim$table6tb,
    table7tb      = sim$table7tb,
    tableMerch    = sim$tableMerch,
    sppEquiv      = sim$sppEquiv
  )
  
  # Step 3: Calculate this year's increments.-----------------------------------
  # Full outer join between current biomass and previous biomass (incremented age)
  incrementsDT <- merge(
    sim$aboveGroundBiomass,
    biomassTminus1,
    by = c("pixelIndex", "speciesCode", "age"),
    all = TRUE 
  )
  # Handle NA values resulting from the join:
  # - NAs in current pools (merch, foliage, other) mean cohort disappeared -> fill with 0
  # - NAs in previous pools (merchTminus1, etc.) mean cohort is new -> fill with 0
  fillCols <- c("merch", "foliage", "other", "merchTminus1", "foliageTminus1", "otherTminus1")
  setnafill(incrementsDT, fill = 0, cols = fillCols)
  # Calculate increments by subtracting previous from current
  incrementsDT[, `:=`(
    merch_inc   = merch   - merchTminus1,
    foliage_inc = foliage - foliageTminus1,
    other_inc   = other   - otherTminus1
  )]
  # Create gcID and cohortID (the same for annual increments).
  groupCols <- c("speciesCode", "age", "merch_inc", "foliage_inc", "other_inc")
  incrementsDT[, gcID := .GRP, by = groupCols]
  incrementsDT[, cohortID := .I]
  # Create data.table with cohort-level information
  sim$cohortDT <- incrementsDT[, .(cohortID, pixelIndex, age, speciesCode, gcID)]
  # Create data.table with growth curve-level information
  sim$gcMeta <- unique(incrementsDT, by = "gcID")[, .(gcID, speciesCode)]
  
  sim$gcMeta[, sw := !CBMutils::sppMatch(
    sim$gcMeta$speciesCode, sppEquiv = sim$sppEquiv,
    match = "LandR", return = "Broadleaf")$Broadleaf]
  
  # Create final growth increment data.table
  sim$gcIncrements <- unique(incrementsDT, by = "gcID")[, .(gcID, age, merch_inc, foliage_inc, other_inc)]
  setkey(sim$gcIncrements, gcID)
  
  return(invisible(sim))
}

# Update cohort groups for CBM annual event
UpdateCohortGroups <- function(sim){
  sim$cbm_vars$key[, row_idx_prev := row_idx]
  # Get the pools for the cohort groups of the previous timestep
  cohortsPrev <- unique(sim$cbm_vars$key, by = c("pixelIndex", "row_idx_prev"))[, .(pixelIndex, row_idx_prev)]
  cohortsPrev <- merge(cohortsPrev,
                       sim$cbm_vars$state[, .(row_idx, age, speciesCode)],
                       by.x = "row_idx_prev",
                       by.y = "row_idx",
                       sort = FALSE)
  # Add 1 to age for the match with the current timestep
  cohortsPrev[, age := age + 1L]
  
  # Match the cohorts based on pixel, age, and species.
  cohorts <- merge(
    cohortsPrev,
    sim$cohortDT[, .(pixelIndex, age, gcID, speciesCode, cohortID)],
    by = c("pixelIndex", "age", "speciesCode"),
    all = TRUE,
    allow.cartesian = TRUE,
    sort = FALSE
  )
  
  # Add spatial unit
  cohorts <- merge(cohorts, sim$standDT, by = "pixelIndex", sort = FALSE)
  # Cohort groups have the same increments and the same group in the previous timestep
  cohorts[, row_idx := NA_integer_]
  cohorts[!is.na(gcID), row_idx := .GRP, by = .(row_idx_prev, gcID, admin_abbrev, eco_id)]
  
  # Handle DOM cohorts
  if(any(is.na(cohorts$gcID))){
    missingCohorts <- cohorts[is.na(gcID), ]
    # Check that the DOM cohorts have live pools close to 0
    if(any(sim$cbm_vars$pools[missingCohorts$row_idx_prev, c("Merch", "Foliage", "Other")] > 10^-6)) {
      stop("Some cohorts with positive above ground biomasses are missing.")
    }
    missingCohorts[, gcID := 0L]
    missingCohorts[, cohortID := 0L]
    missingCohorts[, age := 0L]
    maxCohortGroupID <- max(cohorts$row_idx, na.rm = TRUE)
    missingCohorts[, row_idx := .GRP + maxCohortGroupID, by = pixelIndex]
    cohorts[is.na(gcID), ] <- missingCohorts
  }
  
  # Update cbm_vars key
  sim$cbm_vars$key <- merge(
    sim$cbm_vars$key[, `:=`(row_idx = NULL, cohortID = NULL)],
    cohorts[, .(pixelIndex, row_idx_prev, row_idx, cohortID)],
    by = c("pixelIndex", "row_idx_prev"),
    all.y = TRUE,
    sort = FALSE
  ) |> unique()
  setkey(sim$cbm_vars$key, cohortID)
  
  # Update cbm_vars state.
  sim$cbm_vars$state <- merge(
    cohorts[, .(row_idx, gcID, age, speciesCode, row_idx_prev)],
    sim$cbm_vars$state[, .(row_idx, delay, admin_name, eco_id, land_class_id, last_disturbance_type, time_since_last_disturbance, time_since_land_use_change, enabled)],
    by.x = "row_idx_prev",
    by.y = "row_idx",
    all.x = TRUE
  )
  sim$cbm_vars$state[, row_idx_prev := NULL]
  sim$cbm_vars$state[, age := age - 1L]
  sim$cbm_vars$state <- unique(sim$cbm_vars$state, by = "row_idx")
  sim$cbm_vars$state[sim$gcMeta, sw := sw, on = "gcID"]
  setkey(sim$cbm_vars$state, row_idx)
  
  return(invisible(sim))
}

PrepareCBMvars <- function(sim){
  # 1. Prepare cbm pools
  cohortGroupsKey <- unique(sim$cbm_vars$key, by = c("row_idx", "row_idx_prev"))[, .(row_idx, row_idx_prev)]
  # Get the pools of cohorts of the previous timestep
  new_cbm_pools <- merge(cohortGroupsKey,
                         sim$cbm_vars$pools,
                         by.x = "row_idx_prev",
                         by.y = "row_idx",
                         all.x = TRUE,
                         sort = FALSE)
  new_cbm_pools[, row_idx_prev := NULL]
  
  # Fill pools of new cohorts with 0s
  if(any(is.na(new_cbm_pools[, Merch]))) {
    setnafill(new_cbm_pools, cols = "Input", fill = 1L)
    setnafill(new_cbm_pools, fill = 0L)
  }
  
  # Aggregate DOM cohorts of the sharing pixel
  if(any(duplicated(new_cbm_pools$row_idx))){
    DOMcohorts <- new_cbm_pools[, .N, by = row_idx][N > 1, row_idx]
    pool_columns <- setdiff(colnames(new_cbm_pools), "row_idx")
    new_cbm_pools <- new_cbm_pools[, lapply(.SD, sum), by = row_idx, .SDcols = pool_columns]
    new_cbm_pools$Input <- 1L
    # Set live pools to 0 for DOM cohorts.
    new_cbm_pools[row_idx %in% DOMcohorts, c("Merch", "Foliage", "Other", "CoarseRoots", "FineRoots") := 0L]
  }
  setkey(new_cbm_pools, row_idx)
  new_cbm_pools <- unique(new_cbm_pools, by = "row_idx")
  
  # 2. Prepare cbm flux
  # Get the flux of the cohorts of the previous timestep
  new_cbm_flux <- merge(cohortGroupsKey,
                        sim$cbm_vars$flux,
                        by.x = "row_idx_prev",
                        by.y = "row_idx",
                        all.x = TRUE,
                        sort = FALSE)
  new_cbm_flux[, row_idx_prev := NULL]
  
  # Fill fluxes of new cohorts with 0s.
  if(any(is.na(new_cbm_flux))) {
    setnafill(new_cbm_flux, fill = 0L)
  }
  
  # Aggregate DOM cohorts of the sharing pixel
  if(any(duplicated(new_cbm_flux$row_idx))){
    flux_columns <- setdiff(colnames(new_cbm_flux), "row_idx")
    new_cbm_flux <- new_cbm_flux[, lapply(.SD, sum), by = row_idx, .SDcols = flux_columns]
  }
  setkey(new_cbm_flux, row_idx)
  new_cbm_flux <- unique(new_cbm_flux, by = "row_idx")
  
  # 3. Prepare cbm parameters
  new_cbm_parameters <- sim$cbm_vars$state[, .(row_idx, admin_name, eco_id, gcID)]
  
  # Set no disturbance by default (will be changed later for disturbed cohorts)
  new_cbm_parameters[, disturbance_type := 0]
  
  # Get the increments
  new_cbm_parameters <- merge(new_cbm_parameters,
                              sim$gcIncrements,
                              by = "gcID",
                              all.x = TRUE,
                              sort = FALSE)
  
  # For the DOM cohorts (gcID = 0) set increments to 0
  setnafill(new_cbm_parameters, fill = 0L, cols = c("merch_inc", "foliage_inc", "other_inc"))
  
  new_cbm_parameters <- new_cbm_parameters[, .(
    row_idx,
    disturbance_type,
    merch_inc,
    foliage_inc,
    other_inc
  )]
  setkey(new_cbm_parameters, row_idx)
  
  # Update parameters of disturbed cohorts
  if ("disturbance_type_id" %in% names(sim$cbm_vars$key)) {
    
    # Get attributes for disturbed cohorts
    distCohorts <- sim$cbm_vars$key[!is.na(disturbance_type_id), .(row_idx, row_idx_prev, disturbance_type_id)]
    # Remove new cohorts
    distCohorts <- distCohorts[!is.na(row_idx_prev)]
    distCohortGroup <- unique(distCohorts[,.(row_idx, disturbance_type_id)], by = "row_idx")
    new_cbm_parameters[distCohortGroup$row_idx, "disturbance_type"] <- distCohortGroup$disturbance_type_id
    # DC 29-04-2025: Not sure what should be the increments for disturbed cohorts.
    new_cbm_parameters[distCohortGroup$row_idx, merch_inc := 0L]
    new_cbm_parameters[distCohortGroup$row_idx, foliage_inc := 0L]
    new_cbm_parameters[distCohortGroup$row_idx, other_inc := 0L]
  }
  
  # 4. Prepare cbm state
  # Get the state of the cohorts of the previous timestep
  new_cbm_state <- sim$cbm_vars$state
  
  # Change the state of DOM cohorts
  if(any(new_cbm_state$gcID == 0)){
    DOMcohorts <- sim$cbm_vars$state[gcID == 0, row_idx]
    new_cbm_state[row_idx %in% DOMcohorts, age := 0L]
    new_cbm_state[row_idx %in% DOMcohorts, time_since_last_disturbance := 0L]
    new_cbm_state[row_idx %in% DOMcohorts, time_since_land_use_change  := -1L]
    new_cbm_state[row_idx %in% DOMcohorts, last_disturbance_type := -1L]
    new_cbm_state <- unique(new_cbm_state, by = "row_idx")
  }
  
  # Set the state of the new cohorts
  if(any(is.na(new_cbm_state))) {
    
    newCohorts_cbm_state <- new_cbm_state[is.na(enabled), ]
    setnafill(newCohorts_cbm_state, fill = 1L, cols = c("last_disturbance_type", "enabled"))
    setnafill(newCohorts_cbm_state, fill = -1L, cols = c("land_class_id", "time_since_land_use_change")) 
    setnafill(newCohorts_cbm_state, fill = 0L, cols = "delay") 
    
    # Get growth curve information
    newCohort_gcids <- newCohorts_cbm_state$gcID
    newCohorts_gcMeta <- sim$gcMeta[match(newCohort_gcids, sim$gcMeta$gcID)]
    newCohorts_cbm_state[, time_since_last_disturbance := age]
    
    # Add eco_id and admin_name
    newCohorts_cbm_state <- getSpatialInformation(newCohorts_cbm_state, sim$cbm_vars$key, sim$standDT)
    
    # Combine with cohorts that were present before
    new_cbm_state <- rbind(
      new_cbm_state[!is.na(enabled),],
      newCohorts_cbm_state
    )
  }
  setkey(new_cbm_state, row_idx)
  new_cbm_state <- unique(new_cbm_state, by = "row_idx")
  
  # 5. Put in cbm_vars
  sim$cbm_vars <- list(
    key = sim$cbm_vars$key |> unique(by = c("row_idx", "cohortID")),
    pools = new_cbm_pools[!is.na(row_idx)],
    flux = new_cbm_flux[!is.na(row_idx)],
    parameters = new_cbm_parameters[!is.na(row_idx)],
    state = new_cbm_state[!is.na(row_idx)]
  )
  
  return(invisible(sim))
}

.inputObjects <- function(sim) {
  
  # NFI params. Used to split total biomass into biomass of the three CBM
  #                above ground biomass pools.
  if (!suppliedElsewhere("table6tb", sim)) {
    sim$table6tb <- prepInputs(
      url = extractURL("table6tb"),
      fun = data.table::fread(targetFile, verbose = FALSE),
      destinationPath = inputPath(sim),
      targetFile = "appendix2_table6_tb.csv",
      overwrite = TRUE
    ) |> Cache(userTags = "prepInputsTable6tb")
  }
  
  if (!suppliedElsewhere("table7tb", sim)) {
    sim$table7tb <- prepInputs(
      url = extractURL("table7tb"),
      fun = data.table::fread(targetFile, verbose = FALSE),
      destinationPath = inputPath(sim),
      targetFile = "appendix2_table7_tb.csv",
      overwrite = TRUE
    ) |> Cache(userTags = "prepInputsTable7tb")
  }
  
  if (!suppliedElsewhere("tableMerch", sim)) {
    sim$tableMerch <- prepInputs(
      url = extractURL("tableMerch"),
      fun = data.table::fread(targetFile, verbose = FALSE),
      destinationPath = inputPath(sim),
      targetFile = "merchantabilityParams.csv",
      overwrite = TRUE
    ) |> Cache(userTags = "prepInputsTableMerch")
    sim$tableMerch <- cbind(sim$tableMerch, minAge = P(sim)$minMerchantableAge)
  }
  
  return(invisible(sim))
}
