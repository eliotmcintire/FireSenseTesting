## Packages are installed by expt.R before any worker starts; workers only read the library.
## Running this file alone needs only SpaDES.project and Require.
## Run from the project folder (workers and the IDE do); site.R holds what differs per machine.
site <- source("site.R", local = TRUE)$value

# tmux panes report interactive() == TRUE; batch workers run inside tmux
userInteractive <- interactive() && !nzchar(Sys.getenv("TMUX"))

## Phase 4 (a queue row with .score = TRUE, from expt.R's FS_SET=score): score the hindcasts of every ELF that has finished
## all its years, pooled, with fireSimulator_validation (modules/), and write its validation page (validation.html).
## The ledger suffix and the base path come from the row; ELFs are read in parallel on the job's CPUs.
if (isTRUE(get0(".score"))) {
  .base <- get0(".basePath", ifnotfound = site$basePath); .suffix <- get0(".spreadFitFileSuffix", ifnotfound = "")
  for (.f in list.files("modules/fireSimulator_validation/R", "\\.R$", full.names = TRUE)) source(.f)
  suppressMessages({library(ggplot2); library(patchwork)}) # the page's figures; attached as the module's test helper does
  .hcDir <- file.path(.base, "outputs", paste0("hindcast", .suffix))
  .hcYears <- as.integer(strsplit(.hcYears, ",")[[1]]) # the row's years (expt.R FS_HC_YEARS; all 1986-2024 by default)
  .done <- Filter(function(e) length(hindcastFinishedYears(.hcDir, e, .hcYears, 1L)) == length(.hcYears),
                  setdiff(list.files(.hcDir), strsplit(get0(".scoreSkip", ifnotfound = ""), ",")[[1]]))
  .elfs <- parallel::mclapply(.done, mc.cores = as.integer(Sys.getenv("SLURM_CPUS_PER_TASK", "1")), mc.preschedule = FALSE, function(e)
    readFireSenseHindcast(e, .hcDir, years = .hcYears, sets = 1L, inputsDir = file.path(.base, "inputs"),
                          ledgerFile = Sys.glob(file.path(.base, "inputs", e, paste0("fireSenseParams_*", .suffix, ".rds")))[1],
                          nfdbPath = tail(Sys.glob(file.path(.base, "inputs", e, "NFDB_point_*.shp")), 1),
                          nbacPath = tail(Sys.glob(file.path(.base, "inputs", e, "NBAC_merged_*.shp")), 1),
                          obsCacheDir = {d <- file.path(.base, "outputs", paste0("score", .suffix), "obs"); dir.create(d, recursive = TRUE, showWarnings = FALSE); d}))
  .elfs <- Filter(Negate(is.null), .elfs)
  message("scoring ", length(.elfs), " ELFs")
  .res <- runWave1(.elfs)
  saveRDS(.res, file.path(.base, "outputs", paste0("score", .suffix), "wave1.rds"))
  write.csv(.res$results, file.path(.base, "outputs", paste0("score", .suffix), "wave1_results.csv"), row.names = FALSE)
  buildValidationPage(.elfs, .res$gates, file.path(.base, "outputs", paste0("score", .suffix), "validation.html"),
                      template = "modules/fireSimulator_validation/pageTemplate.html", dpi = 50, title = "Hindcast validation, GPSC fits",
                      subtitle = paste0("Spread fits ", sub("^_", "", .suffix), " &middot; hindcasts ", paste(range(.hcYears), collapse = "&ndash;"),
                                        ", ", length(.hcYears), " years, 20 runs per ELF"))
  stop("scoring done", call. = FALSE) # ends the job; the queue marks a stop as an ERROR row, the outputs are what count
}

inSim <- SpaDES.project::setupProject(
  defaultDots = list(.basePath = site$basePath, # inputs, outputs, cache, scratch and shared data live here
                     .rep = 1,
                     .ELFind = "4.3",
                     .SSP = 370,
                     .GCM = "CNRM-ESM2-1", # or "NRV"
                     .samplingRange = 1986:2024, # NRV climate years: the historical climate the run loads (1986-2024); 1985 is not in it, and climateYear >= 0.1.0.9002 stops on a missing year (yearSeed, #10) where older versions dropped it
                     # NRV run archive. Each campaign uploads to its own Drive folder, with its own ledger there, so the old
                     # campaign stays usable until the new one is complete (then its ledger replaces nrvRuns.rds, which setupProject(alreadyRun = ) reads)
                     .uploadGSdir = "https://drive.google.com/drive/folders/188ERmd1k6s6YMv3wHtnHQHD7pgLseBjf?usp=drive_link",
                     .archiveLedger = NA_character_, # e.g. "nrvRuns_2026-11-04.rds": each upload adds a row to it in .uploadGSdir; NA: no row
                     # fitting window: vegetation is strictly before the fire year, so the first SCANFI V2 year
                     # (1985) feeds 1986; the end is the last year of historical climate.
                     # expt.R checks this line against climateData::latestHistoricalYear().
                     .fireYears = c(1986L, 2024L),
                     .doFit = NA,                     # NA: fit only if no ledger fit; TRUE: always refit; FALSE: never fit
                     .spreadFitMode = "fit",          # or "fit,validate"
                     .heldOutFold = NA_integer_,      # 1 or 2 fits only that fold
                     .spreadFitFileSuffix = "_bufIndep20261024", # buffer-independent spread objective (fireSenseUtils#183, fireSense_spreadFit#91), 2026-10-24; "_prod20261021" was the previous ledger
                     .spreadLink = NA_character_,     # NA: the module default ("powerLaw"); or "ceiling", "climateOutside"
                     .spreadWeighted = NA_character_, # NA: the module default (FALSE, every fire weighs 1); or "log", "sqrt" (fireSenseUtils sizeWeight())
                     .spreadGateRefusal = NA_character_, # NA: the module default ("minLik"); "simulate": gate-refused years are simulated and scored
                     .spreadReps = NA_character_, # NA: the module default objfunFireReps; e.g. "100"
                     .spreadCrpsWeights = NA_character_, # NA: fireSenseUtils::crpsWeightsDefault(); e.g. "fire=1,adTest=0.34,areaDist=2.6,totalArea=164,top10=16.5"
                     .spreadTests = NA_character_,    # NA: the module default DEoptimTests; or e.g. "SNLL_FS,pitAD" (comma-separated)
                     .spreadInitialPop = NA_character_, # NA: the module default (DEoptim draws, or calibration passing sets); or an .rds matrix, NP x names(lower)
                     .spreadInitialPopMethod = NA_character_, # NA: the module default ("calibration"); or "emulator" (fireSense_spreadFit#98)
                     .spreadSdBounds = NA_character_, # NA: the module default (0,10); or e.g. "0,1" (bounds of the year-effect SD); "NA": no year effect
                     .prepAreaMult = NA_character_,   # NA: the module default (10 * multiplier(size)); or e.g. "40": the fit buffers' multiplier
                     .spreadPowerLawShape = NA_character_, # NA: the module default powerLawShape (1); or e.g. "2" (curvature of the power-law spread link)
                     .spreadSizeLikDf = NA_character_, # NA: the module default sizeLikDf (5); or e.g. "30" (df of the per-fire t size likelihood)
                     .wholeLandscape = NA,            # NA/FALSE: buffered spread fit; TRUE: every fire spreads on the whole ELF (fireSense_spreadFit wholeLandscapeSpread + fireSense_dataPrepFit wholeLandscapeSpreadCovariates)
                     .hindcastYear = NA_integer_,     # a SCANFI year makes this job a hindcast
                     .hcAllSets = FALSE,              # TRUE: one hindcast job runs every fitted spread parameter set, 20 reps (5 sets x 4; fireSense_spreadPredict paramSetByReplicate)
                     .hcIgnitions = NA_character_,    # NA: the module default ("predicted"); "observedEscapes": a hindcast spreads each year's observed escapes (spread alone)
                     .cohortRemoveSuppressed = NA_character_,   # NA: off; "relativeB,minAgeFrac" e.g. "0.01,0.75" (Biomass_core#131)
                     .cohortSquash = NA_character_,             # NA: off; "interval,width[,width...]" e.g. "100,20" (breaks LandR-CBM)
                     .cohortEstablishmentLimit = NA_character_, # NA: off; "interval,minCohorts" e.g. "20,5"
                     .fireCauses = NA_character_,     # NA: the module defaults; e.g. "natural,unknown": fireSenseUtils::fireCauseCodes() groups for the fits (dataPrepFit) and the hindcast's observed fires (fireSense_summary)
                     .phase1Only = FALSE,             # TRUE: build every input and cache, then stop before the fit (a 1-CPU Slurm job)
                     .phase2Only = FALSE,             # TRUE: fit if this ELF has no fit, then stop; neither phase: fit if needed, then predict
                     .workerMemoryGB = NA_real_,      # NA: the fireSenseUtils default per worker; the ELF's mean worker GB, so a run name with no fit record is budgeted as its Slurm request was
                     .cacheChaining = TRUE,           # FALSE: spades.cacheChaining off
                     .fitQueue = NA_character_,       # phase 1: a fit queue .rds; skip an ELF the fits have claimed or the idle fit workers will claim next
                     .afterQueue = NA_character_,     # phase 2: a phase-1 queue .rds; wait while phase 1 is building this ELF (RUNNING)
                     .diagFile = NA_character_,       # diagnostic: an R file to source, e.g. trace() calls
                     .rprofPrefix = NA_character_,    # diagnostic: profile the run to <prefix>_<ELF>_<pid>.out
                     .cores = site$cores,
                     .times = list(start = 2020, end = 3020),
                     # parent: canClimateData, climateYear, fireSense, localEcozones,
                     # Biomass_borealDataPrep, Biomass_regeneration, Biomass_core
                     .modules = c("PredictiveEcology/LandR_ClimateSensitiveSimulator@development",
                                  # not in the parent: they need restricted PSP data
                                  "PredictiveEcology/Biomass_speciesFactorial@development",
                                  "PredictiveEcology/Biomass_speciesParameters@development",
                                  "PredictiveEcology/NRV_summary@modsForFireSense",
                                  "PredictiveEcology/burnSummaries@modsForFireSense",
                                  "PredictiveEcology/Biomass_summary@modsForFireSense")),
  # A hindcast holds year Y's vegetation static and draws Y's climate (see fireSenseUtils::hindcastSetup)
  .hc = !is.na(.hindcastYear),
  .hcSetup = if (.hc) fireSenseUtils::hindcastSetup(.hindcastYear, allSets = .hcAllSets, ignitions = .hcIgnitions),
  .doFit = if (.hc) FALSE else .doFit, # a hindcast only predicts from a fit in the ledger; it stops if there is none
  .NRV = identical(.GCM, "NRV"),
  .phase = if (.phase1Only && .phase2Only) stop(".phase1Only and .phase2Only are both TRUE")
           else if (.phase1Only) "inputs" else if (.phase2Only) "fit" else "all", # fireSenseUtils::fireSensePhases
  .ELFind = .ELFind,
  .rep = .rep, # without it, a .rep from the queue does not reach params$.globals
  # only dots written here are returned in inSim: a defaultDots value the queue did not set is not a variable after setupProject
  .fitQueue = .fitQueue, .afterQueue = .afterQueue, .diagFile = .diagFile, .rprofPrefix = .rprofPrefix,
  .uploadGSdir = .uploadGSdir, .archiveLedger = .archiveLedger, .GCM = .GCM, .SSP = .SSP,
  paths = list(outputPath = if (.hc) SpaDES.project::pathBuild(pre = file.path(.basePath, "outputs", paste0("hindcast", .spreadFitFileSuffix, if (identical(.hcIgnitions, "observedEscapes")) "_obsEsc")), .ELFind, .hindcastYear, .rep)
                            ## a fit's outputs (DEoptim progress, figures) are per ledger: fits of one ELF under different
                            ## suffixes (e.g. weightings) ran at once and wrote the same DEoptimProgress file, 2026-10-24
                            else if (isTRUE(.doFit)) SpaDES.project::pathBuild(pre = file.path(.basePath, "outputs", paste0("fit", .spreadFitFileSuffix)), .ELFind, .GCM, .SSP, .rep)
                            else SpaDES.project::pathBuild(pre = file.path(.basePath, "outputs"), .ELFind, .GCM, .SSP, .rep),
               cachePath = file.path(.basePath, "cache"),
               scratchPath = file.path(.basePath, "scratch"),
               inputPath = SpaDES.project::pathBuild(pre = file.path(.basePath, "inputs"), .ELFind)), # inputs vary by ELF only: not by rep, GCM or SSP (projected climate tiles carry GCM_ssp in their own folder); the folder is in prepared inputs' cache keys
  # the outputs folder may arrive as its symlink target, so name the run by what is below ".../outputs/"
  runName = gsub("/", "_", sub("^.*/outputs/", "", paths$outputPath)),
  times = if (.hc) .hcSetup$times else .times,
  modules = if (.hc) .hcSetup$modules else .modules, # a hindcast cannot use the parent: it would bring the vegetation modules
  # only what this file itself calls or configures; modules list their own in reqdPkgs
  packages = c("PredictiveEcology/SpaDES.core@development (>= 3.2.1.9056)",      # 9056: nested init keeps dot-named outputs (#516); event hits keep duplicate events (#505); spades.cacheKeyReachable (#500), cacheKey = FALSE (#503); module version out of event cache keys (#501); module globalVariables() (#496) without the self-referencing simList (#498), cache hit restores own outputs only (#495), chain jump restores changed objects (#497)
               "PredictiveEcology/reproducible@development (>= 3.2.1.9084)",     # a short download is never kept, downloads staged in destinationPath (#668); clearCache(cacheId) lists only those ids (#666); offline-safe prepInputs (#657-#659) and CacheGeo with no Google credentials (#664); Copy(), the reproducible.* options and their defaults
               "PredictiveEcology/SpaDES.project@development (>= 1.2.0.9033)",   # 9033: every library of packagePath kept (#218); 9031: experiment*(devPackages = ) starts workers on a dev library, no packagePath here (#216); 9028: optionsCache()/optionsBatch(), queue helpers for runJob() (#210); workers start from a launcher file (#202); params merge, .rep from the caller, per-worker terra/GDAL defaults
               "PredictiveEcology/clusters@development (>= 0.0.93)",          # 0.0.93: host-speed and fit-memory ledgers written at rebalance and fit end, not every generation (#70); 0.0.92: every library of the run reaches workers and remote hosts, run libraries first (#69); 0.0.91: a rebuilt cluster keeps every worker (#68); clusters.maxWorkersPerHost (#58), across all fits on a host (#59)
               "PredictiveEcology/fireSenseUtils@development (>= 0.2.3.9156)",   # 9156: wholeLandscape spread with SpaDES.tools spreadCpp(link =) (needs SpaDES.tools >= 2.2.0.9002), no per-replicate gc, no gates under gateRefusal "simulate", re-score caches keyed on shipped data (#219); 9155: plotELFs() metric colours not covered by `which`, colour bar beside the map (#218); 9154: the whole library path reaches the DEoptim cluster (#217); 9153: optional top-fire area share CRPS term (#216); 9151: fire figures count the same fires on both sides, escape = one pixel in ha (#214); 9150: fire figures use every replicate, median fire size (#213); 9148: plotELFs() metricCol/metricTitle/metricRange, the NRV map (#212); 9147: phase-1 stops before the fit (#211); 9144: SNLL_FS and the threshold calibration removed (#209); 9142: predict-only "latest" ledgers (#206); 9140: runJob(), hindcastSetup(), 9 GB per spread-fit worker (#199); makeFireSenseLCC() CWIM destinationPath (#178); ELFsNotFitted(): Prairie grassland 10.2 left out like the Arctic (#177); yearEffectMultiplier()/yearEffectSD() (#171), fuelCovsToCoarse() (#172) with fuel in the stackAndExtract cache key (#173); ledger suffix, xgbObjective(), hindcastObserved()$points, powerLaw link + calibratePowerLaw() (#167, #168), ELFmetricsFilename() (#170)
               "qs2"),                                                           # reproducible.cacheSaveFormat
  require = c("reproducible", "terra"),
  options = c(
    list("~/googledriveAuthentication.R"), # each user's gargle settings
    SpaDES.project::optionsCache(),
    SpaDES.project::optionsBatch(),
    list(
      SpaDES.project.strict = TRUE, # stop on any error setupProject would otherwise tolerate and skip (Eliot 2026-11-03)
      LandR.assertions = FALSE,
      reproducible.destinationPathShared = file.path(.basePath, "data"),
      reproducible.cloudFolderID = "1oNGYVAV3goXfSzD1dziotKGCdO8P_iV9",
      reproducible.urlRemap = "https://raw.githubusercontent.com/PredictiveEcology/PredictiveEcology.org/main/scripts/arbutus_manifest_SCANFI_v2_clean.csv",
      clusters.deoptimConvergenceWindow = 300L, # DEoptim early stop: judge over 300 generations ...
      clusters.deoptimMinGenerations = 350L,    # ... and never before 350 (Eliot 2026-11-03; was 400/600 since 2026-10-16)
      clusters.maxWorkersPerHost = site$maxWorkersPerHost,
      clusters.workerMemoryGB = if (!is.na(.workerMemoryGB)) .workerMemoryGB, # a fit's own record replaces it
      mc.cores = site$mcCores,
      spades.cacheChaining = .cacheChaining,
      spades.cacheKeyReachable = TRUE, # trial from 2026-10-20 (prodJ): event keys hold only the params/functions the event reaches
      warnPartialMatchArgs = TRUE, # fireSense has objects partial matching would confuse (rstLCC, rstLCCs)
      warnPartialMatchAttr = TRUE,
      warnPartialMatchDollar = TRUE)),
  ## terra's default todisk = FALSE kept every raster result in RAM (25 rasters, 6.1 GB, in 12.1_2's phase 1, 2026-10-28)
  sideEffects = list(
    terra::gdalCache(size = 2048),
    terra::projNetwork(FALSE),
    terra::terraOptions(memmax = 16, todisk = TRUE, memfrac = 0.5)),
  params = Require::modifyList2(
    list(
      .globals = list(
        fitFileSuffix = .spreadFitFileSuffix, # spread + ignition ledgers: <prefix><years><tag><suffix>.rds; "latest" reads only this suffix
        .studyAreaName = .ELFind,
        heldOutFold = .heldOutFold,
        .runName = runName,
        .plotInterval = 100,
        .plots = "png",
        .useCache = c(".inputObjects", "init", "initPlot", "estimateThreshold", "spreadFitPrepare", "checkData")),
      canClimateData = list(climateGCM = .GCM, climateSSP = .SSP),
      fireSense_spreadFit = c(list(
        doFit = .doFit,
        cores = .cores,
        mode = strsplit(.spreadFitMode, ",")[[1]]),
        if (!is.na(.spreadLink)) list(spreadLink = .spreadLink),
        if (!is.na(.spreadWeighted)) list(weighted = .spreadWeighted),
        if (!is.na(.spreadGateRefusal)) list(gateRefusal = .spreadGateRefusal),
        if (!is.na(.spreadReps)) list(objfunFireReps = as.integer(.spreadReps)),
        if (!is.na(.spreadCrpsWeights)) list(crpsWeights = local({kv <- strsplit(strsplit(.spreadCrpsWeights, ",")[[1]], "="); as.list(setNames(as.numeric(vapply(kv, `[`, "", 2L)), vapply(kv, `[`, "", 1L)))})),
        if (!is.na(.spreadTests)) list(DEoptimTests = strsplit(.spreadTests, ",")[[1]]),
        if (!is.na(.spreadInitialPop)) list(initialpop = readRDS(.spreadInitialPop)),
        if (!is.na(.spreadInitialPopMethod)) list(initialPopMethod = .spreadInitialPopMethod),
        if (!is.na(.spreadSdBounds)) list(yearSpreadSDBounds = as.numeric(strsplit(.spreadSdBounds, ",")[[1]])),
        if (!is.na(.spreadSizeLikDf)) list(sizeLikDf = as.numeric(.spreadSizeLikDf)),
        if (!is.na(.spreadPowerLawShape)) list(powerLawShape = as.numeric(.spreadPowerLawShape)),
        if (isTRUE(as.logical(.wholeLandscape))) list(wholeLandscapeSpread = TRUE)),
      fireSense_dataPrepFit = c(list(
        doFit = .doFit, # a refit chooses its spread climate variable afresh (fireSense_dataPrepFit#81)
        dataYears = seq(1985L, 2020L, 5L), # every SCANFI year
        .useCache = c(".inputObjects", "dataPrepBuild", "prepEscapeFitData", "prepSpreadFitData",
                      "prepIgnitionFitData")),
        if (!is.na(.fireCauses)) list(fireCauses = strsplit(.fireCauses, ",")[[1]]),
        if (!is.na(.prepAreaMult)) list(areaMultiplier = as.call(list(as.name("function"), as.pairlist(alist(size = )),
                                                                      bquote(.(as.numeric(.prepAreaMult)) * fireSenseUtils::multiplier(size)), NULL))),
        if (isTRUE(as.logical(.wholeLandscape))) list(wholeLandscapeSpreadCovariates = TRUE)),
      Biomass_core = c(list(),
        if (!is.na(.cohortRemoveSuppressed)) list(cohortRemoveSuppressed = as.numeric(strsplit(.cohortRemoveSuppressed, ",")[[1]])),
        if (!is.na(.cohortSquash)) list(cohortSquash = as.numeric(strsplit(.cohortSquash, ",")[[1]])),
        if (!is.na(.cohortEstablishmentLimit)) list(cohortEstablishmentLimit = as.numeric(strsplit(.cohortEstablishmentLimit, ",")[[1]]))),
      Biomass_speciesFactorial = list(.plots = NULL),
      Biomass_speciesParameters = list(standAgesForFitting = c(0, 100), balanceGrowth = TRUE),
      fireSense_ignitionFit = list(.useCache = TRUE, doFit = .doFit),
      burnSummaries = list(reps = .rep, summaryPeriod = as.integer(c(times$start, times$end))),
      NRV_summary = list(reps = .rep)),
    # NRV samples observed climate, so no projected years (canClimateData still needs a valid GCM)
    if (.NRV) list(
      canClimateData = list(climateGCM = "CNRM-ESM2-1", projectedClimateYears = integer(0)),
      climateYear = list(samplingRange = .samplingRange, yearSeed = as.integer(.rep))), # every ELF of a rep: one climate-year sequence
    if (.hc) .hcSetup$params,
    if (.hc && !is.na(.fireCauses)) list(fireSense_summary = list(fireCauses = strsplit(.fireCauses, ",")[[1]])))
)
message(inSim$runName)
## The NRV page (a queue row with .nrvReport = <tag>, from expt.R's FS_SET=nrvreport): the summary modules in mode = "multi"
## over the ELF's 5 finished NRV runs (NRV_summary with its stand-level "st" event, burnSummaries, fireSense_summary), then
## the tables and figures the page shows are copied to outputs/nrvreport_<tag>/<ELF>/. Work files: scratch/nrvreport_<tag>/<ELF>/.
## After setupProject(), so the job has the same options and sideEffects (terra todisk, PROJ network off) as every run.
if (!is.na(get0(".nrvReport", ifnotfound = NA))) {
  Sys.setenv(OPENBLAS_NUM_THREADS = 1, OMP_NUM_THREADS = 1); data.table::setDTthreads(1)
  .base <- get0(".basePath", ifnotfound = site$basePath); .reps <- 1:5
  .nCores <- as.integer(Sys.getenv("SLURM_CPUS_PER_TASK", "1"))
  options(mc.cores = .nCores, reproducible.cacheSaveFormat = "qs2")
  .runDir <- function(r) file.path(.base, "outputs", .ELFind, "NRV", 370, r) # an NRV run's outputPath (.GCM "NRV", .SSP 370)
  .simFile <- function(r) file.path(.runDir(r), sprintf("%s_NRV_370_%d_%d.rds", .ELFind, r, 3020L))
  .work <- file.path(.base, "scratch", paste0("nrvreport_", .nrvReport), .ELFind)
  .out <- file.path(.base, "outputs", paste0("nrvreport_", .nrvReport), .ELFind)
  .t0 <- Sys.time()
  ## inputs the runs made, from rep 1's saved simList
  .lazy <- file.path(.runDir(1), sprintf("%s_NRV_370_1_3020_lazy", .ELFind))
  .man <- readRDS(file.path(.lazy, "_manifest.rds"))
  ## filebackedPath = NULL: a saved run's rasters are its own files, not Cache entries (with the cachePath option that
  ## setupProject() sets, .unwrap() would look for them in the cache)
  .obj <- function(n) reproducible::.unwrap(readRDS(file.path(.lazy, .man$file[.man$name == n & .man$where == ""][1])),
                                            filebackedPath = NULL)
  ## .nrvReportMap (FS_NRVR_MAP=TRUE): only the map of Canada's ELFs with those reported so far (a DONE.txt in
  ## outputs/nrvreport_<tag>/<ELF>/) coloured by the % of their NRV tests inside the range, by fireSense_summary (mode
  ## "ELFmap"), to outputs/nrvreport_<tag>/ELFmap.png
  if (isTRUE(get0(".nrvReportMap"))) {
    .reported <- basename(dirname(Sys.glob(file.path(dirname(.out), "*", "DONE.txt"))))
    .ELFs <- .obj("ELFs") # the national ELFs, merges applied; their rasters come back packed
    .ELFs$rasWhole <- lapply(.ELFs$rasWhole, terra::unwrap)
    .ELFs$rasCentered <- lapply(.ELFs$rasCentered, terra::unwrap)
    .ELFs$poly <- reproducible::.unwrap(.ELFs$poly)
    SpaDES.core::simInitAndSpades(
      times = list(start = 0, end = 1), modules = "fireSense_summary",
      params = list(fireSense_summary = list(mode = "ELFmap", reportedELFs = .reported, simOutputPath = dirname(.out),
                                             nrvComparisonPath = dirname(.out), .plots = "png")),
      objects = list(ELFs = .ELFs),
      paths = list(modulePath = get0(".nrvReportModules", ifnotfound = "modules"), outputPath = dirname(.out),
                   cachePath = file.path(.work, "cache"), scratchPath = file.path(.work, "scratch")))
    stop("nrvreport map done: ", length(.reported), " ELFs filled", call. = FALSE)
  }
  ## the summary modules find replicates as rep<N> folders (SpaDES.core::dirnamesFromSet()): each run's files are
  ## linked into <work>/rep<N>, and its registered outputs (outputsDF) are pointed at the links
  .odf <- data.table::rbindlist(lapply(.reps, function(r) {
    d <- file.path(.work, paste0("rep", r)); dir.create(d, recursive = TRUE, showWarnings = FALSE)
    for (f in list.files(.runDir(r), full.names = TRUE))
      if (!file.exists(l <- file.path(d, basename(f)))) file.symlink(f, l)
    o <- data.table::as.data.table(SpaDES.core::outputs(readRDS(.simFile(r))))
    o <- o[startsWith(o$file, paste0(.runDir(r), "/")), ]
    o$file <- file.path(d, substring(o$file, nchar(.runDir(r)) + 2L))
    o[dirname(o$file) == d, ]
  }))
  ## the ELF is the one reporting polygon (as multi.R)
  .sar <- .obj("studyAreaReporting")
  .rp <- sf::st_as_sf(.sar); .rp$Name <- .ELFind; .rp <- .rp[, "Name"]; attr(.rp, "field") <- "Name"
  .sim1 <- readRDS(.simFile(1))
  .times <- list(start = as.numeric(SpaDES.core::start(.sim1)), end = as.numeric(SpaDES.core::end(.sim1))) # years
  rm(.sim1)
  .common <- list(mode = "multi", reps = .reps, .studyAreaName = .ELFind, .plots = "png", .useCache = FALSE)
  .simOut <- SpaDES.core::simInitAndSpades(
    times = .times,
    modules = c("NRV_summary", "burnSummaries", "fireSense_summary"),
    params = list(
      NRV_summary = c(.common, list(postprocessEvents = c("lm", "pm", "st"), plotWorkers = min(.nCores, 4L),
                                    summaryPeriod = as.integer(unlist(.times)), summaryInterval = 100L, nrvWindow = 0.3,
                                    ## northern ELFs (ecozones 2, 3, 4, 5, 15): age-class cutoffs at 80% of the module
                                    ## default (Young/Immature/Mature/Old from 0/32/64/96 instead of 0/40/80/120; Eliot 2026-11-03)
                                    ageClassCutOffs = if (sub("\\..*", "", .ELFind) %in% c("2", "3", "4", "5", "15"))
                                      as.integer(round(0.8 * LandWebUtils:::.ageClassCutOffs)) else LandWebUtils:::.ageClassCutOffs)),
      burnSummaries = c(.common, list(historicYears = c(1985L, 2024L))), # observed record: NBAC, 1985-2024
      fireSense_summary = c(.common, list(simOutputPath = .work, climateScenario = "historical",
                                           historicYears = c(1985L, 2024L)))), # observed fires: the same record as burnSummaries
    objects = list(outputsDF = .odf, reportingPolygons = list(ELF = .rp), studyAreaReporting = .sar,
                   sppEquiv = .obj("sppEquiv"), sppColorVect = .obj("sppColorVect"), rasterToMatch = .obj("rasterToMatch")),
    paths = list(modulePath = get0(".nrvReportModules", ifnotfound = "modules"), outputPath = file.path(.work, "mainSim"),
                 inputPath = file.path(.base, "inputs", .ELFind), # the fire records already there (no download)
                 cachePath = file.path(.work, "cache"), scratchPath = file.path(.work, "scratch")))
  ## the page's tables and figures
  .keep <- list.files(.work, recursive = TRUE, pattern = paste0(
    "(lm|st)_nrvComparison\\.csv$|st(_CC)?\\.csv$|stability\\.csv$|burnSummaries_fire(Regime|Blocks)\\.csv$|",
    "current_condition_(overview|all.*)\\.png$|_ribbon\\.png$|stand_(age|leading|biomass)\\.png$|",
    "cumulative_burn_maps\\.png$|fire_per_block\\.png$|cumulBurnMap_.*\\.png$|",
    "burnSummary_.*\\.png$|simulated_(number_size_fires|burnArea_.*|Escapes_.*|Ignitions_.*)\\.png$|stability_.*\\.png$|",
    "^lead_.*\\.png$")) # list.files() matches the pattern against file names only; lead_<species>.png are the st species figures
  .keep <- .keep[!grepl("^rep[0-9]+/|/per-metric/", .keep)]
  ## only what this run wrote: <work> is reused across runs, so a file a module no longer writes would otherwise
  ## reach the page from an earlier run
  .keep <- .keep[file.mtime(file.path(.work, .keep)) >= .t0]
  unlink(.out, recursive = TRUE); dir.create(.out, recursive = TRUE)
  for (.f in .keep) {
    dir.create(dirname(file.path(.out, .f)), recursive = TRUE, showWarnings = FALSE)
    file.copy(file.path(.work, .f), file.path(.out, .f))
  }
  writeLines(c(paste("ELF", .ELFind), paste("reps", paste(.reps, collapse = ",")),
               paste("ageClassCutOffs", paste(.simOut@params$NRV_summary$ageClassCutOffs, collapse = ",")), # the page states them
               paste("minutes", round(as.numeric(difftime(Sys.time(), .t0, units = "mins")), 1)),
               paste("finished", format(Sys.time()))), file.path(.out, "DONE.txt"))
  stop("nrvreport done", call. = FALSE) # ends the job; the queue marks a stop as an ERROR row, the outputs are what count
}

## Runs the job up to the end of its phase ("inputs": stop before the fit; "fit": fit if this ELF has no fit, then stop;
## "all": fit if needed, then predict and save the simList to Drive for multi.R). .fitQueue/.afterQueue: an inputs job
## skips an ELF the fit queue has claimed or will claim next; a fit job waits while the inputs queue is building its ELF.
simOut <- fireSenseUtils::runJob(inSim, elf = inSim$.ELFind, phase = inSim$.phase,
                                 fitQueue = inSim$.fitQueue, afterQueue = inSim$.afterQueue,
                                 diagFile = inSim$.diagFile, rprofPrefix = inSim$.rprofPrefix,
                                 uploadDir = inSim$.uploadGSdir, hindcast = inSim$.hc,
                                 ledger = if (!is.na(inSim$.archiveLedger))
                                   reproducible::CacheGeoLedger(inSim$.archiveLedger, remote = inSim$.uploadGSdir,
                                                                destinationPath = inSim$paths$outputPath, key = "runName"),
                                 scenario = list(ELF = inSim$.ELFind, GCM = inSim$.GCM, SSP = inSim$.SSP, rep = as.integer(inSim$.rep)))
