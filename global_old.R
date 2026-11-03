## Packages are installed by expt.R before any worker starts; workers only read the library.
## Run from the project folder (workers and the IDE do); site.R holds what differs per machine.
site <- source("site.R", local = TRUE)$value

## Phase 4 (a queue row with .score = TRUE, from expt.R's FS_SET=score): score the hindcasts of every ELF that has finished
## years, pooled, with fireSimulator_validation (modules/). The ledger suffix and the base path come from the row.
if (isTRUE(get0(".score"))) {
  .base <- get0(".basePath", ifnotfound = site$basePath); .suffix <- get0(".spreadFitFileSuffix", ifnotfound = "")
  for (.f in list.files("modules/fireSimulator_validation/R", "\\.R$", full.names = TRUE)) source(.f)
  .hcDir <- file.path(.base, "outputs", paste0("hindcast", .suffix))
  .elfs <- lapply(list.files(.hcDir), function(e)
    readFireSenseHindcast(e, .hcDir, years = seq(1986L, 2021L, 5L), sets = 1L, inputsDir = file.path(.base, "inputs"),
                          ledgerFile = Sys.glob(file.path(.base, "inputs", e, paste0("fireSenseParams_*", .suffix, ".rds")))[1],
                          nfdbPath = file.path(.base, "data/NFDB_point_shp/NFDB_point_20250519.shp"),
                          nbacPath = file.path(.base, "data/NBAC_1972to2025_20260513_shp/NBAC_merged_1972_to_2025_20260513.shp"),
                          obsCacheDir = {d <- file.path(.base, "outputs", paste0("score", .suffix), "obs"); dir.create(d, recursive = TRUE, showWarnings = FALSE); d}))
  .elfs <- Filter(Negate(is.null), .elfs)
  message("scoring ", length(.elfs), " ELFs")
  .res <- runWave1(.elfs)
  saveRDS(.res, file.path(.base, "outputs", paste0("score", .suffix), "wave1.rds"))
  write.csv(.res$results, file.path(.base, "outputs", paste0("score", .suffix), "wave1_results.csv"), row.names = FALSE)
  stop("scoring done", call. = FALSE) # ends the job; the queue marks a stop as an ERROR row, the outputs are what count
}

inSim <- SpaDES.project::setupProject(
  defaultDots = list(.basePath = site$basePath, # inputs, outputs, cache, scratch and shared data live here
                     .rep = 1,
                     .ELFind = "4.3",
                     .SSP = 370,
                     .GCM = "CNRM-ESM2-1", # or "NRV"
                     .samplingRange = 1985:2024, # NRV climate years
                     # fitting window: vegetation is strictly before the fire year, so the first SCANFI V2 year
                     # (1985) feeds 1986; the end is the last year of historical climate.
                     # expt.R checks this line against climateData::latestHistoricalYear().
                     .fireYears = c(1986L, 2024L),
                     .doFit = NA,                     # NA: fit only if no ledger fit; TRUE: always refit; FALSE: never fit
                     .spreadFitMode = "fit",          # or "fit,validate"
                     .heldOutFold = NA_integer_,      # 1 or 2 fits only that fold
                     .spreadFitFileSuffix = "_bufIndep20261024", # buffer-independent spread objective, 2026-10-24; "_prod20261021" was the previous ledger
                     .params = list(),                # per-run module parameters (from expt.R): list(<module> = list(<param> = value)), merged over the defaults below.
                     #   `.globals` reaches every module that declares the parameter: list(.globals = list(fireCauses = "natural")).
                     #   fireSense_spreadFit: spreadLink ("ceiling", "climateOutside"; default "powerLaw"), weighted ("log", "sqrt"),
                     #   DEoptimTests, initialPopMethod ("emulator"), initialpop (an .rds path to an NP x names(lower) matrix; read below)
                     .hindcastYear = NA_integer_,     # a SCANFI year makes this job a hindcast
                     .hcAllSets = FALSE,              # TRUE: one hindcast job runs every fitted spread parameter set, 20 reps (5 sets x 4; fireSense_spreadPredict paramSetByReplicate)
                     .hcIgnitions = NA_character_,    # NA: the module default ("predicted"); "observedEscapes": a hindcast spreads each year's observed escapes (spread alone)
                     .phase1Only = FALSE,             # TRUE: build every input and cache, then stop before the spread threshold calibration (a 1-CPU Slurm job)
                     .phase2Only = FALSE,             # TRUE: fit if this ELF has no fit, then stop; neither phase: fit if needed, then predict
                     .workerMemoryGB = NA_real_,      # NA: the fireSenseUtils default per worker; the ELF's mean worker GB, so a run name with no fit record is budgeted as its Slurm request was
                     .cacheChaining = TRUE,           # FALSE: spades.cacheChaining off
                     .fitQueue = NA_character_,       # phase 1: a fit queue .rds; skip an ELF the fits have claimed or the idle fit workers will claim next
                     .afterQueue = NA_character_,     # phase 2: a phase-1 queue .rds; wait while phase 1 is building this ELF (RUNNING)
                     .diagFile = NA_character_,       # diagnostic: an R file to source, e.g. trace() calls
                     .rprofPrefix = NA_character_,    # diagnostic: profile the run to <prefix>_<ELF>_<pid>.out
                     .archiveLedger = NA_character_,  # production waves: the CacheGeo ledger file (in the upload folder) that records each uploaded run for simListArchive; NA for experiments
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
  .knobs = { # the one initialpop that is a path in the queue
    p <- .params
    if (is.character(p$fireSense_spreadFit$initialpop)) p$fireSense_spreadFit$initialpop <- readRDS(p$fireSense_spreadFit$initialpop)
    p
  },
  .doFit = if (.hc) FALSE else .doFit, # a hindcast only predicts from a fit in the ledger; it stops if there is none
  .NRV = identical(.GCM, "NRV"),
  .phase = if (.phase1Only) "inputs" else if (.phase2Only) "fit" else "all",
  .ELFind = .ELFind,
  .rep = .rep, # without it, a .rep from the queue does not reach params$.globals
  # only dots written here are returned in inSim: a defaultDots value the queue did not set is not a variable after setupProject
  .fitQueue = .fitQueue, .afterQueue = .afterQueue, .diagFile = .diagFile, .rprofPrefix = .rprofPrefix,
  .archiveLedger = .archiveLedger, .GCM = .GCM, .SSP = .SSP, # the ledger's scenario columns
  .uploadGSdir = "https://drive.google.com/drive/folders/188ERmd1k6s6YMv3wHtnHQHD7pgLseBjf?usp=drive_link",
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
  packages = c("PredictiveEcology/SpaDES.core@development (>= 3.2.1.9055)",
               "PredictiveEcology/reproducible@development (>= 3.2.1.9084)",
               "PredictiveEcology/SpaDES.project@development (>= 1.2.0.9020)",
               "PredictiveEcology/clusters@development (>= 0.0.88)",
               "PredictiveEcology/fireSenseUtils@development (>= 0.2.3.9118)",
               "qs2"), # reproducible.cacheSaveFormat
  require = c("reproducible", "terra"),
  options = c(
    list("~/googledriveAuthentication.R"), # each user's gargle settings
    SpaDES.project::optionsCache(),
    SpaDES.project::optionsBatch(),
    list(
      LandR.assertions = FALSE,
      reproducible.destinationPathShared = file.path(.basePath, "data"),
      reproducible.cloudFolderID = "1oNGYVAV3goXfSzD1dziotKGCdO8P_iV9",
      reproducible.urlRemap = "https://raw.githubusercontent.com/PredictiveEcology/PredictiveEcology.org/main/scripts/arbutus_manifest_SCANFI_v2_clean.csv",
      clusters.maxWorkersPerHost = site$maxWorkersPerHost,
      clusters.workerMemoryGB = if (!is.na(.workerMemoryGB)) .workerMemoryGB, # a fit's own record replaces it
      mc.cores = site$mcCores,
      spades.cacheChaining = .cacheChaining,
      spades.cacheKeyReachable = TRUE, # trial from 2026-10-20 (prodJ): event keys hold only the params/functions the event reaches
      warnPartialMatchArgs = TRUE, # fireSense has objects partial matching would confuse (rstLCC, rstLCCs)
      warnPartialMatchAttr = TRUE,
      warnPartialMatchDollar = TRUE)),
  sideEffects = list(
    terra::gdalCache(size = 2048),
    terra::projNetwork(FALSE),
    terra::terraOptions(memmax = 16, todisk = TRUE, memfrac = 0.5)), # todisk: terra's default kept every raster result in RAM
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
      fireSense_spreadFit = list(doFit = .doFit, cores = .cores, mode = strsplit(.spreadFitMode, ",")[[1]], phase = .phase),
      fireSense_dataPrepFit = list(doFit = .doFit), # a refit chooses its spread climate variable afresh
      Biomass_speciesFactorial = list(.plots = NULL),
      Biomass_speciesParameters = list(standAgesForFitting = c(0, 100), balanceGrowth = TRUE),
      fireSense_ignitionFit = list(doFit = .doFit),
      burnSummaries = list(reps = .rep, summaryPeriod = as.integer(c(times$start, times$end))),
      NRV_summary = list(reps = .rep)),
    .knobs,
    # NRV samples observed climate, so no projected years (canClimateData still needs a valid GCM)
    if (.NRV) list(
      canClimateData = list(climateGCM = "CNRM-ESM2-1", projectedClimateYears = integer(0)),
      climateYear = list(samplingRange = .samplingRange)),
    if (.hc) .hcSetup$params)
)
message(inSim$runName)

fireSenseUtils::runJob(inSim, elf = inSim$.ELFind, fitQueue = inSim$.fitQueue, afterQueue = inSim$.afterQueue,
                       diagFile = inSim$.diagFile, rprofPrefix = inSim$.rprofPrefix,
                       uploadDir = inSim$.uploadGSdir, hindcast = inSim$.hc,
                       ledger = if (!is.na(inSim$.archiveLedger))
                         reproducible::CacheGeoLedger(inSim$.archiveLedger, remote = inSim$.uploadGSdir,
                                                      destinationPath = inSim$paths$outputPath, key = "runName"),
                       scenario = list(ELF = inSim$.ELFind, GCM = inSim$.GCM, SSP = inSim$.SSP, rep = inSim$.rep))
