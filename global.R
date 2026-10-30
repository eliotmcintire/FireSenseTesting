## Packages are installed by expt.R before any worker starts; workers only read the library.
## Running this file alone needs only SpaDES.project and Require.

projectDir <- switch(Sys.info()[["user"]],
                     ieddy = "~/git/FireSenseTesting",
                     elm001 = "/gpfs/fs7/nrcan/nrcan_forsite/elm001/firesense/FireSenseTesting", # GPSC
                     "~/GitHub/FireSenseTesting")
setwd(projectDir)

# tmux panes report interactive() == TRUE; batch workers run inside tmux
userInteractive <- interactive() && !nzchar(Sys.getenv("TMUX"))

inSim <- SpaDES.project::setupProject(
  defaultDots = list(.basePath = "/mnt/fast", # inputs, outputs, cache, scratch and shared data live here
                     .rep = 1,
                     .ELFind = "4.3",
                     .SSP = 370,
                     .GCM = "CNRM-ESM2-1", # or "NRV"
                     .samplingRange = 1985:2024, # NRV climate years
                     # fitting window: vegetation is strictly before the fire year, so the first SCANFI V2 year
                     # (1985) feeds 1986; the end is the last year of historical climate.
                     # expt_draft.R checks this line against climateData::latestHistoricalYear().
                     .fireYears = c(1986L, 2024L),
                     .doFit = NA,                     # NA: fit only if no ledger fit; TRUE: always refit; FALSE: never fit
                     .spreadFitMode = "fit",          # or "fit,validate"
                     .heldOutFold = NA_integer_,      # 1 or 2 fits only that fold
                     .spreadFitFileSuffix = "_bufIndep20261024", # buffer-independent spread objective (fireSenseUtils#183, fireSense_spreadFit#91), 2026-10-24; "_prod20261021" was the previous ledger
                     .spreadLink = NA_character_,     # NA: the module default ("powerLaw"); or "ceiling", "climateOutside"
                     .spreadWeighted = NA_character_, # NA: the module default (FALSE, every fire weighs 1); or "log", "sqrt" (fireSenseUtils sizeWeight())
                     .spreadTests = NA_character_,    # NA: the module default DEoptimTests; or e.g. "SNLL_FS,pitAD" (comma-separated)
                     .spreadInitialPop = NA_character_, # NA: the module default (DEoptim draws, or calibration passing sets); or an .rds matrix, NP x names(lower)
                     .spreadInitialPopMethod = NA_character_, # NA: the module default ("calibration"); or "emulator" (fireSense_spreadFit#98)
                     .fireCauses = NA_character_,     # NA: the module defaults; e.g. "natural,unknown": fireSenseUtils::fireCauseCodes() groups for the fits (dataPrepFit) and the hindcast's observed fires (fireSense_summary)
                     .hindcastYear = NA_integer_,     # a SCANFI year makes this job a hindcast
                     .hcAllSets = FALSE,              # TRUE: one hindcast job runs every fitted spread parameter set, 20 reps (5 sets x 4; fireSense_spreadPredict paramSetByReplicate)
                     .hcIgnitions = NA_character_,    # NA: the module default ("predicted"); "observedEscapes": a hindcast spreads each year's observed escapes (spread alone)
                     .phase1Only = FALSE,             # TRUE: build every input and cache, then stop before the spread threshold calibration (a 1-CPU Slurm job)
                     .phase2Only = FALSE,             # TRUE: fit if this ELF has no fit, then stop; neither phase: fit if needed, then predict
                     .workerMemoryGB = NA_real_,      # NA: 10 under Slurm, else clusters' own default; the ELF's mean worker GB, so a run name with no fit record is budgeted as its Slurm request was
                     .cacheChaining = TRUE,           # FALSE: spades.cacheChaining off
                     .fitQueue = NA_character_,       # phase 1: a fit queue .rds; skip an ELF the fits have claimed or the idle fit workers will claim next
                     .afterQueue = NA_character_,     # phase 2: a phase-1 queue .rds; wait while phase 1 is building this ELF (RUNNING)
                     .diagFile = NA_character_,       # diagnostic: an R file to source, e.g. trace() calls
                     .rprofPrefix = NA_character_,    # diagnostic: profile the run to <prefix>_<ELF>_<pid>.out
                     .cores = c("birds", "camas", "caribou", "coco", "core", "dougfir",
                                "fire", "mpb", "sbw", "acer", "abies", "pinus", "landr", "kodama", "mega"), # mega added 2026-10-20 as phase 1 empties; biomass stays out; carbon (lent 2026-10-23) returned 2026-10-25
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
  # A hindcast holds year Y's vegetation static and draws Y's climate, 10 reps as decimal years
  # Y to Y.009. It runs no vegetation or summary modules.
  .hc = !is.na(.hindcastYear),
  .doFit = if (.hc) FALSE else .doFit, # a hindcast only predicts from a fit in the ledger; it stops if there is none
  .hcStep = 0.001, # years between hindcast reps
  .NRV = identical(.GCM, "NRV"),
  .ELFind = .ELFind,
  .rep = .rep, # without it, a .rep from the queue does not reach params$.globals
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
  # a hindcast's 10 reps: end at 9.5 steps, as nine additions of .hcStep land just below 9 steps
  times = if (.hc) list(start = .hindcastYear, end = .hindcastYear + (if (isTRUE(.hcAllSets)) 19.5 else 9.5) * .hcStep) else .times,
  # a hindcast cannot use the parent: it would bring the vegetation modules
  modules = if (.hc) c("PredictiveEcology/canClimateData@modsForFireSense",
                       "PredictiveEcology/climateYear@modsForFireSense",
                       "PredictiveEcology/fireSense@development")
            else .modules,
  # only what this file itself calls or configures; modules list their own in reqdPkgs
  packages = c("PredictiveEcology/SpaDES.core@development (>= 3.2.1.9055)",      # a chain hit counts as newly computed only when Cache says so (#513); a chain jump restores only what an untagged entry changed (#512); event hits keep duplicate events (#505); spades.cacheKeyReachable (#500), cacheKey = FALSE (#503); module version out of event cache keys (#501); module globalVariables() (#496) without the self-referencing simList (#498), cache hit restores own outputs only (#495), chain jump restores changed objects (#497)
               "PredictiveEcology/reproducible@development (>= 3.2.1.9084)",     # a short download is never kept, downloads staged in destinationPath (#668); clearCache(cacheId) lists only those ids (#666); offline-safe prepInputs (#657-#659) and CacheGeo with no Google credentials (#664); Copy(), the reproducible.* options and their defaults
               "PredictiveEcology/SpaDES.project@development (>= 1.2.0.9020)",   # workers start from a launcher file (#202); params merge, .rep from the caller, per-worker terra/GDAL defaults
               "PredictiveEcology/clusters@development (>= 0.0.88)",          # clusters.maxWorkersPerHost (#58), across all fits on a host (#59); Slurm nodes sharing a home do not count each other's bookings (#63)
               "PredictiveEcology/fireSenseUtils@development (>= 0.2.3.9118)",   # makeFireSenseLCC() CWIM destinationPath (#178); ELFsNotFitted(): Prairie grassland 10.2 left out like the Arctic (#177); yearEffectMultiplier()/yearEffectSD() (#171), fuelCovsToCoarse() (#172) with fuel in the stackAndExtract cache key (#173); ledger suffix, xgbObjective(), hindcastObserved()$points, powerLaw link + calibratePowerLaw() (#167, #168), ELFmetricsFilename() (#170)
               "qs2"),                                                           # reproducible.cacheSaveFormat
  require = c("reproducible", "terra"),
  options = list(
    "~/googledriveAuthentication.R", # each user's gargle settings
    LandR.assertions = FALSE,
    reproducible.cacheSaveFormat = "qs2",
    reproducible.shapefileRead = "terra::vect",
    reproducible.overwrite = TRUE,
    reproducible.destinationPathShared = file.path(.basePath, "data"),
    reproducible.cloudFolderID = "1oNGYVAV3goXfSzD1dziotKGCdO8P_iV9",
    reproducible.urlRemap = "https://raw.githubusercontent.com/PredictiveEcology/PredictiveEcology.org/main/scripts/arbutus_manifest_SCANFI_v2_clean.csv",
    reproducible.useCOG = FALSE,
    # showSimilar scans the whole cache on every miss: minutes per miss in batch workers
    reproducible.showSimilar = userInteractive,
    reproducible.showSimilarDepth = 8,
    reproducible.objSize = FALSE,
    reproducible.memoisePersist = TRUE,
    reproducible.useMemoise = userInteractive,
    clusters.deoptimConvergenceWindow = 400L, # DEoptim early stop: judge over 400 generations ...
    clusters.deoptimMinGenerations = 600L,    # ... and never before 600 (350 always stopped too early)
    clusters.maxWorkersPerHost = c(mega = 55), # mega is also the master; 55 (2026-10-21: stays at 55 until hindcast validations need room)
    # a GPSC node (Slurm) holds 40 workers only at 10 GB each (the fireSenseUtils default, 14, gives 29); p90 is ~6.5 GB
    ## .workerMemoryGB: a fit's own record replaces it
    clusters.workerMemoryGB = if (!is.na(.workerMemoryGB)) .workerMemoryGB else
      if (nzchar(Sys.getenv("SLURM_JOB_ID"))) 10,
    mc.cores = 8, # per-process cap on climate mosaics, fire buffers and threshold forks; otherwise each takes the whole machine
    spades.recoveryMode = userInteractive + 0,
    spades.cacheChaining = .cacheChaining,
    spades.cacheKeyReachable = TRUE, # trial from 2026-10-20 (prodJ): event keys hold only the params/functions the event reaches
    spades.allowInitDuringSimInit = TRUE,
    warnPartialMatchArgs = TRUE, # fireSense has objects partial matching would confuse (rstLCC, rstLCCs)
    warnPartialMatchAttr = TRUE,
    warnPartialMatchDollar = TRUE),
  ## as global.R (see its comments): terra's default todisk = FALSE kept every raster result in RAM
  ## (25 rasters, 6.1 GB, in 12.1_2's phase 1, 2026-10-28)
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
        if (!is.na(.spreadTests)) list(DEoptimTests = strsplit(.spreadTests, ",")[[1]]),
        if (!is.na(.spreadInitialPop)) list(initialpop = readRDS(.spreadInitialPop)),
        if (!is.na(.spreadInitialPopMethod)) list(initialPopMethod = .spreadInitialPopMethod)),
      fireSense_dataPrepFit = c(list(
        doFit = .doFit, # a refit chooses its spread climate variable afresh (fireSense_dataPrepFit#81)
        dataYears = seq(1985L, 2020L, 5L), # every SCANFI year
        .useCache = c(".inputObjects", "dataPrepBuild", "prepEscapeFitData", "prepSpreadFitData",
                      "prepIgnitionFitData")),
        if (!is.na(.fireCauses)) list(fireCauses = strsplit(.fireCauses, ",")[[1]])),
      Biomass_speciesFactorial = list(.plots = NULL),
      Biomass_speciesParameters = list(standAgesForFitting = c(0, 100), balanceGrowth = TRUE),
      fireSense_ignitionFit = list(.useCache = TRUE, doFit = .doFit),
      burnSummaries = list(reps = .rep, summaryPeriod = as.integer(c(times$start, times$end))),
      NRV_summary = list(reps = .rep)),
    # NRV samples observed climate, so no projected years (canClimateData still needs a valid GCM)
    if (.NRV) list(
      canClimateData = list(climateGCM = "CNRM-ESM2-1", projectedClimateYears = integer(0)),
      climateYear = list(samplingRange = .samplingRange)),
    ## a hindcast plots nothing in the modules that run every 0.001-year rep; the data modules keep the fit's
    ## `.plots`, which is in their cache keys, so a hindcast reuses the fit's data prep (2026-10-17)
    if (.hc) list(
      climateYear = list(samplingRange = .hindcastYear),
      fireSense_dataPrepPredict = list(fireTimeStep = .hcStep, .plots = NA),
      fireSense_ignitionPredict = list(.runInterval = .hcStep, .plots = NA),
      fireSense_spreadPredict = c(list(.runInterval = .hcStep, .plots = NA),
                                  if (isTRUE(.hcAllSets)) list(paramSetByReplicate = TRUE)), # fireSense_spreadPredict >= 1.2.0.9004
      fireSense_burn = c(list(.runInterval = .hcStep, .plots = NA),
                         if (!is.na(.hcIgnitions)) list(ignitionSource = .hcIgnitions)),
      fireSense_summary = c(list(hindcastYear = .hindcastYear, hindcastInterval = .hcStep, .plots = NA),
                            if (!is.na(.fireCauses)) list(fireCauses = strsplit(.fireCauses, ",")[[1]]))))
)
message(inSim$runName)
inSimCopy <- reproducible::Copy(inSim)

## Phase 1 (.phase1Only): build every input and cache, stop before the threshold calibration, which forks and
##   so runs in phase 2 on the fit's cores (a 1-CPU phase-1 Slurm job then needs no more).
## Phase 2 (.phase2Only): fit if this ELF has no fit, then stop.
## Neither: fit if this ELF has no fit, then predict.
stopifnot(!(.phase1Only && .phase2Only))
if (.phase1Only) {
  inSimCopy$events <- list(.stopBefore = list(fireSense_spreadFit = "estimateThreshold",
                                              fireSense_dataPrepPredict = "getClimateRasters"))
} else if (.phase2Only) {
  .validates <- "validate" %in% inSimCopy$params$fireSense_spreadFit$mode ||
    !is.na(inSimCopy$params$.globals$heldOutFold)
  inSimCopy$events <- list(.stopAfter = list(fireSense_spreadFit = if (.validates) "crossValidate" else "run"),
                           .stopBefore = list(fireSense_dataPrepPredict = "getClimateRasters"))
}

## The fit queue and a phase-1 queue run side by side; each reads the other's queue file (its workers keep it current).
## .afterQueue = <phase-1 queue .rds>: a fit job waits only while phase 1 is building its ELF right now (RUNNING),
## then reuses that work; it never waits for a phase-1 job that has not started.
## .fitQueue = <fit queue .rds>: a phase-1 job skips an ELF the fits have claimed (RUNNING or DONE), and an ELF
## the idle fit workers will claim next: the fit queue takes its topmost pending rows, so with k fit workers not
## running a job (the fit workers start 2 min apart), its first k pending rows are left to the fits.
## The queue's Google Sheet is its record; the local .rds can lag (a lost lock skips the claim write, a resumed launch
## rewrites it), so it is only the fallback when the sheet cannot be read.
## the queue's rows in claim order, as columns ELF and status (NULL when neither the sheet nor the file can be read)
.queueRows <- function(queueFile) {
  q <- tryCatch({
    nm <- sub("\\.rds$", "", basename(queueFile))
    id <- googledrive::drive_find(nm, type = "spreadsheet", n_max = 1)$id
    q <- as.data.frame(googlesheets4::read_sheet(id, sheet = "Status", col_types = "c"))
    data.frame(ELF = as.character(q$dotELFind), status = as.character(q$status))
  }, error = function(e) NULL)
  if (is.null(q)) q <- tryCatch({ .q <- readRDS(queueFile)
    data.frame(ELF = as.character(.q$.ELFind), status = as.character(.q$status)) }, error = function(e) NULL)
  q
}
.queueStatus <- function(queueFile, elf) {
  q <- .queueRows(queueFile)
  if (is.null(q)) NA_character_ else q$status[q$ELF == elf][1]
}
.skipJob <- FALSE
if (.phase1Only && !is.na(.fitQ <- .fitQueue) && !is.null(.fq <- .queueRows(.fitQ))) {
  ## one worker_launch_<n>.sh per fit worker, in the fit queue's log folder
  .nFitWorkers <- length(dir(file.path(dirname(.fitQ), "logs", basename(.fitQ)), "^worker_launch_[0-9]+\\.sh$"))
  .idleFit <- max(0L, .nFitWorkers - sum(.fq$status == "RUNNING"))
  .nextFit <- head(.fq$ELF[.fq$status %in% c("PENDING", "INTERRUPTED")], .idleFit)
  .fitSt <- .fq$status[.fq$ELF == .ELFind][1]
  .skipJob <- isTRUE(.fitSt %in% c("RUNNING", "DONE")) || .ELFind %in% .nextFit
  if (.skipJob) message("phase 1 of ", .ELFind, " skipped: ",
                        if (.ELFind %in% .nextFit) paste0("one of the next ", .idleFit, " ELFs the idle fit workers will claim")
                        else "the fit queue has claimed it")
}
if (.phase2Only && !is.na(.afterQ <- .afterQueue)) {
  repeat {
    .st <- .queueStatus(.afterQ, .ELFind)
    if (!identical(.st, "RUNNING")) break
    message(format(Sys.time(), "%H:%M"), " waiting for phase 1 of ", .ELFind, " (", .st, ") in ", basename(.afterQ))
    Sys.sleep(300)
  }
}
if (!is.na(.diagFile)) source(.diagFile)
if (!is.na(.rprofPrefix))
  Rprof(paste0(.rprofPrefix, "_", .ELFind, "_", Sys.getpid(), ".out"), interval = 0.5, line.profiling = TRUE)
if (!.skipJob) simOut <- SpaDES.core::simInitAndSpades2(inSimCopy)

## Runs that predict save their simList to Drive for multi.R; an upload failure only warns
if (!.phase1Only && !.phase2Only && !inSim$.hc)
  tryCatch(SpaDES.project::outSaveTarUpload(runName = inSim$runName, sim = simOut, gFolder = inSim$.uploadGSdir),
           error = function(e) warning("outSaveTarUpload failed: ", conditionMessage(e), call. = FALSE))
