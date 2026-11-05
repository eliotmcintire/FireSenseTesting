## Launches a FireSense campaign: installs packages once, builds the job queue, starts the tmux workers.
## Each worker sources global.R per job. Run inside tmux:
##   FS_SET=nrv FS_NRV_WAVE=<fit|predict|ignition> [FS_NRV_ELFS=4.1,14.4|all] [FS_NRV_LEDGER=_burnEmu_20261027] Rscript expt.R
##     predict also needs FS_ARCHIVE_DIR=<this campaign's own Drive folder URL> FS_ARCHIVE_LEDGER=nrvRuns_<campaign>.rds
##   FS_SET=fit [FS_NRV_ELFS=...|all] [FS_AFTER_QUEUE=<phase-1 queue>] Rscript expt.R   (fit every ELF: phases 1 + 2)
##   FS_SET=phase1 [FS_NRV_ELFS=...|all] [FS_FIT_QUEUE=<fit queue>] Rscript expt.R   (inputs and caches only, ahead of the fits:
##     same order as the fits; skips any ELF the fit queue has claimed)
##   FS_SET=hindcast FS_HC_ELFS=4.1,14.4 [FS_HC_LEDGER=_boundfix_rep81] [FS_HC_IGNITIONS=observedEscapes] [FS_HC_ALLSETS=TRUE] Rscript expt.R
##   FS_SET=refit FS_RF_ELFS=6.6.1,9.1.2 Rscript expt.R   (same rep and ledger as production, so caches are shared)
##   FS_SET=refit FS_PHASE1_ONLY=TRUE FS_RF_ELFS=... Rscript expt.R   (caches only: here, ahead of the same refit on the GPSC)
##   FS_SET=nrvreport FS_NRVR_ELFS=6.4,9.3 [FS_NRVR_TAG=phase5] [FS_NRVR_MODULES=modules] Rscript expt.R   (the NRV page's
##     tables and figures, one job per ELF whose 5 NRV runs are DONE)
##   FS_SET=nrvreport FS_NRVR_MAP=TRUE [FS_NRVR_TAG=phase5] Rscript expt.R   (the page's map of the ELFs reported so far)
##   FS_TRIAL=<name> (see "Spread-fit trials" below) with FS_SET=refit, hindcast or score: one trial's fit, its hindcast and the
##     scoring of that hindcast, all with the trial's settings and ledger. On the GPSC, one fit per ELF (its own memory):
##     FS_SET=refit FS_TRIAL=d05 FS_RF_ELFS=6.6.2 FS_SBATCH_MEM=176G Rscript expt.R
##     FS_SET=hindcast FS_TRIAL=d05 FS_HC_ELFS=6.6.2,14.3,11.3,11.4 Rscript expt.R   (each ELF after its own fit, if still running)
##     FS_SET=score FS_TRIAL=d05 Rscript expt.R   (after every hindcast job of the trial)

## inputs, outputs, cache, scratch and shared data: overrides global.R's .basePath here (preRunSetupProject)
## and, as a queue column, in the workers. site.R (in the project folder) holds what differs per machine.
.basePath <- source("site.R")$value$basePath
## no CRAN mirror set (GPSC): install.packages() stops when not interactive. Same repos as the local R; the GPSC is
## Ubuntu 24.04 (noble) too, so the P3M binaries fit its system GDAL
if ("@CRAN@" %in% getOption("repos"))
  options(repos = c(P3M = "https://packagemanager.posit.co/cran/__linux__/noble/latest", CRAN = "https://cloud.r-project.org"))

suppressWarnings(rm(.ELFind)) # a leftover from an earlier run in this session would override global.R's default

####################
# Install packages once, before any worker exists; workers only read the library.
# FS_NO_INSTALL=TRUE skips this, for a launch beside running jobs.
# @modsForFireSense branches = development + our open PRs (R/updateModsForFireSense.sh).
####################
## The project library (.Rprofile) is the frozen "stable" library: nothing installs into it again. An install builds a new
## dev library beside it, <stable>_dev/<id>, with hard links to the current one plus whatever changed (SpaDES.project
## newDevLibrary()), so a running job's library never changes. Each queue row carries its library path (.packagePath).
.stableLib <- .libPaths()[1]
.devCurrent <- file.path(paste0(.stableLib, "_dev"), "current") # what SpaDES.project::currentDevLibrary() reads; read here
if (file.exists(.devCurrent)) .libPaths(c(readLines(.devCurrent, n = 1L), .libPaths())) # because stable's SpaDES.project predates it
if (!isTRUE(as.logical(Sys.getenv("FS_NO_INSTALL", "FALSE")))) {
  if (utils::packageVersion("SpaDES.project") < "1.2.0.9030") { # first install only: newDevLibrary() from a throwaway library
    .boot <- tempfile("spbootstrap"); dir.create(.boot)
    pak::pkg_install("PredictiveEcology/SpaDES.project@development", lib = .boot, dependencies = FALSE, ask = FALSE)
    loadNamespace("SpaDES.project", lib.loc = .boot)
  }
  ## (HEAD): Require compares with the branch's latest commit; a bare @branch installs nothing when the package is present
  SpaDES.project::newDevLibrary(c("PredictiveEcology/Require@development (HEAD)",
                                  "PredictiveEcology/SpaDES.project@development (HEAD)",
                                  "PredictiveEcology/reproducible@development (HEAD)",
                                  "PredictiveEcology/SpaDES.core@development (HEAD)",
                                  "PredictiveEcology/SpaDES.tools@development (HEAD)",
                                  "PredictiveEcology/LandR@modsForFireSense (HEAD)",
                                  "PredictiveEcology/clusters@development (HEAD)",
                                  "PredictiveEcology/filelock@main (HEAD)",
                                  "PredictiveEcology/climateData@modsForFireSense (HEAD)",
                                  "PredictiveEcology/quickPlot@development (HEAD)",
                                  "googlesheets4"), stable = .stableLib) # googlesheets4: the sheet-backed queue (a Suggests of SpaDES.project)
  # no dependencies: its Remotes say LandR@development, which conflicts with LandR@modsForFireSense
  SpaDES.project::newDevLibrary("PredictiveEcology/fireSenseUtils@development", stable = .stableLib,
                                installFun = function(packages, libPath)
                                  pak::pkg_install(packages, lib = libPath, dependencies = FALSE, ask = FALSE))
}
## "<dev>;<stable>" (stable alone before the first install). Set here, it overrides global.R's default in
## preRunSetupProject() below, as .basePath does, and it is each queue row's library (column .packagePath)
.packagePath <- paste(SpaDES.project::currentDevLibrary(stable = .stableLib), collapse = ";")
.libPaths(unique(c(strsplit(.packagePath, ";", fixed = TRUE)[[1]], .libPaths())))

options(spades.useRequire = FALSE) # nothing installs from here on: pak above did it, so setupProject() must not run Require
outs <- SpaDES.project::preRunSetupProject(file = "global.R", upTo = "params")

####################
# Fitting years: the end is the last year of historical climate. Workers take the window from
# global.R's defaultDots, so stop if that has drifted.
####################
.fireYearStart <- 1986L # vegetation strictly before the fire year: SCANFI 1985 feeds 1986
.fireYearEnd <- as.integer(climateData::latestHistoricalYear())
.globalYears <- as.integer(regmatches(.l <- grep("^\\s*\\.fireYears = c\\(", readLines("global.R"), value = TRUE)[1],
                                      gregexpr("[0-9]{4}", .l))[[1]])
if (!identical(.globalYears, c(.fireYearStart, .fireYearEnd)))
  stop("global.R's .fireYears do not match ", .fireYearStart, ":", .fireYearEnd, "; edit global.R")

####################
# Campaign settings. A queue name is resumed, never extended: relaunching changed settings needs a new tag.
####################
.split <- function(x) strsplit(x, ",")[[1]]

## Spread-fit trials. FS_TRIAL=<name> sets that trial's FS_* values (a value already in the environment wins), so its fit,
## hindcast and score use one ledger and one set of settings. Fit queues are refit_<name>_<ELF>, hindcast queues
## hindcast_<name>_<ELF>, the score queue score_<name>.
.trialBase <- c(FS_FIRE_CAUSES = "natural,unknown", FS_SPREAD_GATE_REFUSAL = "simulate", FS_SPREAD_REPS = "100",
                FS_SPREAD_INITPOP_METHOD = "emulator", FS_SPREAD_TESTS = "crps",
                FS_SPREAD_CRPS_WEIGHTS = "fire=1,adTest=0.34,areaDist=2.6,totalArea=164,top10=16.5,annualArea=520",
                FS_HC_ALLSETS = "TRUE", FS_HC_WORKERS = "1", FS_HC_AFTER_FITS = "TRUE")
.trials <- list(
  ## 2026-11-04, GPSC, 6.6.2 14.3 11.3 11.4: δ (power-law shape) 0.3-0.5; top-10 weight x3 (t3); the tail-share term (ts);
  ## the year-effect SD capped at 1 (sd1)
  d03    = c(ledger = "_crpsCauseAnnD03_20261104",      FS_SPREAD_POWERLAW_SHAPE = "0.3"),
  d04    = c(ledger = "_crpsCauseAnnD04_20261104",      FS_SPREAD_POWERLAW_SHAPE = "0.4"),
  d05    = c(ledger = "_crpsCauseAnnD05_20261104",      FS_SPREAD_POWERLAW_SHAPE = "0.5"),
  d05sd1 = c(ledger = "_crpsCauseAnnD05Sd1_20261104",   FS_SPREAD_POWERLAW_SHAPE = "0.5", FS_SPREAD_SD_BOUNDS = "0,1"),
  t3d03  = c(ledger = "_crpsCauseAnnD03Top3_20261104",  FS_SPREAD_POWERLAW_SHAPE = "0.3",
             FS_SPREAD_CRPS_WEIGHTS = "fire=1,adTest=0.34,areaDist=2.6,totalArea=164,top10=49.5,annualArea=520"),
  t3d04  = c(ledger = "_crpsCauseAnnD04Top3_20261104",  FS_SPREAD_POWERLAW_SHAPE = "0.4",
             FS_SPREAD_CRPS_WEIGHTS = "fire=1,adTest=0.34,areaDist=2.6,totalArea=164,top10=49.5,annualArea=520"),
  t3d05  = c(ledger = "_crpsCauseAnnD05Top3_20261104",  FS_SPREAD_POWERLAW_SHAPE = "0.5",
             FS_SPREAD_CRPS_WEIGHTS = "fire=1,adTest=0.34,areaDist=2.6,totalArea=164,top10=49.5,annualArea=520"),
  tsd03  = c(ledger = "_crpsCauseAnnD03Share_20261104", FS_SPREAD_POWERLAW_SHAPE = "0.3",
             FS_SPREAD_CRPS_WEIGHTS = "fire=1,adTest=0.34,areaDist=2.6,totalArea=164,top10=16.5,annualArea=520,topShare=111"),
  tsd04  = c(ledger = "_crpsCauseAnnD04Share_20261104", FS_SPREAD_POWERLAW_SHAPE = "0.4",
             FS_SPREAD_CRPS_WEIGHTS = "fire=1,adTest=0.34,areaDist=2.6,totalArea=164,top10=16.5,annualArea=520,topShare=111"),
  tsd05  = c(ledger = "_crpsCauseAnnD05Share_20261104", FS_SPREAD_POWERLAW_SHAPE = "0.5",
             FS_SPREAD_CRPS_WEIGHTS = "fire=1,adTest=0.34,areaDist=2.6,totalArea=164,top10=16.5,annualArea=520,topShare=111")
)
.trial <- Sys.getenv("FS_TRIAL")
if (nzchar(.trial)) {
  if (!.trial %in% names(.trials)) stop("FS_TRIAL must be one of ", paste(names(.trials), collapse = ", "))
  .tv <- .trials[[.trial]]
  .tv <- c(.trialBase[setdiff(names(.trialBase), names(.tv))], .tv[names(.tv) != "ledger"],
           FS_RF_SUFFIX = .tv[["ledger"]], FS_HC_LEDGER = .tv[["ledger"]], FS_HC_TAG = .trial,
           FS_RF_TAG = paste0(.trial, "_", gsub("\\.", "_", gsub(",", "-", Sys.getenv("FS_RF_ELFS")))))
  .tv <- .tv[!nzchar(Sys.getenv(names(.tv)))]
  if (length(.tv)) do.call(Sys.setenv, as.list(.tv))
}

.phase1Only <- isTRUE(as.logical(Sys.getenv("FS_PHASE1_ONLY", "FALSE")))
.set <- Sys.getenv("FS_SET")
## FS_SET=fit and FS_SET=phase1: the nrv fit wave (phases 1 + 2) and its phase 1 alone, under their own labels
.setLabel <- .set
if (.set %in% c("fit", "phase1")) {
  .phase1Only <- identical(.set, "phase1"); .set <- "nrv"; Sys.setenv(FS_NRV_WAVE = "fit")
}

## hindcast and score years: every fire year of the fits, 1986-2024 (decided 2026-10-25); FS_HC_YEARS=<years,...> for a subset.
## Y+1: SCANFI year Y may already show year Y's burns, so year Y+1 uses SCANFI Y (pre-burn vegetation)
.hcYears <- if (nzchar(Sys.getenv("FS_HC_YEARS"))) as.integer(.split(Sys.getenv("FS_HC_YEARS"))) else 1986:2024

if (identical(.set, "nrv")) {
  ## fit:      fit every named ELF (refit), then stop. With FS_PHASE1_ONLY=TRUE: build caches only.
  ## predict:  1000-year NRV runs, reps 1-5, from the ledger fit. Name only ELFs whose fit is DONE.
  ## ignition: refit ignition and escape only; the spread fit is reused from the ledger.
  .wave <- Sys.getenv("FS_NRV_WAVE")
  .waves <- c(fit = 13L, predict = 6L, ignition = 10L) # default workers
  if (!.wave %in% names(.waves)) stop("FS_SET=nrv needs FS_NRV_WAVE=fit, predict or ignition")
  if (.phase1Only && .wave != "fit") stop("FS_SET=nrv phase 1 is only for FS_NRV_WAVE=fit")
  .ELFs <- .split(Sys.getenv("FS_NRV_ELFS", "14.4,4.1,4.2.2"))
  .tag <- Sys.getenv("FS_NRV_TAG", paste0("2026-10-08f_", paste(.ELFs, collapse = "-")))
  .phaseSetup <- list(queue = if (.setLabel %in% c("fit", "phase1")) paste0("experiment_queue_", .setLabel, "_", .tag, ".rds")
                              else paste0("experiment_queue_nrv_", .tag, "_wave", .wave, if (.phase1Only) "_phase1", ".rds"),
                      onlyELFs = if (!identical(.ELFs, "all")) .ELFs,
                      reps = if (nzchar(Sys.getenv("FS_NRV_REPS"))) as.integer(.split(Sys.getenv("FS_NRV_REPS")))
                             else if (.wave == "predict") 1:5 else 1L,
                      n_workers = as.integer(Sys.getenv("FS_NRV_WORKERS", if (.phase1Only) 12L else .waves[[.wave]])),
                      ledgerSuffix = Sys.getenv("FS_NRV_LEDGER"), # "" = global.R's .spreadFitFileSuffix
                      refit = .wave == "fit",
                      predict = .wave == "predict")
} else if (identical(.set, "hindcast")) {
  ## one job per ELF x SCANFI year x spread parameter set (.rep); each job runs 10 decimal-year reps.
  ## FS_HC_JOBS=<ELF>/<year>/<set>,... reruns only those jobs. FS_HC_INTERLEAVE=TRUE takes one job of each ELF
  ## in turn, so workers do not queue behind one ELF's cache build.
  if (.phase1Only) stop("FS_SET=hindcast has no phase-1 form")
  if (!nzchar(Sys.getenv("FS_HC_ELFS"))) stop("FS_SET=hindcast needs FS_HC_ELFS=<ELFs with a fit, comma-separated>")
  .phaseSetup <- list(queue = paste0("experiment_queue_hindcast_", Sys.getenv("FS_HC_TAG", format(Sys.Date(), "%Y-%m-%d")), ".rds"),
                      onlyELFs = .split(Sys.getenv("FS_HC_ELFS")),
                      ## FS_HC_ALLSETS=TRUE: one job per ELF x year runs all sets (20 reps, 5 x 4), so one .rep
                      hcAllSets = isTRUE(as.logical(Sys.getenv("FS_HC_ALLSETS", "FALSE"))),
                      reps = as.integer(.split(Sys.getenv("FS_HC_SETS", if (isTRUE(as.logical(Sys.getenv("FS_HC_ALLSETS", "FALSE")))) "1" else "1,2,3,4,5"))),
                      hindcastYears = .hcYears,
                      ledgerSuffix = Sys.getenv("FS_HC_LEDGER"), # "" = production ledger
                      hcIgnitions = if (nzchar(Sys.getenv("FS_HC_IGNITIONS"))) Sys.getenv("FS_HC_IGNITIONS"), # unset: drawn ("predicted"); "observedEscapes": spread the observed escapes
                      fireCauses = if (nzchar(Sys.getenv("FS_FIRE_CAUSES"))) Sys.getenv("FS_FIRE_CAUSES"), # unset: the module defaults; e.g. "natural,unknown" (same value as the fit's)
                      n_workers = as.integer(Sys.getenv("FS_HC_WORKERS", "12")),
                      refit = FALSE,
                      predict = TRUE)
} else if (identical(.set, "refit")) {
  ## refit the named ELFs, fit only. Same rep (1) and ledger suffix ("") as production by default: a separate rep or
  ## suffix puts different labels in the cache keys and redoes data preparation (2026-10-19)
  ## With FS_PHASE1_ONLY=TRUE: build caches only, from the same rows, so a fit elsewhere (the GPSC) finds them
  if (!nzchar(Sys.getenv("FS_RF_ELFS"))) stop("FS_SET=refit needs FS_RF_ELFS")
  .phaseSetup <- list(queue = paste0("experiment_queue_refit_", Sys.getenv("FS_RF_TAG", format(Sys.Date(), "%Y-%m-%d")),
                                     if (.phase1Only) "_phase1", ".rds"),
                      onlyELFs = .split(Sys.getenv("FS_RF_ELFS")),
                      reps = as.integer(Sys.getenv("FS_RF_REP", "1")),
                      ledgerSuffix = Sys.getenv("FS_RF_SUFFIX", ""),
                      spreadLink = if (nzchar(Sys.getenv("FS_SPREAD_LINK"))) Sys.getenv("FS_SPREAD_LINK"), # unset: the module default
                      spreadWeighted = if (nzchar(Sys.getenv("FS_SPREAD_WEIGHTED"))) Sys.getenv("FS_SPREAD_WEIGHTED"), # unset: the module default (FALSE)
                      spreadGateRefusal = if (nzchar(Sys.getenv("FS_SPREAD_GATE_REFUSAL"))) Sys.getenv("FS_SPREAD_GATE_REFUSAL"), # unset: the module default ("minLik"); "simulate": a gate-refused year is still simulated and scored
                      spreadReps = if (nzchar(Sys.getenv("FS_SPREAD_REPS"))) Sys.getenv("FS_SPREAD_REPS"), # unset: the module default objfunFireReps
                      spreadCrpsWeights = if (nzchar(Sys.getenv("FS_SPREAD_CRPS_WEIGHTS"))) Sys.getenv("FS_SPREAD_CRPS_WEIGHTS"), # unset: fireSenseUtils::crpsWeightsDefault(); "fire=1,adTest=0.34,..."
                      fireCauses = if (nzchar(Sys.getenv("FS_FIRE_CAUSES"))) Sys.getenv("FS_FIRE_CAUSES"), # unset: the module defaults; e.g. "natural,unknown" (ignition, escape and spread fits)
                      spreadTests = if (nzchar(Sys.getenv("FS_SPREAD_TESTS"))) Sys.getenv("FS_SPREAD_TESTS"), # unset: the module default DEoptimTests
                      spreadInitialPop = if (nzchar(Sys.getenv("FS_SPREAD_INITIALPOP"))) Sys.getenv("FS_SPREAD_INITIALPOP"), # unset: the module default start
                      spreadInitialPopMethod = if (nzchar(Sys.getenv("FS_SPREAD_INITPOP_METHOD"))) Sys.getenv("FS_SPREAD_INITPOP_METHOD"), # unset: the module default ("calibration")
                      spreadSdBounds = if (nzchar(Sys.getenv("FS_SPREAD_SD_BOUNDS"))) Sys.getenv("FS_SPREAD_SD_BOUNDS"), # unset: the module default c(0, 10); "0,1" caps the year-effect SD at 1; "NA" = no year effect
                      prepAreaMult = if (nzchar(Sys.getenv("FS_PREP_AREA_MULT"))) Sys.getenv("FS_PREP_AREA_MULT"), # unset: the module default (10 * multiplier(size)); "40" = buffers 4x as large
                      spreadSizeLikDf = if (nzchar(Sys.getenv("FS_SPREAD_SIZELIK_DF"))) Sys.getenv("FS_SPREAD_SIZELIK_DF"), # unset: the module default sizeLikDf (5); "30" = per-fire t likelihood with df 30
                      spreadPowerLawShape = if (nzchar(Sys.getenv("FS_SPREAD_POWERLAW_SHAPE"))) Sys.getenv("FS_SPREAD_POWERLAW_SHAPE"), # unset: the module default powerLawShape (1); e.g. "2" = sharper rise of the power-law link toward p_c
                      n_workers = as.integer(Sys.getenv("FS_RF_WORKERS", length(.split(Sys.getenv("FS_RF_ELFS"))))), # each fit is a cluster; FS_RF_WORKERS caps fits at once
                      refit = TRUE,
                      predict = FALSE)
} else if (identical(.set, "score")) {
  ## Phase 4: one job scores the hindcasts of every ELF (global.R's .score branch). FS_HC_TAG and FS_HC_LEDGER name the
  ## hindcast to score; on Slurm it starts after every job of that hindcast has ended (afterany, even if some failed)
  .phaseSetup <- list(queue = paste0("experiment_queue_score_", Sys.getenv("FS_HC_TAG", format(Sys.Date(), "%Y-%m-%d")), ".rds"),
                      onlyELFs = .split(Sys.getenv("FS_HC_ELFS", "4.1")), # a single row; the job reads every ELF found
                      reps = 1L,
                      ledgerSuffix = Sys.getenv("FS_HC_LEDGER"),
                      n_workers = 1L,
                      refit = FALSE,
                      predict = FALSE,
                      score = TRUE)
} else if (identical(.set, "nrvreport")) {
  ## The NRV page: one job per ELF summarises that ELF's 5 finished NRV runs with the summary modules in mode = "multi"
  ## (global.R's .nrvReport branch) and keeps the page's tables and figures in outputs/nrvreport_<FS_NRVR_TAG>/<ELF>/.
  ## FS_NRVR_MODULES=<folder>: the summary modules from that folder instead of modules/ (a test of changed copies)
  ## FS_NRVR_MAP=TRUE: one light job redraws the map of the ELFs reported so far (outputs/nrvreport_<tag>/ELFmap.png),
  ## from the saved NRV run of the first reported ELF
  .map <- isTRUE(as.logical(Sys.getenv("FS_NRVR_MAP", "FALSE")))
  .ELFs <- if (.map) basename(dirname(Sys.glob(file.path(.basePath, "outputs", paste0("nrvreport_", Sys.getenv("FS_NRVR_TAG", "phase5")),
                                                          "*", "DONE.txt"))))[1]
           else .split(Sys.getenv("FS_NRVR_ELFS"))
  if (!length(.ELFs) || anyNA(.ELFs) || !all(nzchar(.ELFs)))
    stop("FS_SET=nrvreport needs FS_NRVR_ELFS=<ELFs whose 5 NRV runs are DONE>, or FS_NRVR_MAP=TRUE once one ELF is reported")
  .phaseSetup <- list(queue = paste0("experiment_queue_nrvreport_", Sys.getenv("FS_NRVR_TAG", "phase5"), if (.map) "_map", "_",
                                     format(Sys.time(), "%Y%m%d-%H%M"), ".rds"),
                      onlyELFs = .ELFs,
                      reps = 1L,
                      n_workers = length(.ELFs), # one job per ELF
                      refit = FALSE,
                      predict = FALSE,
                      nrvReport = Sys.getenv("FS_NRVR_TAG", "phase5"),
                      nrvReportMap = .map)
} else {
  stop("Set FS_SET=nrv, phase1, hindcast, refit, score or nrvreport")
}
message("FS_SET=", .setLabel, ": queue ", .phaseSetup$queue)

####################
# Build the queue
####################
queue_path <- .phaseSetup$queue
.ELFinds <- fireSenseUtils::runELFs(outs, whatOut = "allNames")
.ELFsFitted <- fireSenseUtils::runELFs(outs, whatOut = "fittedNamesOnly")

expt <- expand.grid(.ELFind = .ELFinds, .rep = .phaseSetup$reps, stringsAsFactors = FALSE)
expt$.GCM <- "NRV"
expt$.basePath <- .basePath
if (.phaseSetup$refit) expt$.doFit <- TRUE
if (nzchar(.phaseSetup$ledgerSuffix %||% "")) expt$.spreadFitFileSuffix <- .phaseSetup$ledgerSuffix
if (!is.null(.phaseSetup$spreadLink)) expt$.spreadLink <- .phaseSetup$spreadLink
if (!is.null(.phaseSetup$spreadWeighted)) expt$.spreadWeighted <- .phaseSetup$spreadWeighted
if (!is.null(.phaseSetup$spreadGateRefusal)) expt$.spreadGateRefusal <- .phaseSetup$spreadGateRefusal
if (!is.null(.phaseSetup$spreadReps)) expt$.spreadReps <- .phaseSetup$spreadReps
if (!is.null(.phaseSetup$spreadCrpsWeights)) expt$.spreadCrpsWeights <- .phaseSetup$spreadCrpsWeights
if (!is.null(.phaseSetup$spreadTests)) expt$.spreadTests <- .phaseSetup$spreadTests
if (!is.null(.phaseSetup$spreadInitialPop)) expt$.spreadInitialPop <- .phaseSetup$spreadInitialPop
if (!is.null(.phaseSetup$spreadInitialPopMethod)) expt$.spreadInitialPopMethod <- .phaseSetup$spreadInitialPopMethod
if (!is.null(.phaseSetup$spreadSdBounds)) expt$.spreadSdBounds <- .phaseSetup$spreadSdBounds
if (!is.null(.phaseSetup$prepAreaMult)) expt$.prepAreaMult <- .phaseSetup$prepAreaMult
if (!is.null(.phaseSetup$spreadSizeLikDf)) expt$.spreadSizeLikDf <- .phaseSetup$spreadSizeLikDf
if (!is.null(.phaseSetup$spreadPowerLawShape)) expt$.spreadPowerLawShape <- .phaseSetup$spreadPowerLawShape
expt$.packagePath <- .packagePath # each job keeps this library (global.R paths$packagePath)
if (!is.null(.phaseSetup$hcIgnitions)) expt$.hcIgnitions <- .phaseSetup$hcIgnitions
if (isTRUE(.phaseSetup$hcAllSets)) expt$.hcAllSets <- TRUE
if (!is.null(.phaseSetup$fireCauses)) expt$.fireCauses <- .phaseSetup$fireCauses
## Biomass_core cohort-reduction options (#131), e.g. FS_COHORT_REMOVE_SUPPRESSED=0.01,0.75 FS_COHORT_ESTABLISHMENT_LIMIT=20,5
if (nzchar(Sys.getenv("FS_COHORT_REMOVE_SUPPRESSED"))) expt$.cohortRemoveSuppressed <- Sys.getenv("FS_COHORT_REMOVE_SUPPRESSED")
if (nzchar(Sys.getenv("FS_COHORT_SQUASH"))) expt$.cohortSquash <- Sys.getenv("FS_COHORT_SQUASH")
if (nzchar(Sys.getenv("FS_COHORT_ESTABLISHMENT_LIMIT"))) expt$.cohortEstablishmentLimit <- Sys.getenv("FS_COHORT_ESTABLISHMENT_LIMIT")
## the phase and the run's diagnostics are columns, so a worker reads them as global.R's defaultDots
if (isTRUE(.phaseSetup$score)) {expt$.score <- TRUE; expt$.hcYears <- paste(.hcYears, collapse = ",")}
if (!is.null(.phaseSetup$nrvReport)) expt$.nrvReport <- .phaseSetup$nrvReport
if (isTRUE(.phaseSetup$nrvReportMap)) expt$.nrvReportMap <- TRUE
expt$.phase1Only <- .phase1Only
expt$.phase2Only <- !(.phase1Only || .phaseSetup$predict)
.optionalColumn <- function(col, env, f = identity) if (nzchar(Sys.getenv(env))) expt[[col]] <<- f(Sys.getenv(env))
.optionalColumn(".workerMemoryGB", "FS_WORKER_MEM_GB", as.numeric)
.optionalColumn(".cacheChaining", "FS_NO_CACHECHAINING", function(x) !isTRUE(as.logical(x)))
.optionalColumn(".fitQueue", "FS_FIT_QUEUE")
## NRV archive: a predict wave uploads into its own campaign folder, so it never overwrites the runs users read now
if (identical(.set, "nrv") && isTRUE(.phaseSetup$predict) && !(nzchar(Sys.getenv("FS_ARCHIVE_DIR")) && nzchar(Sys.getenv("FS_ARCHIVE_LEDGER"))))
  stop("FS_NRV_WAVE=predict needs FS_ARCHIVE_DIR (the campaign's own Drive folder) and FS_ARCHIVE_LEDGER (its ledger file name)")
.optionalColumn(".uploadGSdir", "FS_ARCHIVE_DIR")
.optionalColumn(".archiveLedger", "FS_ARCHIVE_LEDGER")
.optionalColumn(".afterQueue", "FS_AFTER_QUEUE")
.optionalColumn(".diagFile", "FS_DIAG_FILE")
.optionalColumn(".rprofPrefix", "FS_RPROF")
.optionalColumn(".scoreSkip", "FS_SCORE_SKIP") # FS_SET=score: ELFs left out of the scoring, comma-separated
.optionalColumn(".nrvReportModules", "FS_NRVR_MODULES") # FS_SET=nrvreport: the folder of the summary modules

if (is.null(.phaseSetup$onlyELFs)) {
  ## every ELF, by ecozone priority
  top <- c("4", "6", "5", "9", "14", "12", "11", "15")
  expt <- expt[order(match(sub("\\..*", "", expt$.ELFind), top, nomatch = length(top) + 1)), ]
} else {
  .missing <- setdiff(.phaseSetup$onlyELFs, expt$.ELFind)
  if (length(.missing)) warning("Not in the ELF map: ", paste(.missing, collapse = ", "))
  expt <- expt[expt$.ELFind %in% .phaseSetup$onlyELFs, , drop = FALSE]
  expt <- expt[order(match(expt$.ELFind, .phaseSetup$onlyELFs)), , drop = FALSE]
}
if (!.phaseSetup$refit)
  message(sum(expt$.ELFind %in% .ELFsFitted), " of ", NROW(expt), " jobs' ELFs have a fit")

if (!is.null(.phaseSetup$hindcastYears)) {
  .ny <- length(.phaseSetup$hindcastYears)
  expt <- expt[rep(seq_len(NROW(expt)), each = .ny), , drop = FALSE]
  expt$.hindcastYear <- rep(.phaseSetup$hindcastYears, times = NROW(expt) / .ny)
  if (nzchar(Sys.getenv("FS_HC_JOBS")))
    expt <- expt[paste(expt$.ELFind, expt$.hindcastYear, expt$.rep, sep = "/") %in% .split(Sys.getenv("FS_HC_JOBS")), , drop = FALSE]
  if (isTRUE(as.logical(Sys.getenv("FS_HC_INTERLEAVE", "FALSE"))))
    expt <- expt[order(ave(seq_len(NROW(expt)), expt$.ELFind, FUN = seq_along), match(expt$.ELFind, unique(expt$.ELFind))), , drop = FALSE]
}
rownames(expt) <- NULL
## every queue column must be a name global.R uses (a defaultDots entry, a setupProject dot or a get0() name):
## a worker silently ignores a column the script never reads, e.g. a misspelled setting
.globalNames <- local({
  pd <- utils::getParseData(parse("global.R", keep.source = TRUE))
  unique(gsub("^[\"']|[\"']$", "", pd$text[pd$token %in% c("SYMBOL", "SYMBOL_SUB", "STR_CONST")]))
})
if (length(.unread <- setdiff(names(expt), .globalNames)))
  stop("queue column(s) that global.R never reads: ", paste(.unread, collapse = ", "))
message("Queueing ", NROW(expt), " jobs, fire years ", .fireYearStart, ":", .fireYearEnd)

####################
# Run. Worker panes take the tmux session's environment, not this process's.
# With Slurm (the GPSC), each worker is a one-node job; it takes this process's environment.
####################
if (nzchar(Sys.which("sbatch"))) {
  ## compute nodes reach the internet only through the science proxy; workers and the cluster stay direct
  if (!nzchar(Sys.getenv("https_proxy")))
    Sys.setenv(https_proxy = "http://webproxy.science.gc.ca:8888", http_proxy = "http://webproxy.science.gc.ca:8888",
               no_proxy = "localhost,127.0.0.1,science.gc.ca")
  .oneCore <- identical(.set, "hindcast") || identical(.set, "score") || (identical(.set, "nrv") && .phaseSetup$predict)
  expt$.cores <- 40L # spread fits on the job's own node, not on .cores's machines; 40 at 9 GB each (the fireSenseUtils default; FS_WORKER_MEM_GB sets it)
  .slurmTmp <- file.path(.basePath, "scratch", "tmp") # TMPDIR of the jobs (see sbatch_opts)
  dir.create(.slurmTmp, recursive = TRUE, showWarnings = FALSE)
  .submit <- function(df, queue_path, after) SpaDES.project::experimentSBATCH(
    df          = df,
    global_path = "global.R",
    n_workers   = .phaseSetup$n_workers,
    queue_path  = queue_path,
    log_dir     = file.path("logs", sub("\\.rds$", "", basename(queue_path))),
    ## the account's allocation is on gpsc8 (64 CPUs, ~486 GB per standard node); its "low" QoS allows 0 CPUs
    ## phase 1 is one core (it stops before the fit); FS_SBATCH_AFTER=<job ids>
    ## starts these jobs only after those succeed (a fit after its phase 1); FS_HC_AFTER_FITS=TRUE: each ELF's hindcast
    ## after its own fit job, if one is queued or running (found by its log folder; none: it starts now and stops if the ledger has no fit)
    ## a compute node's /tmp is a 500 MB tmpfs: R's tempdir() there (reproducible.tempPath, where downloads and
    ## postProcess land first) cut every file at ~500 MB (SCANFI landcover, crop "too few values", 2026-10-29)
    sbatch_opts = c(list(clusters = "gpsc8", account = "nrcan_forsite", partition = "standard",
                         qos = "nrcan__standard_gpsc8_allocation", nodes = 1, ntasks = 1,
                         export = paste0("ALL,TMPDIR=", .slurmTmp)),
                    if (.phase1Only) list(cpus_per_task = 1, mem = "96G", time = "1-00:00:00") # 32G was OOM-killed (12.1_2, prepSpreadFitData); to be set from measured peaks
                    else if (identical(.set, "nrvreport") && isTRUE(.phaseSetup$nrvReportMap)) # the ELF map only
                      list(cpus_per_task = 1, mem = "4G", time = "01:00:00") # 2.5 min, MaxRSS 1.7 GB
                    else if (identical(.set, "nrvreport")) # one ELF's 5 runs; the landscape metrics and figures use the CPUs
                      list(cpus_per_task = 4, mem = Sys.getenv("FS_SBATCH_MEM", "24G"), time = "1-00:00:00") # 6.4: 42 min, MaxRSS 13 GB
                    else if (identical(.set, "score")) # the scoring job reads 12 ELFs at once (global.R); ~8 GB each
                      list(cpus_per_task = 12, mem = Sys.getenv("FS_SBATCH_MEM", "160G"), time = "2-00:00:00")
                    else if (.oneCore) # one core per ELF x year (hindcast) or ELF x set (NRV) job; FS_SBATCH_MEM until the MaxRSS of the first jobs is known
                      list(cpus_per_task = 1, mem = Sys.getenv("FS_SBATCH_MEM", "64G"), time = if (identical(.set, "nrv")) "7-00:00:00" else "2-00:00:00")
                    else list(cpus_per_task = 42, # 40 workers + 2 kept free; 7.1 GB/CPU is the partition's limit
                              ## per ELF (FS_SBATCH_MEM): 40 x the worker peak clusters records in fitMemory.rds, / 0.9 headroom,
                              ## + the master. 13.1 and 6.6.1 need ~1-2 GB per worker (294G); 12.1_2 needs 8.5 (420G)
                              mem = Sys.getenv("FS_SBATCH_MEM", "294G"),
                              time = "7-00:00:00"), # required by gpsc8, though the partition has no limit
                    if (length(after)) list(dependency = paste0(if (identical(.set, "score")) "afterany:" else "afterok:", paste(after, collapse = ":")))),
    ss_id       = NULL # compute nodes have no internet: the local queue only
  )

  .after <- strsplit(Sys.getenv("FS_SBATCH_AFTER"), ",")[[1]]
  ## FS_SCORE_NOW=TRUE: score what has finished now, without waiting for the jobs still queued (rerun later for the rest)
  if (identical(.set, "score") && !isTRUE(as.logical(Sys.getenv("FS_SCORE_NOW", "FALSE")))) # every job of the hindcast being scored
    .after <- c(.after, vapply(Filter(function(j) length(j) == 3 && grepl(paste0("experiment_queue_hindcast_", Sys.getenv("FS_HC_TAG"), "(_|/)"), j[3]),
                                      strsplit(system2("squeue", c("-M", "gpsc8", "-u", Sys.info()[["user"]], "-h", "-o", "'%i|%T|%o'"), stdout = TRUE), "\\|")),
                               `[`, "", 1))
  if (.oneCore && !identical(.set, "score") && isTRUE(as.logical(Sys.getenv("FS_HC_AFTER_FITS", "FALSE")))) {
    .jobs <- strsplit(system2("squeue", c("-M", "gpsc8", "-u", Sys.info()[["user"]], "-h", "-o", "'%i|%T|%o'"), stdout = TRUE), "\\|")
    .jobs <- Filter(function(j) length(j) == 3 && j[2] %in% c("RUNNING", "PENDING"), .jobs)
    for (.elf in unique(expt$.ELFind)) {
      .fit <- vapply(Filter(function(j) grepl(paste0("experiment_queue_refit_", if (nzchar(.trial)) .trial else "pile[A-Za-z0-9]*", "_", gsub("\\.", "_", .elf), "/"), j[3]), .jobs), `[`, "", 1)
      .submit(expt[expt$.ELFind == .elf, , drop = FALSE], sub("\\.rds$", paste0("_", .elf, ".rds"), queue_path), c(.after, .fit))
    }
  } else .submit(expt, queue_path, .after)
  q(save = "no")
}
if (!nzchar(Sys.getenv("TMUX"))) stop("Run expt.R inside tmux")

workers <- SpaDES.project::experimentTmux(
  df                  = expt,
  global_path         = "global.R",
  n_workers           = .phaseSetup$n_workers,
  queue_path          = queue_path,
  on_error            = "continue",
  delay_before_source = 120,
  statusCalculate = quote({dd <- dir(file.path("/mnt/fast/outputs", runName), recursive = TRUE, full.names = TRUE)
                           ee <- grep(value = TRUE, pattern = "objFun.*png$", dd)
                           fi <- file.info(ee)
                           tail(fi[order(fi$mtime),], 1) |> rownames() |> dirname()}),
  folderWithIterInFilename = quote({dd <- dir(file.path("/mnt/fast/outputs", runName), recursive = TRUE, full.names = TRUE)
                                    ee <- grep(value = TRUE, pattern = "hists", dd)
                                    fi <- file.info(ee)
                                    tail(fi[order(fi$mtime),], 1) |> rownames() |> dirname()}),
  workersToMonitor = outs$params$fireSense_spreadFit$cores,
  runNameLabel = quote(colnames(q)[1]),
  ss_id = "https://drive.google.com/drive/folders/1X9-mRjyLMNpgkP_cfqhbr_AQEPOsVCHf"
)
