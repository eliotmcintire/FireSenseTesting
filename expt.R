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
##   FS_SET=chain FS_TRIAL=d05,wd05 FS_RF_ELFS=6.6.2,14.3 [FS_FIT_MEM=6.6.2=320G] [FS_FIT_WORKER_GB=6.6.2=6.1] [FS_CHAIN_SUFFIX=_e]
##     Rscript expt.R   (Slurm: every trial x ELF's fit, hindcast and score in one call, one queue; see "FS_SET=chain" below)
##   FS_DRY_RUN=TRUE with any Slurm FS_SET: write the queue and the job scripts (logs/<queue>/), submit nothing

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
# Every launch installs: into a new dev library, never into one a running job uses.
# @modsForFireSense branches = development + our open PRs (R/updateModsForFireSense.sh).
####################
## The project library (.Rprofile) is the frozen "stable" library: nothing installs into it again. The launcher
## (experimentTmux/experimentSBATCH devPackages, below) builds a dev library beside it, <stable>_dev/<id>, with hard links
## to the current one plus whatever changed, and starts every worker on c(<dev>, <stable>); global.R has no packagePath.
## With no FS_SET, this run only builds the dev library.
.stableLib <- .libPaths()[1]
.devCurrent <- file.path(paste0(.stableLib, "_dev"), "current") # what SpaDES.project::currentDevLibrary() reads; read here
if (file.exists(.devCurrent)) .libPaths(c(readLines(.devCurrent, n = 1L), .libPaths())) # because stable's SpaDES.project predates it
## (HEAD): Require compares with the branch's latest commit; a bare @branch installs nothing when the package is present
.devPackages <- c("PredictiveEcology/Require@development (HEAD)",
                  "PredictiveEcology/SpaDES.project@development (HEAD)",
                  "PredictiveEcology/reproducible@development (HEAD)",
                  "PredictiveEcology/SpaDES.core@development (HEAD)",
                  "PredictiveEcology/SpaDES.tools@development (HEAD)",
                  "PredictiveEcology/LandR@modsForFireSense (HEAD)",
                  "PredictiveEcology/clusters@development (HEAD)",
                  "PredictiveEcology/filelock@main (HEAD)",
                  "PredictiveEcology/climateData@modsForFireSense (HEAD)",
                  "PredictiveEcology/quickPlot@development (HEAD)",
                  "googlesheets4", # the sheet-backed queue (a Suggests of SpaDES.project)
                  "PredictiveEcology/fireSenseUtils@development (HEAD)")
if (utils::packageVersion("SpaDES.project") < "1.2.0.9034") { # 9034: queue groups (FS_SET=chain). First launch only: SpaDES.project from a throwaway library
  .boot <- tempfile("spbootstrap"); dir.create(.boot)
  pak::pkg_install("PredictiveEcology/SpaDES.project@development", lib = .boot, dependencies = FALSE, ask = FALSE)
  loadNamespace("SpaDES.project", lib.loc = .boot)
  ## build the dev library now, first on the path: SpaDES.project is loaded again from the path before the launch, and an
  ## older one there ignores devPackages (2026-11-05: GPSC jobs started on the stable library alone)
  .libPaths(c(SpaDES.project::newDevLibrary(.devPackages, stable = .stableLib)[1], .libPaths()))
}
if (!nzchar(Sys.getenv("FS_SET"))) {SpaDES.project::newDevLibrary(.devPackages, stable = .stableLib); quit(save = "no")} # no FS_SET: build the dev library only
## this session's own setupProject() (preRunSetupProject below) uses the current dev library and leaves the .Rprofile, as a worker does
options(spades.packagePath = SpaDES.project::currentDevLibrary(stable = .stableLib), SpaDES.project.updateRprofile = FALSE)

options(spades.useRequire = FALSE) # nothing installs from here on: the dev library above has it, so setupProject() must not run Require
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

## Spread-fit trials. FS_TRIAL=<name> gives that trial's FS_* values (a value in the environment wins; see .step()), so its fit,
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
## 2026-11-07: the same 10 arms with every fire spreading on the whole ELF (no buffer cut; fireSenseUtils#219) and 50 replicates:
## w<arm>, ledger <arm ledger>Whole_20261107
.trials <- c(.trials, setNames(lapply(.trials, function(t) c(ledger = sub("_20261104$", "Whole_20261107", t[["ledger"]]),
                                                              t[names(t) != "ledger"], FS_WHOLE_LANDSCAPE = "TRUE", FS_SPREAD_REPS = "50")),
                               paste0("w", names(.trials))))
## 2026-11-08: the whole-landscape arms run longer (half the replicates; Eliot): wl<arm>, ledger <arm ledger>WholeLong_20261108,
## early stop judged over 400 generations, never before 400 (300/350 otherwise)
.trials <- c(.trials, setNames(lapply(.trials[!startsWith(names(.trials), "w")], function(t)
  c(ledger = sub("_20261104$", "WholeLong_20261108", t[["ledger"]]), t[names(t) != "ledger"], FS_WHOLE_LANDSCAPE = "TRUE",
    FS_SPREAD_REPS = "50", FS_DEOPTIM_WINDOW = "400", FS_DEOPTIM_MIN_GENERATIONS = "400")),
  paste0("wl", names(.trials)[!startsWith(names(.trials), "w")])))
## 2026-11-10: controls. The w*/wl* fits ran on stale buffered tables (a cached dataPrepFit step restored the Nov-4
## tables; its cache entries were deleted on the GPSC 2026-11-10). wd04/wd05 again on truly whole tables, today's code:
## <arm>fix, ledger <arm ledger>WholeFix_20261110, 400/400 stop
.trials <- c(.trials, setNames(lapply(.trials[c("wd04", "wd05")], function(t)
  c(ledger = sub("Whole_20261107$", "WholeFix_20261110", t[["ledger"]]), t[names(t) != "ledger"],
    FS_DEOPTIM_WINDOW = "400", FS_DEOPTIM_MIN_GENERATIONS = "400")),
  c("wd04fix", "wd05fix")))
## 2026-11-11 campaign: fires that start in the ELF core only, at their whole extent, in fit, hindcast and score; one
## escape definition shared by every module; ignition and escape models trained on the core; the spread climate variable
## chosen per ELF from the larger candidate pool (dataPrepFit spreadClimateCandidates). Whole landscape, 50 reps, 400/400.
## c<arm>: no per-fire effect (ledger <arm>Core_20261111); c<arm>r: with it (ledger <arm>CoreRE_20261111);
## arms d04, d05, t3d04, t3d05, tsd04, tsd05
.coreArms <- c("d04", "d05", "t3d04", "t3d05", "tsd04", "tsd05")
.coreTrial <- function(t, tag, fireSD) c(ledger = sub("_20261104$", paste0(tag, "_20261111"), t[["ledger"]]), t[names(t) != "ledger"],
                                         FS_WHOLE_LANDSCAPE = "TRUE", FS_SPREAD_REPS = "50", FS_DEOPTIM_WINDOW = "400",
                                         FS_DEOPTIM_MIN_GENERATIONS = "400", fireSD)
.trials <- c(.trials,
             setNames(lapply(.trials[.coreArms], .coreTrial, tag = "Core", fireSD = c(FS_SPREAD_FIRE_SD_BOUNDS = "NA")),
                      paste0("c", .coreArms)),
             setNames(lapply(.trials[.coreArms], .coreTrial, tag = "CoreRE", fireSD = character()),
                      paste0("c", .coreArms, "r")))
## 2026-11-12: the same 12 core trials with the ignition fit weighted by each coarse pixel's core share (dataPrepFit#101,
## ignitionFit#55); the spread fits are reused (each trial's spread ledger copied to the new suffix before launch), so only
## ignition and escape refit: c<arm>i / c<arm>ri, ledger <arm>CoreIg_20261112 / <arm>CoreREIg_20261112
.coreTrials <- paste0("c", c(.coreArms, paste0(.coreArms, "r")))
.trials <- c(.trials, setNames(lapply(.trials[.coreTrials], function(t)
  c(ledger = sub("_20261111$", "Ig_20261112", t[["ledger"]]), t[names(t) != "ledger"])), paste0(.coreTrials, "i")))
## 2026-11-14: the spread link (fireSenseUtils#229/#230, SpaDES.tools#157). The power law had no lower asymptote (p clamped
## to 0 below x ~ 0), so escaped fires stalled under 50 ha. Arms are the core top-10 x3 trials (ct3d04/ct3d05 settings, core
## starts, whole landscape, 50 reps, 400/400), refit from scratch (spread, ignition and escape) under the new ledger:
##   lp04 / lp05: "powerLawSmooth", delta 0.4 / 0.5, lower asymptote 0.13;  lgz: "richards" nu 0 (Gompertz), 0.13;
##   lri: "richards" nu 0.5, 0.13;  each also with the per-fire effect (suffix r);  lp04r0 / lgzr0: lower asymptote 0.001, per-fire only
## Experiment fit ledgers go to the Drive testing folder, not production (Eliot 2026-11-13)
.ledgerFolderTesting <- "https://drive.google.com/drive/folders/1nqSVuGRYxIvcgZMn7TIbn5HfRiUHR2xq"
.linkTrial <- function(base, tag, set, fireSD) c(ledger = paste0("_crpsCauseAnn", tag, "_20261114"),
                                               .trials[[base]][setdiff(names(.trials[[base]]), c("ledger", "FS_SPREAD_FIRE_SD_BOUNDS"))],
                                               set, fireSD, FS_LEDGER_FOLDER = .ledgerFolderTesting)
.linkArms <- list(lp04 = list("ct3d04", "LinkP04", c(FS_SPREAD_LINK = "powerLawSmooth")),
                  lp05 = list("ct3d05", "LinkP05", c(FS_SPREAD_LINK = "powerLawSmooth")),
                  lgz  = list("ct3d04", "LinkGz",  c(FS_SPREAD_LINK = "richards", FS_SPREAD_RICHARDS_NU = "0")),
                  lri  = list("ct3d04", "LinkRi",  c(FS_SPREAD_LINK = "richards", FS_SPREAD_RICHARDS_NU = "0.5")))
for (.a in names(.linkArms)) { .l <- .linkArms[[.a]]
  .trials[[.a]] <- .linkTrial(.l[[1]], .l[[2]], .l[[3]], c(FS_SPREAD_FIRE_SD_BOUNDS = "NA"))
  .trials[[paste0(.a, "r")]] <- .linkTrial(.l[[1]], paste0(.l[[2]], "RE"), .l[[3]], character())
  if (.a %in% c("lp04", "lgz"))
    .trials[[paste0(.a, "r0")]] <- .linkTrial(.l[[1]], paste0(.l[[2]], "RE0"), c(.l[[3]], FS_SPREAD_LOWER_ASYMPTOTE = "0.001"), character())
}
## the ELF map and which ELFs have a fit: the same for every launch of this call
.ELFinds <- fireSenseUtils::runELFs(outs, whatOut = "allNames")
.ELFsFitted <- fireSenseUtils::runELFs(outs, whatOut = "fittedNamesOnly")
.slurm <- nzchar(Sys.which("sbatch"))
## FS_DRY_RUN=TRUE (Slurm): write the queue and the job scripts (logs/<queue>/worker_*.sh), submit nothing
.dryRun <- isTRUE(as.logical(Sys.getenv("FS_DRY_RUN", "FALSE")))

## One step's queue rows: the FS_SET `set` with the trial's settings. A setting is read from `over` (FS_SET=chain's
## per-step values), else the environment, else the trial's value (FS_TRIAL, see "Spread-fit trials" above), else
## its default. Returns the rows and what the launch needs (queue name, workers, the setting lookup).
.step <- function(set, trial = Sys.getenv("FS_TRIAL"), over = character()) {
  .tv <- character()
  .ev <- function(x, unset = "") {
    if (x %in% names(over)) return(over[[x]])
    v <- Sys.getenv(x)
    if (nzchar(v)) v else if (x %in% names(.tv)) .tv[[x]] else unset
  }
  if (nzchar(trial)) {
    if (!trial %in% names(.trials)) stop("FS_TRIAL must be one of ", paste(names(.trials), collapse = ", "))
    .t <- .trials[[trial]]
    .tv <- c(.trialBase[setdiff(names(.trialBase), names(.t))], .t[names(.t) != "ledger"],
             FS_RF_SUFFIX = .t[["ledger"]], FS_HC_LEDGER = .t[["ledger"]], FS_HC_TAG = trial,
             FS_RF_TAG = paste0(trial, "_", gsub("\\.", "_", gsub(",", "-", .ev("FS_RF_ELFS")))))
  }

  .phase1Only <- isTRUE(as.logical(.ev("FS_PHASE1_ONLY", "FALSE")))
  .set <- set
  ## FS_SET=fit and FS_SET=phase1: the nrv fit wave (phases 1 + 2) and its phase 1 alone, under their own labels
  .setLabel <- .set
  if (.set %in% c("fit", "phase1")) {
    .phase1Only <- identical(.set, "phase1"); .set <- "nrv"; over["FS_NRV_WAVE"] <- "fit"
  }

  ## hindcast and score years: every fire year of the fits, 1986-2024 (decided 2026-10-25); FS_HC_YEARS=<years,...> for a subset.
  ## Y+1: SCANFI year Y may already show year Y's burns, so year Y+1 uses SCANFI Y (pre-burn vegetation)
  .hcYears <- if (nzchar(.ev("FS_HC_YEARS"))) as.integer(.split(.ev("FS_HC_YEARS"))) else 1986:2024

  if (identical(.set, "nrv")) {
    ## fit:      fit every named ELF (refit), then stop. With FS_PHASE1_ONLY=TRUE: build caches only.
    ## predict:  1000-year NRV runs, reps 1-5, from the ledger fit. Name only ELFs whose fit is DONE.
    ## ignition: refit ignition and escape only; the spread fit is reused from the ledger.
    .wave <- .ev("FS_NRV_WAVE")
    .waves <- c(fit = 13L, predict = 6L, ignition = 10L) # default workers
    if (!.wave %in% names(.waves)) stop("FS_SET=nrv needs FS_NRV_WAVE=fit, predict or ignition")
    if (.phase1Only && .wave != "fit") stop("FS_SET=nrv phase 1 is only for FS_NRV_WAVE=fit")
    .ELFs <- .split(.ev("FS_NRV_ELFS", "14.4,4.1,4.2.2"))
    .tag <- .ev("FS_NRV_TAG", paste0("2026-10-08f_", paste(.ELFs, collapse = "-")))
    .phaseSetup <- list(queue = if (.setLabel %in% c("fit", "phase1")) paste0("experiment_queue_", .setLabel, "_", .tag, ".rds")
                                else paste0("experiment_queue_nrv_", .tag, "_wave", .wave, if (.phase1Only) "_phase1", ".rds"),
                        onlyELFs = if (!identical(.ELFs, "all")) .ELFs,
                        reps = if (nzchar(.ev("FS_NRV_REPS"))) as.integer(.split(.ev("FS_NRV_REPS")))
                               else if (.wave == "predict") 1:5 else 1L,
                        n_workers = as.integer(.ev("FS_NRV_WORKERS", if (.phase1Only) 12L else .waves[[.wave]])),
                        ledgerSuffix = .ev("FS_NRV_LEDGER"), # "" = global.R's .spreadFitFileSuffix
                        refit = .wave == "fit",
                        predict = .wave == "predict")
  } else if (identical(.set, "hindcast")) {
    ## one job per ELF x SCANFI year x spread parameter set (.rep); each job runs 10 decimal-year reps.
    ## FS_HC_JOBS=<ELF>/<year>/<set>,... reruns only those jobs. FS_HC_INTERLEAVE=TRUE takes one job of each ELF
    ## in turn, so workers do not queue behind one ELF's cache build.
    if (.phase1Only) stop("FS_SET=hindcast has no phase-1 form")
    if (!nzchar(.ev("FS_HC_ELFS"))) stop("FS_SET=hindcast needs FS_HC_ELFS=<ELFs with a fit, comma-separated>")
    .phaseSetup <- list(queue = paste0("experiment_queue_hindcast_", .ev("FS_HC_TAG", format(Sys.Date(), "%Y-%m-%d")), ".rds"),
                        onlyELFs = .split(.ev("FS_HC_ELFS")),
                        ## FS_HC_ALLSETS=TRUE: one job per ELF x year runs all sets (20 reps, 5 x 4), so one .rep
                        hcAllSets = isTRUE(as.logical(.ev("FS_HC_ALLSETS", "FALSE"))),
                        reps = as.integer(.split(.ev("FS_HC_SETS", if (isTRUE(as.logical(.ev("FS_HC_ALLSETS", "FALSE")))) "1" else "1,2,3,4,5"))),
                        hindcastYears = .hcYears,
                        ledgerSuffix = .ev("FS_HC_LEDGER"), # "" = production ledger
                        hcIgnitions = if (nzchar(.ev("FS_HC_IGNITIONS"))) .ev("FS_HC_IGNITIONS"), # unset: drawn ("predicted"); "observedEscapes": spread the observed escapes
                        fireCauses = if (nzchar(.ev("FS_FIRE_CAUSES"))) .ev("FS_FIRE_CAUSES"), # unset: the module defaults; e.g. "natural,unknown" (same value as the fit's)
                        n_workers = as.integer(.ev("FS_HC_WORKERS", "12")),
                        refit = FALSE,
                        predict = TRUE)
  } else if (identical(.set, "refit")) {
    ## refit the named ELFs, fit only. Same rep (1) and ledger suffix ("") as production by default: a separate rep or
    ## suffix puts different labels in the cache keys and redoes data preparation (2026-10-19)
    ## With FS_PHASE1_ONLY=TRUE: build caches only, from the same rows, so a fit elsewhere (the GPSC) finds them
    if (!nzchar(.ev("FS_RF_ELFS"))) stop("FS_SET=refit needs FS_RF_ELFS")
    .phaseSetup <- list(queue = paste0("experiment_queue_refit_", .ev("FS_RF_TAG", format(Sys.Date(), "%Y-%m-%d")),
                                       if (.phase1Only) "_phase1", ".rds"),
                        onlyELFs = .split(.ev("FS_RF_ELFS")),
                        reps = as.integer(.ev("FS_RF_REP", "1")),
                        ledgerSuffix = .ev("FS_RF_SUFFIX", ""),
                        spreadLink = if (nzchar(.ev("FS_SPREAD_LINK"))) .ev("FS_SPREAD_LINK"), # unset: the module default
                        spreadLowerAsymptote = if (nzchar(.ev("FS_SPREAD_LOWER_ASYMPTOTE"))) .ev("FS_SPREAD_LOWER_ASYMPTOTE"), # unset: the module default lowerAsymptote (0.13); "0.001" = the minimum
                        spreadRichardsNu = if (nzchar(.ev("FS_SPREAD_RICHARDS_NU"))) .ev("FS_SPREAD_RICHARDS_NU"), # unset: the module default richardsNu (0.5); "0" = Gompertz
                        spreadWeighted = if (nzchar(.ev("FS_SPREAD_WEIGHTED"))) .ev("FS_SPREAD_WEIGHTED"), # unset: the module default (FALSE)
                        spreadGateRefusal = if (nzchar(.ev("FS_SPREAD_GATE_REFUSAL"))) .ev("FS_SPREAD_GATE_REFUSAL"), # unset: the module default ("minLik"); "simulate": a gate-refused year is still simulated and scored
                        spreadReps = if (nzchar(.ev("FS_SPREAD_REPS"))) .ev("FS_SPREAD_REPS"), # unset: the module default objfunFireReps
                        spreadCrpsWeights = if (nzchar(.ev("FS_SPREAD_CRPS_WEIGHTS"))) .ev("FS_SPREAD_CRPS_WEIGHTS"), # unset: fireSenseUtils::crpsWeightsDefault(); "fire=1,adTest=0.34,..."
                        fireCauses = if (nzchar(.ev("FS_FIRE_CAUSES"))) .ev("FS_FIRE_CAUSES"), # unset: the module defaults; e.g. "natural,unknown" (ignition, escape and spread fits)
                        spreadTests = if (nzchar(.ev("FS_SPREAD_TESTS"))) .ev("FS_SPREAD_TESTS"), # unset: the module default DEoptimTests
                        spreadInitialPop = if (nzchar(.ev("FS_SPREAD_INITIALPOP"))) .ev("FS_SPREAD_INITIALPOP"), # unset: the module default start
                        spreadInitialPopMethod = if (nzchar(.ev("FS_SPREAD_INITPOP_METHOD"))) .ev("FS_SPREAD_INITPOP_METHOD"), # unset: the module default ("calibration")
                        spreadSdBounds = if (nzchar(.ev("FS_SPREAD_SD_BOUNDS"))) .ev("FS_SPREAD_SD_BOUNDS"), # unset: the module default c(0, 10); "0,1" caps the year-effect SD at 1; "NA" = no year effect
                        spreadFireSdBounds = if (nzchar(.ev("FS_SPREAD_FIRE_SD_BOUNDS"))) .ev("FS_SPREAD_FIRE_SD_BOUNDS"), # unset: the module default c(0, 10) (whole-landscape fits); "NA" = no per-fire effect
                        prepAreaMult = if (nzchar(.ev("FS_PREP_AREA_MULT"))) .ev("FS_PREP_AREA_MULT"), # unset: the module default (10 * multiplier(size)); "40" = buffers 4x as large
                        spreadSizeLikDf = if (nzchar(.ev("FS_SPREAD_SIZELIK_DF"))) .ev("FS_SPREAD_SIZELIK_DF"), # unset: the module default sizeLikDf (5); "30" = per-fire t likelihood with df 30
                        wholeLandscape = if (nzchar(.ev("FS_WHOLE_LANDSCAPE"))) .ev("FS_WHOLE_LANDSCAPE"), # unset: buffered fit; "TRUE": every fire spreads on the whole ELF (whole-ELF covariate tables from dataPrepFit)
                        spreadPowerLawShape = if (nzchar(.ev("FS_SPREAD_POWERLAW_SHAPE"))) .ev("FS_SPREAD_POWERLAW_SHAPE"), # unset: the module default powerLawShape (1); e.g. "2" = sharper rise of the power-law link toward p_c
                        n_workers = as.integer(.ev("FS_RF_WORKERS", length(.split(.ev("FS_RF_ELFS"))))), # each fit is a cluster; FS_RF_WORKERS caps fits at once
                        refit = TRUE,
                        predict = FALSE)
  } else if (identical(.set, "score")) {
    ## Phase 4: one job scores the hindcasts of every ELF (global.R runs fireSimulator_validation). FS_HC_TAG and FS_HC_LEDGER name the
    ## hindcast to score; on Slurm it starts after every job of that hindcast has ended (afterany, even if some failed)
    .phaseSetup <- list(queue = paste0("experiment_queue_score_", .ev("FS_HC_TAG", format(Sys.Date(), "%Y-%m-%d")), ".rds"),
                        onlyELFs = .split(.ev("FS_HC_ELFS", "4.1")), # a single row; the job reads every ELF found
                        reps = 1L,
                        ledgerSuffix = .ev("FS_HC_LEDGER"),
                        hcIgnitions = if (nzchar(.ev("FS_HC_IGNITIONS"))) .ev("FS_HC_IGNITIONS"), # scores the observedEscapes hindcast (its own folders)
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
    .map <- isTRUE(as.logical(.ev("FS_NRVR_MAP", "FALSE")))
    .ELFs <- if (.map) basename(dirname(Sys.glob(file.path(.basePath, "outputs", paste0("nrvreport_", .ev("FS_NRVR_TAG", "phase5")),
                                                            "*", "DONE.txt"))))[1]
             else .split(.ev("FS_NRVR_ELFS"))
    if (!length(.ELFs) || anyNA(.ELFs) || !all(nzchar(.ELFs)))
      stop("FS_SET=nrvreport needs FS_NRVR_ELFS=<ELFs whose 5 NRV runs are DONE>, or FS_NRVR_MAP=TRUE once one ELF is reported")
    .phaseSetup <- list(queue = paste0("experiment_queue_nrvreport_", .ev("FS_NRVR_TAG", "phase5"), if (.map) "_map", "_",
                                       format(Sys.time(), "%Y%m%d-%H%M"), ".rds"),
                        onlyELFs = .ELFs,
                        reps = 1L,
                        n_workers = length(.ELFs), # one job per ELF
                        refit = FALSE,
                        predict = FALSE,
                        nrvReport = .ev("FS_NRVR_TAG", "phase5"),
                        nrvReportMap = .map)
  } else {
    stop("Set FS_SET=nrv, phase1, hindcast, refit, score, nrvreport or chain")
  }

  ####################
  # Build the queue rows
  ####################
  expt <- expand.grid(.ELFind = .ELFinds, .rep = .phaseSetup$reps, stringsAsFactors = FALSE)
  expt$.GCM <- "NRV"
  expt$.basePath <- .basePath
  if (.phaseSetup$refit) expt$.doFit <- TRUE
  if (nzchar(.phaseSetup$ledgerSuffix %||% "")) expt$.spreadFitFileSuffix <- .phaseSetup$ledgerSuffix
  if (!is.null(.phaseSetup$spreadLink)) expt$.spreadLink <- .phaseSetup$spreadLink
  if (!is.null(.phaseSetup$spreadLowerAsymptote)) expt$.spreadLowerAsymptote <- .phaseSetup$spreadLowerAsymptote
  if (!is.null(.phaseSetup$spreadRichardsNu)) expt$.spreadRichardsNu <- .phaseSetup$spreadRichardsNu
  if (!is.null(.phaseSetup$spreadWeighted)) expt$.spreadWeighted <- .phaseSetup$spreadWeighted
  if (!is.null(.phaseSetup$spreadGateRefusal)) expt$.spreadGateRefusal <- .phaseSetup$spreadGateRefusal
  if (!is.null(.phaseSetup$spreadReps)) expt$.spreadReps <- .phaseSetup$spreadReps
  if (!is.null(.phaseSetup$spreadCrpsWeights)) expt$.spreadCrpsWeights <- .phaseSetup$spreadCrpsWeights
  if (!is.null(.phaseSetup$spreadTests)) expt$.spreadTests <- .phaseSetup$spreadTests
  if (!is.null(.phaseSetup$spreadInitialPop)) expt$.spreadInitialPop <- .phaseSetup$spreadInitialPop
  if (!is.null(.phaseSetup$spreadInitialPopMethod)) expt$.spreadInitialPopMethod <- .phaseSetup$spreadInitialPopMethod
  if (!is.null(.phaseSetup$spreadSdBounds)) expt$.spreadSdBounds <- .phaseSetup$spreadSdBounds
  if (!is.null(.phaseSetup$spreadFireSdBounds)) expt$.spreadFireSdBounds <- .phaseSetup$spreadFireSdBounds
  if (!is.null(.phaseSetup$prepAreaMult)) expt$.prepAreaMult <- .phaseSetup$prepAreaMult
  if (!is.null(.phaseSetup$spreadSizeLikDf)) expt$.spreadSizeLikDf <- .phaseSetup$spreadSizeLikDf
  if (!is.null(.phaseSetup$wholeLandscape)) expt$.wholeLandscape <- as.logical(.phaseSetup$wholeLandscape)
  if (!is.null(.phaseSetup$spreadPowerLawShape)) expt$.spreadPowerLawShape <- .phaseSetup$spreadPowerLawShape
  if (!is.null(.phaseSetup$hcIgnitions)) expt$.hcIgnitions <- .phaseSetup$hcIgnitions
  if (isTRUE(.phaseSetup$hcAllSets)) expt$.hcAllSets <- TRUE
  if (!is.null(.phaseSetup$fireCauses)) expt$.fireCauses <- .phaseSetup$fireCauses
  ## Biomass_core cohort-reduction options (#131), e.g. FS_COHORT_REMOVE_SUPPRESSED=0.01,0.75 FS_COHORT_ESTABLISHMENT_LIMIT=20,5
  if (nzchar(.ev("FS_COHORT_REMOVE_SUPPRESSED"))) expt$.cohortRemoveSuppressed <- .ev("FS_COHORT_REMOVE_SUPPRESSED")
  if (nzchar(.ev("FS_COHORT_SQUASH"))) expt$.cohortSquash <- .ev("FS_COHORT_SQUASH")
  if (nzchar(.ev("FS_COHORT_ESTABLISHMENT_LIMIT"))) expt$.cohortEstablishmentLimit <- .ev("FS_COHORT_ESTABLISHMENT_LIMIT")
  ## the phase and the run's diagnostics are columns, so a worker reads them as global.R's defaultDots
  if (isTRUE(.phaseSetup$score)) {expt$.score <- TRUE; expt$.hcYears <- paste(.hcYears, collapse = ",")}
  if (!is.null(.phaseSetup$nrvReport)) expt$.nrvReport <- .phaseSetup$nrvReport
  if (isTRUE(.phaseSetup$nrvReportMap)) expt$.nrvReportMap <- TRUE
  expt$.phase1Only <- .phase1Only
  expt$.phase2Only <- !(.phase1Only || .phaseSetup$predict)
  .optionalColumn <- function(col, env, f = identity) if (nzchar(.ev(env))) expt[[col]] <<- f(.ev(env))
  .optionalColumn(".workerMemoryGB", "FS_WORKER_MEM_GB", as.numeric)
  .optionalColumn(".cacheChaining", "FS_NO_CACHECHAINING", function(x) !isTRUE(as.logical(x)))
  .optionalColumn(".fitQueue", "FS_FIT_QUEUE")
  .optionalColumn(".ledgerFolder", "FS_LEDGER_FOLDER") # experiments: the Drive testing folder for fit ledgers (production keeps the default)
  .optionalColumn(".deoptimWindow", "FS_DEOPTIM_WINDOW", as.integer)
  .optionalColumn(".deoptimMinGenerations", "FS_DEOPTIM_MIN_GENERATIONS", as.integer)
  ## NRV archive: a predict wave uploads into its own campaign folder, so it never overwrites the runs users read now
  if (identical(.set, "nrv") && isTRUE(.phaseSetup$predict) && !(nzchar(.ev("FS_ARCHIVE_DIR")) && nzchar(.ev("FS_ARCHIVE_LEDGER"))))
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
    if (nzchar(.ev("FS_HC_JOBS")))
      expt <- expt[paste(expt$.ELFind, expt$.hindcastYear, expt$.rep, sep = "/") %in% .split(.ev("FS_HC_JOBS")), , drop = FALSE]
    if (isTRUE(as.logical(.ev("FS_HC_INTERLEAVE", "FALSE"))))
      expt <- expt[order(ave(seq_len(NROW(expt)), expt$.ELFind, FUN = seq_along), match(expt$.ELFind, unique(expt$.ELFind))), , drop = FALSE]
  }
  ## Slurm: spread fits on the job's own node, not on .cores's machines; 40 at 9 GB each (the fireSenseUtils default;
  ## FS_WORKER_MEM_GB sets it)
  if (.slurm) expt$.cores <- 40L
  rownames(expt) <- NULL
  list(expt = expt, set = .set, label = .setLabel, trial = trial, phase1Only = .phase1Only, ps = .phaseSetup, ev = .ev,
       queue = .phaseSetup$queue, n_workers = .phaseSetup$n_workers,
       oneCore = identical(.set, "hindcast") || identical(.set, "score") || (identical(.set, "nrv") && .phaseSetup$predict))
}

## every queue column must be a name global.R uses (a defaultDots entry, a setupProject dot or a get0() name):
## a worker silently ignores a column the script never reads, e.g. a misspelled setting. .group, .after and
## .afterType are read by the launcher (SpaDES.project ?experimentGroups), not by global.R.
.checkColumns <- function(expt) {
  pd <- utils::getParseData(parse("global.R", keep.source = TRUE))
  .globalNames <- unique(gsub("^[\"']|[\"']$", "", pd$text[pd$token %in% c("SYMBOL", "SYMBOL_SUB", "STR_CONST")]))
  if (length(.unread <- setdiff(names(expt), c(.globalNames, ".group", ".after", ".afterType"))))
    stop("queue column(s) that global.R never reads: ", paste(.unread, collapse = ", "))
  message("Queueing ", NROW(expt), " jobs, fire years ", .fireYearStart, ":", .fireYearEnd)
}

####################
# Slurm (the GPSC): each worker is a one-node job; it takes this process's environment.
####################
if (.slurm) {
  ## compute nodes reach the internet only through the science proxy; workers and the cluster stay direct
  if (!nzchar(Sys.getenv("https_proxy")))
    Sys.setenv(https_proxy = "http://webproxy.science.gc.ca:8888", http_proxy = "http://webproxy.science.gc.ca:8888",
               no_proxy = "localhost,127.0.0.1,science.gc.ca")
  .slurmTmp <- file.path(.basePath, "scratch", "tmp") # TMPDIR of the jobs (see .sbatchCommon)
  dir.create(.slurmTmp, recursive = TRUE, showWarnings = FALSE)
  ## the account's allocation is on gpsc8 (64 CPUs, ~486 GB per standard node); its "low" QoS allows 0 CPUs.
  ## A compute node's /tmp is a 500 MB tmpfs: R's tempdir() there (reproducible.tempPath, where downloads and
  ## postProcess land first) cut every file at ~500 MB (SCANFI landcover, crop "too few values", 2026-10-29).
  ## kill_on_invalid_dep: a job whose dependency can never be met (its fit failed) is cancelled, not left pending
  .sbatchCommon <- list(clusters = "gpsc8", account = "nrcan_forsite", partition = "standard",
                        qos = "nrcan__standard_gpsc8_allocation", nodes = 1, ntasks = 1,
                        export = paste0("ALL,TMPDIR=", .slurmTmp), kill_on_invalid_dep = "yes")
  ## a step's own options. `mem` (FS_SBATCH_MEM for one FS_SET) replaces the default memory; "" keeps it
  .sbatchStep <- function(s, mem = s$ev("FS_SBATCH_MEM")) {
    .mem <- function(default) if (nzchar(mem)) mem else default
    if (s$phase1Only) list(cpus_per_task = 1, mem = "96G", time = "1-00:00:00") # 32G was OOM-killed (12.1_2, prepSpreadFitData); to be set from measured peaks
    else if (identical(s$set, "nrvreport") && isTRUE(s$ps$nrvReportMap)) # the ELF map only
      list(cpus_per_task = 1, mem = "4G", time = "01:00:00") # 2.5 min, MaxRSS 1.7 GB
    else if (identical(s$set, "nrvreport")) # one ELF's 5 runs; the landscape metrics and figures use the CPUs
      list(cpus_per_task = 4, mem = .mem("24G"), time = "1-00:00:00") # 6.4: 42 min, MaxRSS 13 GB
    else if (identical(s$set, "score")) # the scoring job reads 12 ELFs at once (fireSimulator_validation nCores); ~8 GB each
      list(cpus_per_task = 12, mem = .mem("160G"), time = "2-00:00:00")
    else if (s$oneCore) # one core per ELF x year (hindcast) or ELF x set (NRV) job; FS_SBATCH_MEM until the MaxRSS of the first jobs is known
      list(cpus_per_task = 1, mem = .mem("64G"), time = if (identical(s$set, "nrv")) "7-00:00:00" else "2-00:00:00")
    else list(cpus_per_task = 42, # 40 workers + 2 kept free; 7.1 GB/CPU is the partition's limit
              ## per ELF (FS_SBATCH_MEM, or FS_FIT_MEM in a chain): 40 x the worker peak clusters records in
              ## fitMemory.rds, / 0.9 headroom, + the master. 13.1 and 6.6.1 need ~1-2 GB per worker (294G); 12.1_2 needs 8.5 (420G)
              mem = .mem("294G"),
              time = "7-00:00:00") # required by gpsc8, though the partition has no limit
  }
  .logDir <- function(queue_path) file.path("logs", sub("\\.rds$", "", basename(queue_path)))
}

## FS_SET=chain: a whole campaign in one call (Slurm: experimentSBATCH(); elsewhere tmux panes, FS_CHAIN_WORKERS), one queue experiment_queue_chain_<FS_CHAIN_SUFFIX
## or today>.rds. For each trial (FS_TRIAL, comma-separated) x ELF (FS_RF_ELFS), tag <trial>_<ELF><FS_CHAIN_SUFFIX>, three
## queue groups (SpaDES.project ?experimentGroups): fit_<tag> (the refit row), hc_<tag> (its ELF x year rows, after the
## fit succeeded) and score_<tag> (after every hindcast job ended, afterany). Memory per ELF: FS_FIT_MEM="6.6.2=320G,14.3=200G"
## (the fit job) and FS_FIT_WORKER_GB="6.6.2=6.1,14.3=3.5" (per DEoptim worker); an ELF left out takes the defaults.
## FS_SCORE_MEM (default 32G): each score job, one ELF.
##   FS_SET=chain FS_TRIAL=wd03,wd04 FS_RF_ELFS=6.6.2,14.3 FS_FIT_MEM=... FS_FIT_WORKER_GB=... [FS_CHAIN_SUFFIX=_e] Rscript expt.R
## FS_DRY_RUN=TRUE writes the queue and the job scripts without submitting; delete both (queue .rds, logs/<queue>/) after.
## Without Slurm it only prints the rows per step and the pane count.
## Worker panes on this machine (no Slurm): one queue, these panes. Worker panes take the tmux session's environment,
## not this process's.
.runTmux <- function(df, n_workers, queue_path) {
  if (!nzchar(Sys.getenv("TMUX"))) stop("Run expt.R inside tmux")
  SpaDES.project::experimentTmux(
    df                  = df,
    devPackages         = .devPackages,
    global_path         = "global.R",
    n_workers           = n_workers,
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
}

if (identical(Sys.getenv("FS_SET"), "chain")) {
  .trialsRun <- .split(Sys.getenv("FS_TRIAL")); .elfsRun <- .split(Sys.getenv("FS_RF_ELFS"))
  if (!length(.trialsRun) || !all(.trialsRun %in% names(.trials))) stop("FS_SET=chain needs FS_TRIAL=<trials in .trials, comma-separated>")
  if (!length(.elfsRun)) stop("FS_SET=chain needs FS_RF_ELFS")
  .perELF <- function(env) { kv <- strsplit(.split(Sys.getenv(env, "")), "="); setNames(vapply(kv, `[`, "", 2L), vapply(kv, `[`, "", 1L)) }
  .fitMem <- .perELF("FS_FIT_MEM"); .fitWorkerGB <- .perELF("FS_FIT_WORKER_GB")
  .suffix <- Sys.getenv("FS_CHAIN_SUFFIX")
  .rows <- list(); .workers <- integer(); .opts <- list()
  for (.tr in .trialsRun) for (.e in .elfsRun) {
    .g <- paste0(c("fit_", "hc_", "score_"), .tr, "_", gsub("\\.", "_", .e), .suffix)
    .steps <- list(.step("refit", .tr, c(FS_RF_ELFS = .e, if (!is.na(.fitWorkerGB[.e])) c(FS_WORKER_MEM_GB = .fitWorkerGB[[.e]]))),
                   .step("hindcast", .tr, c(FS_HC_ELFS = .e)),
                   .step("score", .tr, c(FS_HC_ELFS = .e)))
    .mems <- c(if (!is.na(.fitMem[.e])) .fitMem[[.e]] else "", "", Sys.getenv("FS_SCORE_MEM", "32G"))
    for (.k in 1:3) {
      .x <- .steps[[.k]]$expt
      .x$.group <- .g[.k]
      .x$.after <- c("", .g[1:2])[.k]
      .x$.afterType <- c("afterok", "afterok", "afterany")[.k]
      .rows[[length(.rows) + 1L]] <- .x
      .workers[.g[.k]] <- .steps[[.k]]$n_workers
      if (.slurm) .opts[[.g[.k]]] <- .sbatchStep(.steps[[.k]], mem = .mems[.k])
    }
  }
  ## columns another step does not use are NA: global.R reads NA as "not set"
  .expt <- as.data.frame(data.table::rbindlist(.rows, fill = TRUE))
  .checkColumns(.expt)
  .queue <- paste0("experiment_queue_chain_", if (nzchar(.suffix)) sub("^_", "", .suffix) else format(Sys.Date(), "%Y-%m-%d"), ".rds")
  message("FS_SET=chain: queue ", .queue, ", ", length(.workers), " groups")
  if (!.slurm) {
    ## tmux: one pool of panes for every group (a pane claims a row once the groups in its .after have finished).
    ## FS_CHAIN_WORKERS panes, default one per fit plus FS_HC_WORKERS (12) for the hindcast and score rows.
    .nPanes <- as.integer(Sys.getenv("FS_CHAIN_WORKERS", sum(grepl("^fit_", .expt$.group)) + as.integer(Sys.getenv("FS_HC_WORKERS", "12"))))
    if (.dryRun) {
      print(table(sub("_.*", "", .expt$.group))); message("FS_DRY_RUN: ", nrow(.expt), " rows, ", .nPanes, " panes; nothing launched")
    } else .runTmux(.expt, .nPanes, .queue)
  } else SpaDES.project::experimentSBATCH(
    df          = .expt,
    devPackages = .devPackages, # one dev library for every group
    global_path = "global.R",
    n_workers   = .workers,
    queue_path  = .queue,
    log_dir     = .logDir(.queue),
    sbatch_opts = c(list(.sbatchCommon), .opts),
    ss_id       = NULL, # compute nodes have no internet: the local queue only
    dry_run     = .dryRun)
} else {
  .s <- .step(Sys.getenv("FS_SET"))
  message("FS_SET=", .s$label, ": queue ", .s$queue)
  expt <- .s$expt; queue_path <- .s$queue
  .checkColumns(expt)

  ####################
  # Run. Worker panes take the tmux session's environment, not this process's.
  ####################
  if (.slurm) {
    .submit <- function(df, queue_path, after) SpaDES.project::experimentSBATCH(
      df          = df,
      devPackages = .devPackages, # nothing changed since the last launch: its dev library
      global_path = "global.R",
      n_workers   = .s$n_workers,
      queue_path  = queue_path,
      log_dir     = .logDir(queue_path),
      ## FS_SBATCH_AFTER=<job ids> starts these jobs only after those succeed (a fit after its phase 1);
      ## FS_HC_AFTER_FITS=TRUE: each ELF's hindcast after its own fit job, if one is queued or running (found by its
      ## log folder; none: it starts now and stops if the ledger has no fit)
      sbatch_opts = c(.sbatchCommon, .sbatchStep(.s),
                      if (length(after)) list(dependency = paste0(if (identical(.s$set, "score")) "afterany:" else "afterok:", paste(after, collapse = ":")))),
      ss_id       = NULL, # compute nodes have no internet: the local queue only
      dry_run     = .dryRun
    )

    .after <- .split(.s$ev("FS_SBATCH_AFTER"))
    ## FS_SCORE_NOW=TRUE: score what has finished now, without waiting for the jobs still queued (rerun later for the rest)
    if (identical(.s$set, "score") && !isTRUE(as.logical(.s$ev("FS_SCORE_NOW", "FALSE")))) # every job of the hindcast being scored
      .after <- c(.after, vapply(Filter(function(j) length(j) == 3 && grepl(paste0("experiment_queue_hindcast_", .s$ev("FS_HC_TAG"), "(_|/)"), j[3]),
                                        strsplit(system2("squeue", c("-M", "gpsc8", "-u", Sys.info()[["user"]], "-h", "-o", "'%i|%T|%o'"), stdout = TRUE), "\\|")),
                                 `[`, "", 1))
    if (.s$oneCore && !identical(.s$set, "score") && isTRUE(as.logical(.s$ev("FS_HC_AFTER_FITS", "FALSE")))) {
      .jobs <- strsplit(system2("squeue", c("-M", "gpsc8", "-u", Sys.info()[["user"]], "-h", "-o", "'%i|%T|%o'"), stdout = TRUE), "\\|")
      .jobs <- Filter(function(j) length(j) == 3 && j[2] %in% c("RUNNING", "PENDING"), .jobs)
      for (.elf in unique(expt$.ELFind)) {
        ## the fit job of this ELF: queue refit_<trial>_<ELF>
        .rfQueue <- paste0(if (nzchar(.s$trial)) .s$trial else "pile[A-Za-z0-9]*", "_", gsub("\\.", "_", .elf))
        .fit <- vapply(Filter(function(j) grepl(paste0("experiment_queue_refit_", .rfQueue, "/"), j[3]), .jobs), `[`, "", 1)
        .submit(expt[expt$.ELFind == .elf, , drop = FALSE], sub("\\.rds$", paste0("_", .elf, ".rds"), queue_path), c(.after, .fit))
      }
    } else .submit(expt, queue_path, .after)
  } else {
    workers <- .runTmux(expt, .s$n_workers, queue_path)
  }
}
