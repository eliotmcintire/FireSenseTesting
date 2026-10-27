## Launches a FireSense campaign: installs packages once, builds the job queue, starts the tmux workers.
## Each worker sources global_draft.R per job. Run inside tmux:
##   FS_SET=nrv FS_NRV_WAVE=<fit|predict|ignition> [FS_NRV_ELFS=4.1,14.4|all] Rscript expt.R
##   FS_SET=fit [FS_NRV_ELFS=...|all] [FS_AFTER_QUEUE=<phase-1 queue>] Rscript expt.R   (fit every ELF: phases 1 + 2)
##   FS_SET=phase1 [FS_NRV_ELFS=...|all] [FS_FIT_QUEUE=<fit queue>] Rscript expt.R   (inputs and caches only, ahead of the fits:
##     same order as the fits; skips any ELF the fit queue has claimed)
##   FS_SET=hindcast FS_HC_ELFS=4.1,14.4 [FS_HC_LEDGER=_boundfix_rep81] [FS_HC_IGNITIONS=observedEscapes] Rscript expt.R
##   FS_SET=refit FS_RF_ELFS=6.6.1,9.1.2 Rscript expt.R   (same rep and ledger as production, so caches are shared)

## inputs, outputs, cache, scratch and shared data: overrides global_draft.R's .basePath here (preRunSetupProject)
## and, as a queue column, in the workers. GPSC (user elm001): no /mnt/fast; GPFS, which the compute nodes see
.basePath <- if (Sys.info()[["user"]] == "elm001") "/gpfs/fs7/nrcan/nrcan_forsite/elm001/firesense/fast" else "/mnt/fast"
## no CRAN mirror set (GPSC): install.packages() stops when not interactive. Same repos as the local R; the GPSC is
## Ubuntu 24.04 (noble) too, so the P3M binaries fit its system GDAL
if ("@CRAN@" %in% getOption("repos"))
  options(repos = c(P3M = "https://packagemanager.posit.co/cran/__linux__/noble/latest", CRAN = "https://cloud.r-project.org"))

suppressWarnings(rm(.ELFind)) # a leftover from an earlier run in this session would override global_draft.R's default

####################
# Install packages once, before any worker exists; workers only read the library.
# FS_NO_INSTALL=TRUE skips this, for a launch beside running jobs.
# @modsForFireSense branches = development + our open PRs (R/updateModsForFireSense.sh).
####################
if (!isTRUE(as.logical(Sys.getenv("FS_NO_INSTALL", "FALSE")))) {
  if (!require("pak")) install.packages("pak")
  pak::pak(c("PredictiveEcology/Require@development",
             "PredictiveEcology/SpaDES.project@development",
             "PredictiveEcology/reproducible@development",
             "PredictiveEcology/SpaDES.core@development",
             "PredictiveEcology/SpaDES.tools@development",
             "PredictiveEcology/LandR@modsForFireSense",
             "PredictiveEcology/clusters@development",
             "PredictiveEcology/filelock@main",
             "PredictiveEcology/climateData@modsForFireSense",
             "PredictiveEcology/quickPlot@development",
             "googlesheets4"), ask = FALSE) # googlesheets4: the sheet-backed queue (a Suggests of SpaDES.project)
  # no dependencies: its Remotes say LandR@development, which conflicts with LandR@modsForFireSense
  pak::pkg_install("PredictiveEcology/fireSenseUtils@development", dependencies = FALSE, ask = FALSE)
}

options(spades.useRequire = FALSE) # nothing installs from here on: pak above did it, so setupProject() must not run Require
outs <- SpaDES.project::preRunSetupProject(file = "global_draft.R", upTo = "params")

####################
# Fitting years: the end is the last year of historical climate. Workers take the window from
# global_draft.R's defaultDots, so stop if that has drifted.
####################
.fireYearStart <- 1986L # vegetation strictly before the fire year: SCANFI 1985 feeds 1986
.fireYearEnd <- as.integer(climateData::latestHistoricalYear())
.globalYears <- as.integer(regmatches(.l <- grep("^\\s*\\.fireYears = c\\(", readLines("global_draft.R"), value = TRUE)[1],
                                      gregexpr("[0-9]{4}", .l))[[1]])
if (!identical(.globalYears, c(.fireYearStart, .fireYearEnd)))
  stop("global_draft.R's .fireYears do not match ", .fireYearStart, ":", .fireYearEnd, "; edit global_draft.R")

####################
# Campaign settings. A queue name is resumed, never extended: relaunching changed settings needs a new tag.
####################
.split <- function(x) strsplit(x, ",")[[1]]
.phase1Only <- isTRUE(as.logical(Sys.getenv("FS_PHASE1_ONLY", "FALSE")))
.set <- Sys.getenv("FS_SET")
## FS_SET=fit and FS_SET=phase1: the nrv fit wave (phases 1 + 2) and its phase 1 alone, under their own labels
.setLabel <- .set
if (.set %in% c("fit", "phase1")) {
  .phase1Only <- identical(.set, "phase1"); .set <- "nrv"; Sys.setenv(FS_NRV_WAVE = "fit")
}

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
                      reps = as.integer(.split(Sys.getenv("FS_HC_SETS", "1,2,3,4,5"))),
                      ## Y+1: SCANFI year Y may already show year Y's burns, so year Y+1 uses SCANFI Y (pre-burn vegetation)
                      hindcastYears = as.integer(.split(Sys.getenv("FS_HC_YEARS", "1986,1991,1996,2001,2006,2011,2016,2021"))),
                      ledgerSuffix = Sys.getenv("FS_HC_LEDGER"), # "" = production ledger
                      hcIgnitions = if (nzchar(Sys.getenv("FS_HC_IGNITIONS"))) Sys.getenv("FS_HC_IGNITIONS"), # unset: drawn ("predicted"); "observedEscapes": spread the observed escapes
                      n_workers = as.integer(Sys.getenv("FS_HC_WORKERS", "12")),
                      refit = FALSE,
                      predict = TRUE)
} else if (identical(.set, "refit")) {
  ## refit the named ELFs, fit only. Same rep (1) and ledger suffix ("") as production by default: a separate rep or
  ## suffix puts different labels in the cache keys and redoes data preparation (2026-10-19)
  if (.phase1Only) stop("FS_SET=refit has no phase-1 form")
  if (!nzchar(Sys.getenv("FS_RF_ELFS"))) stop("FS_SET=refit needs FS_RF_ELFS")
  .phaseSetup <- list(queue = paste0("experiment_queue_refit_", Sys.getenv("FS_RF_TAG", format(Sys.Date(), "%Y-%m-%d")), ".rds"),
                      onlyELFs = .split(Sys.getenv("FS_RF_ELFS")),
                      reps = as.integer(Sys.getenv("FS_RF_REP", "1")),
                      ledgerSuffix = Sys.getenv("FS_RF_SUFFIX", ""),
                      spreadLink = if (nzchar(Sys.getenv("FS_SPREAD_LINK"))) Sys.getenv("FS_SPREAD_LINK"), # unset: the module default
                      spreadWeighted = if (nzchar(Sys.getenv("FS_SPREAD_WEIGHTED"))) Sys.getenv("FS_SPREAD_WEIGHTED"), # unset: the module default (FALSE)
                      spreadTests = if (nzchar(Sys.getenv("FS_SPREAD_TESTS"))) Sys.getenv("FS_SPREAD_TESTS"), # unset: the module default DEoptimTests
                      spreadInitialPop = if (nzchar(Sys.getenv("FS_SPREAD_INITIALPOP"))) Sys.getenv("FS_SPREAD_INITIALPOP"), # unset: the module default start
                      n_workers = length(.split(Sys.getenv("FS_RF_ELFS"))), # each fit is a cluster
                      refit = TRUE,
                      predict = FALSE)
} else {
  stop("Set FS_SET=nrv, phase1, hindcast or refit")
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
if (!is.null(.phaseSetup$spreadTests)) expt$.spreadTests <- .phaseSetup$spreadTests
if (!is.null(.phaseSetup$spreadInitialPop)) expt$.spreadInitialPop <- .phaseSetup$spreadInitialPop
if (!is.null(.phaseSetup$hcIgnitions)) expt$.hcIgnitions <- .phaseSetup$hcIgnitions

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
message("Queueing ", NROW(expt), " jobs, fire years ", .fireYearStart, ":", .fireYearEnd)

####################
# Run. Worker panes take the tmux session's environment, not this process's.
# With Slurm (the GPSC), each worker is a one-node job; it takes this process's environment.
####################
if (nzchar(Sys.which("sbatch"))) {
  Sys.setenv(FS_PHASE1_ONLY = .phase1Only, FS_PHASE2_ONLY = !(.phase1Only || .phaseSetup$predict))
  expt$.cores <- 64L # spread fits on the job's own node, not on .cores's machines
  workers <- SpaDES.project::experimentSBATCH(
    df          = expt,
    global_path = "global_draft.R",
    n_workers   = .phaseSetup$n_workers,
    queue_path  = queue_path,
    log_dir     = file.path("logs", sub("\\.rds$", "", basename(queue_path))),
    ## the account's allocation is on gpsc8 (64 CPUs, ~486 GB per standard node); its "low" QoS allows 0 CPUs
    sbatch_opts = list(clusters = "gpsc8", account = "nrcan_forsite", partition = "standard",
                       qos = "nrcan__standard_gpsc8_allocation", nodes = 1, ntasks = 1, cpus_per_task = 64, mem = 0),
    ss_id       = NULL # compute nodes have no internet: the local queue only
  )
  q(save = "no")
}
if (!nzchar(Sys.getenv("TMUX"))) stop("Run expt.R inside tmux")
system2("tmux", c("set-environment", "FS_PHASE1_ONLY", if (.phase1Only) "TRUE" else "FALSE"))
system2("tmux", c("set-environment", "FS_PHASE2_ONLY", if (.phase1Only || .phaseSetup$predict) "FALSE" else "TRUE"))
## workers read these in global_draft.R; tmux panes do not inherit the launcher's environment
for (.v in c("FS_NO_CACHECHAINING", "FS_RPROF", "FS_DIAG_FILE", "FS_AFTER_QUEUE", "FS_FIT_QUEUE"))
  system2("tmux", if (nzchar(Sys.getenv(.v))) c("set-environment", .v, Sys.getenv(.v)) else c("set-environment", "-u", .v))

workers <- SpaDES.project::experimentTmux(
  df                  = expt,
  global_path         = "global_draft.R",
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
