## Launches a FireSense campaign: installs packages once, builds the job queue, starts the tmux workers.
## Each worker sources global_draft.R per job. Run inside tmux:
##   FS_SET=nrv FS_NRV_WAVE=<fit|predict|ignition> [FS_NRV_ELFS=4.1,14.4|all] Rscript expt.R
##   FS_SET=hindcast FS_HC_ELFS=4.1,14.4 [FS_HC_LEDGER=_boundfix_rep81] Rscript expt.R
##   FS_SET=boundfix [FS_BF_ELFS=6.6.1,9.1.2] Rscript expt.R

repos <- c("https://predictiveecology.r-universe.dev", getOption("repos"))
source("https://raw.githubusercontent.com/PredictiveEcology/pemisc/refs/heads/development/R/getOrUpdatePkg.R")
getOrUpdatePkg(c("Require", "remotes"), c("1.0.1.9013", "0.0.0"))

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

outs <- SpaDES.project::preRunSetupProject(file = "global_draft.R", upTo = "params")
options(spades.useRequire = FALSE) # nothing installs from here on

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
  .phaseSetup <- list(queue = paste0("experiment_queue_nrv_", .tag, "_wave", .wave, if (.phase1Only) "_phase1", ".rds"),
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
                      n_workers = as.integer(Sys.getenv("FS_HC_WORKERS", "12")),
                      refit = FALSE,
                      predict = TRUE)
} else if (identical(.set, "boundfix")) {
  ## refit the ELFs whose spread fits sat on the old maxAsymptote bound, fit only, into their own ledger
  ## (_boundfix_rep81); hindcast them after with FS_SET=hindcast FS_HC_LEDGER=_boundfix_rep81
  if (.phase1Only) stop("FS_SET=boundfix has no phase-1 form")
  .phaseSetup <- list(queue = paste0("experiment_queue_boundfix_", Sys.getenv("FS_BF_TAG", format(Sys.Date(), "%Y-%m-%d")), ".rds"),
                      onlyELFs = .split(Sys.getenv("FS_BF_ELFS", "6.6.1,9.1.2,4.2.1,6.6.2")),
                      reps = 81L,
                      ledgerSuffix = "_boundfix_rep81",
                      n_workers = length(.split(Sys.getenv("FS_BF_ELFS", "6.6.1,9.1.2,4.2.1,6.6.2"))), # each fit is a cluster
                      refit = TRUE,
                      predict = FALSE)
} else {
  stop("Set FS_SET=nrv, hindcast or boundfix")
}
message("FS_SET=", .set, ": queue ", .phaseSetup$queue)

####################
# Build the queue
####################
queue_path <- .phaseSetup$queue
outs$params$fireSense_ELFs$queue_path <- queue_path
.ELFinds <- fireSenseUtils::runELFs(outs, whatOut = "allNames")
.ELFsFitted <- fireSenseUtils::runELFs(outs, whatOut = "fittedNamesOnly")

expt <- expand.grid(.ELFind = .ELFinds, .rep = .phaseSetup$reps, stringsAsFactors = FALSE)
expt$.GCM <- "NRV"
if (.phaseSetup$refit) expt$.doFit <- TRUE
if (nzchar(.phaseSetup$ledgerSuffix %||% "")) expt$.spreadFitFileSuffix <- .phaseSetup$ledgerSuffix

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
####################
if (!nzchar(Sys.getenv("TMUX"))) stop("Run expt.R inside tmux")
system2("tmux", c("set-environment", "FS_PHASE1_ONLY", if (.phase1Only) "TRUE" else "FALSE"))
system2("tmux", c("set-environment", "FS_PHASE2_ONLY", if (.phase1Only || .phaseSetup$predict) "FALSE" else "TRUE"))

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
