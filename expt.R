repos <- c("https://predictiveecology.r-universe.dev", getOption("repos"))
source("https://raw.githubusercontent.com/PredictiveEcology/pemisc/refs/heads/development/R/getOrUpdatePkg.R")
getOrUpdatePkg(c("Require", "remotes"), c("1.0.1.9013", "0.0.0")) # only install/update if required
# remotes::install_github("PredictiveEcology/SpaDES.project@cacheRequire")


suppressWarnings(rm(.ELFind)) # This is a precaution as this may exist if there is a failure below; and this is rerun

####################
# Install packages -- ONCE, here, before any worker exists
####################
# This used to live at the top of global.R, which every worker sources at the start of
# every job: 13 processes writing one shared library. On 2026-09-09 a worker rewrote
# SpaDES.tools at 10:21:51 and four siblings died with "lazy-load database ... is corrupt".
if (!require("pak")) install.packages("pak")
 pak::pak(c("PredictiveEcology/Require@development",
            "PredictiveEcology/SpaDES.project@development",
            # reproducible PRs #601 (no restore into a foreign temp dir) and #602 (useCache = FALSE
            # bypass) merged 2026-09-14; back on development.
            "PredictiveEcology/reproducible@development",
            # SpaDES.core PRs #449, #450, #451, #452 merged 2026-09-14; back on development.
            "PredictiveEcology/SpaDES.core@development",
            # fireSenseUtils carries the objective function; the `packages` list below
            # pins it with a floor (>= 0.2.0), so once any 0.2.x was installed it never
            # moved again. That is why the fits ran without the objFunSpread adTest fix
            # from 2026-09-07. Track development here instead.
            "PredictiveEcology/fireSenseUtils@development",
            # SpaDES.tools carries the spread() hot path. It is otherwise pulled in
            # through setupProject's `packages` list with a version FLOOR, which never
            # moves once any satisfying version is installed -- the same reason
            # fireSenseUtils sat on a stale build. Track development here instead.
            "PredictiveEcology/SpaDES.tools@development",
            # Same floor-only gap as the two above: LandR, clusters, climateData and
            # quickPlot all arrive through setupProject's `packages` list with a `>=`
            # floor, so a satisfying build already on the machine is never replaced and
            # merged fixes never reach a worker. LandR in particular carries the
            # terraOptions restore (#213) and the SCANFI drive fallback; clusters carries
            # the PSOCK host handling this run depends on. clusters has no development
            # branch, so track main.
            "PredictiveEcology/LandR@development",
            "PredictiveEcology/clusters@main",
            # PINNED to a feature branch, not development: PR #24 adds the years argument to
            # climateLayers() plus latestHistoricalYear(), and the 2023/2024 tile-index rows
            # this campaign's end year depends on. Tracking @development here would silently
            # overwrite it on the next launch and drop the end year back to 2022.
            # Move back to @development once PR #24 merges.
            "PredictiveEcology/climateData@fireSense/combined-fixes",
            "PredictiveEcology/quickPlot@development"), ask = FALSE)

####################
# pre RUN the global.R setupProject
####################
# preRunSetupProject() evaluates everything in global.R above its setupProject() call and
# then setupProject() itself up to `params`, so this is also where the modules' and the
# `packages` list's dependencies get resolved -- once, in this process.

outs <- SpaDES.project::preRunSetupProject(file = "global.R", upTo = "params")

# From here on nothing may install. Workers source global.R in their own sessions, so this
# has to be off in global.R's own options() block to reach them -- it is; this line covers
# the rest of THIS session.
options(spades.useRequire = FALSE)

####################
# RESOLVE THE FITTING YEARS -- ONCE, here, for the whole campaign
####################
# Each of the 15 workers sources global.R itself, so a "latest year" looked up per worker
# could differ between ELFs if a data release landed mid-campaign. Resolve it once here and
# pin it onto every row of `expt`, so every ELF is fit on the identical window.
#
# The end year must be the MINIMUM over inputs (latest-fire-year.md sec 2): spread fitting
# needs NBAC polygons AND climate for every year; ignition fitting needs NFDB points AND
# climate. A year missing from the fire data is silently kept as a no-fire year, which is
# a wrong fit with no error -- hence the minimum, not the maximum.
#
# Climate binds today: latestHistoricalYear() is 2024 (the tile index now carries the
# 2023/2024 rows), NBAC reaches 2025, the local NFDB copy 2024. fireregimetools has no
# latest_fire_year() yet, so the fire side is asserted below rather than queried.
#
# These are NOT pushed to the workers as queue columns. Each worker's global.R takes the
# window from its own `defaultDots`; passing it as a dot as well produced a self-reference
# (`.fireYearStart = .fireYearStart`) that resolved to NA in every worker on 2026-09-11.
# The check below is what keeps the two in step: if the resolved end year ever moves away
# from what global.R defaults to, this stops rather than fitting the wrong window.
.fireYearStart <- 1985L                                  # first SCANFI V2 year
.fireYearEnd <- as.integer(climateData::latestHistoricalYear())
stopifnot(is.finite(.fireYearEnd), .fireYearEnd > .fireYearStart)

.globalTxt <- readLines("global.R")
.globalStart <- as.integer(sub(".*\\.fireYearStart = ([0-9]+)L.*", "\\1",
                               grep("^\\s*\\.fireYearStart = [0-9]+L,", .globalTxt, value = TRUE)[1]))
.globalEnd <- as.integer(sub(".*\\.fireYearEnd = ([0-9]+)L.*", "\\1",
                             grep("^\\s*\\.fireYearEnd = [0-9]+L,", .globalTxt, value = TRUE)[1]))
if (!identical(.globalStart, .fireYearStart) || !identical(.globalEnd, .fireYearEnd))
  stop("global.R defaultDots has fire years ", .globalStart, ":", .globalEnd,
       " but this campaign resolved ", .fireYearStart, ":", .fireYearEnd,
       ".\nEdit global.R's .fireYearStart/.fireYearEnd to match -- the workers read the window",
       " from there, not from the queue.")
message("Fitting fire years ", .fireYearStart, ":", .fireYearEnd)

####################
# RUN fireSense_ELFs to get the ELF map
####################

# Get all ELFs.
# NB: a NEW queue file. experimentTmux only materializes the queue from `expt`
# when queue_path does not yet exist (tmux.R ~L855); pointing at the old
# experiment_queue_predict5.rds would silently reuse its March 2026 rows --
# including 6 rows still marked RUNNING -- and ignore `expt` entirely.
# 2026-09-11: the 2026-09-10 queue is FINISHED (54 DONE, 9 QUARANTINED) and was fit on
# fire years 2002-2022, so reusing its name would queue nothing at all.
# 2026-09-13: the 2026-09-12b queue FINISHED (54/54 DONE) but with module-internal caching on; this
# pass reruns phase 1 from a cleared cache with options(spades.useCache = "eventsOnly").
# Per-phase launch settings. FS_PHASE is the same switch global.R reads for the events
# barrier (1 caches, 2 fit, 3 predict). Each later phase queues only the ELFs the previous
# phase's queue finished (`from`), so a phase can start while the one before still runs.
# A queue name is resumed, never extended: to add ELFs that finish later, use a new name.
.phase <- as.integer(Sys.getenv("FS_PHASE", unset = "1"))
.phaseSetup <- list(
  ## 2026-09-15 (Eliot): rebuild, at a slower pace alongside phase 2, the event caches that the 2026-09-14 module
  ## updates invalidated, for the ELFs phase 2 has still to fit. `reverse` starts from the end of phase 2's order, so
  ## the two campaigns rarely work on the same ELF; `skipDoneIn` leaves out what phase 2 already finished.
  ## The all-ELF phase-1 queue waits for fireSense_ELFs #14:
  ##   list(queue = "experiment_queue_fits_2026-09-14.rds", n_workers = 15, from = NULL)  # every ELF in the map
  ## 2026-09-16: the 2026-09-15 cache queue is retired. Its remaining rows were all ELFs that phase 2 is
  ## fitting right now, so every worker that freed up claimed one, hit the collision guard and was closed --
  ## the campaign consumed itself without advancing (36 of 53 done). A queue is resumed, never extended, so
  ## this is a new name holding only ELFs no fit will reach for many hours: 5.3.2 is absent from the fit
  ## queue entirely, and the rest sit at fit-queue positions 37-43. 6.2.3 and 6.3.1 are DELIBERATELY left
  ## out -- they are at fit positions 4 and 5 and would be claimed within the hour.
  ## Phase 1 still runs estimateThreshold, which now caches a DETERMINISTIC threshold (.elfSeed), and NP is
  ## not part of that key -- so this warms exactly what the later fits will look up.
  ## ...-16.rds was created at 14:04 from a whitelist that still contained 5.3.2, and a queue lives in the
  ## GOOGLE SHEET -- deleting the local .rds mirror and relaunching just RESUMED that sheet, stale list and
  ## all, so the collision guard had to close a worker twice. A queue is resumed, never edited: use a new name.
  list(queue = "experiment_queue_caches_2026-09-16b.rds",  n_workers = 3,
       from = "experiment_queue_fits_2026-09-13.rds",
       ## 5.3.2 was dropped from this list at 14:06: it was safe when computed against SIX live fits, but
       ## raising phase 2 to TEN workers advanced the fit queue and a fit claimed it minutes later. Recompute
       ## a whitelist AFTER the fits have claimed, never before.
       onlyELFs = c("5.1.1", "5.1.2", "5.2.2", "5.4", "11.1", "11.2", "3.1.1")),
  ## 2026-09-14: the first phase-2 queue fits the 54 ELFs that finished the phase-1 rerun; ELFs from the
  ## all-ELF phase-1 queue above need a later phase-2 queue name (a queue is resumed, never extended)
  ## 2026-09-16: NP is the cluster size (clusters:::.clusterNP), so nCoresNeeded = 60 in global.R makes each
  ## fit a 60-worker cluster. 10 fits x 60 = 600 of 704 cores, and the allocator's proportional split then
  ## puts 45 workers on each 48-core host, 14 on each 16-core host and 45 (incl. 10 masters) on mega's 80 --
  ## all at or under capacity. 11 fits would put 50 on the 48-core hosts, so 10 is the ceiling here.
  list(queue = "experiment_queue_fit_2026-09-14.rds",     n_workers = 10,  # each fit is a 60-worker cluster
       from = "experiment_queue_fits_2026-09-13.rds"),
  list(queue = "experiment_queue_predict_2026-09-14.rds", n_workers = 5,
       from = "experiment_queue_fit_2026-09-14.rds")
)[[.phase]]
queue_path <- .phaseSetup$queue
outs$params$fireSense_ELFs$queue_path <- queue_path
.ELFinds <- fireSenseUtils::runELFs(outs, whatOut = "allNames")
# Already-fitted ELFs, straight from the shared cloud ledger that fireSense_SpreadFit
# writes to (`fireSenseParams_*.rds`). This is the same list fireSense_dataPrepFit uses
# to decide whether to skip a fit, so deriving the queue from it means re-running this
# script can never re-fit something that is already done.
.ELFsFitted <- fireSenseUtils::runELFs(outs, whatOut = "fittedNamesOnly")

####################
# SET UP EXPERIMENT
####################

.reps <- 1
expt <- expand.grid(.ELFind = .ELFinds, .rep = .reps, stringsAsFactors = FALSE)
if (exists(".modules"))
  expt <- cbind(expt, .modules = I(lapply(seq_len(NROW(expt)), function(x) .modules)))
if (exists(".times"))
  expt <- cbind(expt, .times = I(lapply(seq_len(NROW(expt)), function(x) .times)))

# Only fit what the ledger says is missing
expt <- expt[!expt$.ELFind %in% .ELFsFitted, ]

# These errored in earlier attempts. They are no longer excluded -- the causes may have
# been fixed since -- but they are sorted to the back so the well-behaved ELFs get the
# cluster first and any that still fail do so after the bulk of the work is banked.
problematic <- c("5.1.1", "5.1.2", "5.1.3" # something in climate, missing in future tile 39; only has 2011,12
            , "3.1.1" # 
            , "5.2.2", "5.4", "11.2", "11.1"  # Error in purrr::pmap(.l = list(igOrEsc = whichProcessesToFit), sim = sim,  :
            #ℹ In index: 2.
            #ℹ With name: fireSense_EscapeFitted.
            #Caused by error in `roc.default()`:
            #  ! 'response' must have two levels
            , "12.1" # had no fires
) 

# Put them in an interesting order i.e., prioritize
top <- c("4", "6", "5", "9", "14", "12", "11", "15")
ord <- grepl(
  paste(paste0("^", top), collapse = "|"),
  expt$.ELFind   )
vals <- sapply(strsplit(expt$.ELFind[ord], "\\."), function(x) x[[1]])
ord2 <- match(vals, top)
ord3 <- as.numeric(!ord) * (max(ord2) + 1)
# ord3[ord3 == 0] <- 
expt <- expt[order(ord3), ]
expt <- rbind(expt[!expt$.ELFind %in% problematic,], expt[expt$.ELFind %in% problematic,])

if (!is.null(.phaseSetup$from)) {
  .prev <- as.data.frame(readRDS(.phaseSetup$from))
  .prevDone <- .prev[[grep("ELFind$", names(.prev), value = TRUE)[1]]][.prev$status == "DONE"]
  expt <- expt[expt$.ELFind %in% .prevDone, ]
  message("Phase ", .phase, ": ", NROW(expt), " ELFs are DONE in ",
          .phaseSetup$from, "; the rest need a later queue")
}
message("Queueing ", NROW(expt), " ELFs for fire years ", .fireYearStart, ":", .fireYearEnd)

# First runs: ELFs that fail in the vecseq fuel-class join, so fireSenseUtils #50's message
# naming the duplicated species arrives early.
firstRuns <- c("14.3", "13.1")
expt <- rbind(expt[expt$.ELFind %in% firstRuns, ], expt[!expt$.ELFind %in% firstRuns, ])

if (!is.null(.phaseSetup$skipDoneIn) && file.exists(.phaseSetup$skipDoneIn)) {
  .other <- as.data.frame(readRDS(.phaseSetup$skipDoneIn))
  .otherDone <- .other[[grep("ELFind$", names(.other), value = TRUE)[1]]][.other$status == "DONE"]
  expt <- expt[!expt$.ELFind %in% .otherDone, ]
  message("Leaving out ", length(.otherDone), " ELFs already DONE in ", .phaseSetup$skipDoneIn)
}
if (!is.null(.phaseSetup$onlyELFs)) {
  ## An explicit whitelist, for a queue that must avoid ELFs another campaign is working on. Named
  ## rather than derived, because "what phase 2 is running right now" changes minute to minute and a
  ## queue is built once. Anything named but absent from the map is reported rather than ignored.
  .missing <- setdiff(.phaseSetup$onlyELFs, expt$.ELFind)
  if (length(.missing))
    warning("onlyELFs names ELFs that are not in this map: ", paste(.missing, collapse = ", "))
  expt <- expt[expt$.ELFind %in% .phaseSetup$onlyELFs, , drop = FALSE]
  message("Restricting to ", NROW(expt), " named ELFs: ", paste(expt$.ELFind, collapse = " "))
}
if (isTRUE(.phaseSetup$reverse)) {
  ## reverse the working order, but keep `problematic` last: phase 2 reaches those last too
  .isProb <- expt$.ELFind %in% problematic
  expt <- rbind(expt[!.isProb, , drop = FALSE][rev(seq_len(sum(!.isProb))), , drop = FALSE],
                expt[.isProb, , drop = FALSE])
}

rownames(expt) <- 1:NROW(expt) # re-number each row
####################
# Run the experiment -- this must be run at a command prompt, inside tmux
####################
workers <- SpaDES.project::experimentTmux(
  df                  = expt,          # df provided here
  global_path         = "global.R",
  n_workers           = .phaseSetup$n_workers,   # phase 1: memfrac = 0 keeps per-worker memory down; measured 90th-pct peak 37.7 GB
  queue_path          = queue_path,
  delay_before_source = 120,
  statusCalculate = quote({dd <- dir(file.path("outputs", runName), recursive = TRUE, full.names = TRUE)
                                  ee <- grep(value = TRUE, pattern = "objFun.*png$", dd)
                                  fi <- file.info(ee)
                                  tail(fi[order(fi$mtime),], 1) |> rownames() |> dirname()}),
  folderWithIterInFilename = quote({dd <- dir(file.path("outputs", runName), recursive = TRUE, full.names = TRUE)
                                   ee <- grep(value = TRUE, pattern = "hists", dd)
                                   fi <- file.info(ee)
                                   tail(fi[order(fi$mtime),], 1) |> rownames() |> dirname()}),
  workersToMonitor = outs$cores,
  runNameLabel = quote(colnames(q)[1]), # just first column in the queue
  ss_id = "https://drive.google.com/drive/folders/1X9-mRjyLMNpgkP_cfqhbr_AQEPOsVCHf"
)


if (FALSE) {
  # This is the abandonned experiment3 way
  future::plan("cluster", workers = min(NROW(expt), 3), persistent = TRUE)
  p <- SpaDES.project::experiment3(expt, file = "global.R",
                                   saveSimToDisk = FALSE, tmuxName = "ex8")
  # get google authentication
  sss <- readLines("~/googledriveAuthentication.R")
  eval(parse(text = sss)) |> options()
  
  # For estimating elapsed time
  sim = SpaDES.core:::savedSimEnv()$.sim
  ee = elapsedTime(sim)
  ee[, Predict := c("Fit", "Predict")[1 + as.numeric(grepl("redict", moduleName) | grepl("Dispersal|mortalityAndGrowth|summaryBGM", eventType))]]
  ee[, sum (elapsedTime), by = Predict]
  sim$.runName
}