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
            # LOCAL INTEGRATION (2026-09-17, Eliot: no merges to development until he has met achubaty and
            # ceresbarros). These two are PR BRANCHES, not development, and every launch reinstalls from
            # whatever is listed here -- pinning @development silently reverted them once already.
            #   SpaDES.core#456: an event's cacheId no longer depends on outputs(sim)$arguments' class
            #   2026-09-18: #456 merged (4663c1c) and its branch deleted -> the branch pin broke pak.
            #   development == the branch's code (compare: 1 merge commit, 0 files).
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
            # the PSOCK host handling this run depends on. clusters got a development branch
            # on 2026-09-28; track it like the other packages.
            #   LandR#228 merged 2026-09-17, so development now carries prepInputs_CWIM() and
            #   wetlandToLCC(); it also carries LandR#236, without which every ELF dies in
            #   Biomass_speciesData:init ("cropTo must be a Raster*..."). Do NOT pin LandR back to a
            #   branch: this pak block runs on EVERY launch and would reinstall over both fixes.
            "PredictiveEcology/LandR@development",
            "PredictiveEcology/clusters@development",
            #   filelock: CRAN/RSPM 1.0.3 leaks one file descriptor per failed timed lock attempt, and
            #   reproducible retries a held cache lock every 2.5 s. A fold waiting ~1.5 h on its twin's
            #   inputs reached ~2000 fds, and mclapply then failed with "file descriptor is too large for
            #   select()" (held-out 4.2.2, 2026-09-28). The fork (>= 1.0.3.9001) closes it.
            "PredictiveEcology/filelock@main",
            #   climateData PRs #22-#25 all merged 2026-09-18, so development now carries the
            #   years argument to climateLayers(), latestHistoricalYear(), the tile-dir regexp
            #   fix and the hms Imports declaration. Back on @development as of 2026-09-18.
            "PredictiveEcology/climateData@development",
            "PredictiveEcology/quickPlot@development"), ask = FALSE)
## INTEGRATION BRANCHES (2026-09-28, Eliot): PRs from the FireSense work stay open against
## development, unmerged, until the other maintainers have reviewed them. Runs use <repo>@modsForFireSense
## instead: development plus those PR branches, merged in. R/updateModsForFireSense.sh rebuilds them
## (merges forward, never force-pushes); rerun it after opening or updating such a PR. Currently:
##   LandR#248: imputed ages from a log(age) model, never negative (Biomass_borealDataPrep#131, #132).
##   LandR#250: SCANFI v3 non-forest land cover (fireSenseUtils >= 0.2.3.9061 defaults to it).
##   LandR#251: forest land never relabels water, snow/ice or 0 as disturbed forest.
##   LandR#255, #256: LANDISDisp ward-screen fix + pgv argument (Biomass_core#121 needs it); OpenMP threads.
##   climateData#29: climate stacks written band-interleaved and tiled.
## Modules are pinned @modsForFireSense in global.R / globalFireCarbon.R the same way.
## Installed after the call above; dependencies = FALSE keeps what the call above installed. Module
## reqdPkgs that say LandR@development (>= x) are satisfied by this install: an integration branch's
## version is the highest of development and its merged PR branches, so Require does not replace it.
## Go back to @development above once the PRs are merged.
pak::pkg_install(c("PredictiveEcology/LandR@modsForFireSense",
                   "PredictiveEcology/climateData@modsForFireSense"),
                 ask = FALSE, upgrade = FALSE, dependencies = FALSE)
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
# Per-phase launch settings: [[1]] phase 1 (caches), [[2]] fit or predict, whichever each ELF needs.
# FS_PHASE1_ONLY is the same switch global.R reads. The [[2]] queue takes only the ELFs the phase-1
# queue finished (`from`), so it can start while phase 1 still runs.
# A queue name is resumed, never extended: to add ELFs that finish later, use a new name.
## 2026-09-23 (Eliot): phases 2 and 3 are no longer chosen here -- global.R's `.stopAfter` barrier makes an ELF
## without a fit fit and stop, and one with a fit predict. Only phase 1 is asked for: FS_PHASE1_ONLY=TRUE, the same
## switch global.R reads. So there are two setups: [[1]] phase 1, [[2]] everything else (fit or predict).
if (nzchar(Sys.getenv("FS_PHASE")))
  stop("FS_PHASE is retired; use FS_PHASE1_ONLY=TRUE for phase 1, and leave it unset to fit or predict")
.phase1Only <- isTRUE(as.logical(Sys.getenv("FS_PHASE1_ONLY", unset = "FALSE")))
.phase <- if (.phase1Only) 1L else 2L   # index into the setups below
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
  ## (previous phase-1 entry, 2026-09-16: queue "experiment_queue_caches_2026-09-16b.rds", n_workers = 3,
  ##  from the fits queue, onlyELFs = 5.1.1 5.1.2 5.2.2 5.4 11.1 11.2 3.1.1 -- a whitelist computed AFTER the
  ##  fits had claimed; 5.3.2 had to be dropped from it when phase 2 went from six to ten workers.)
  ## 2026-09-20 LINEAR-FUEL WAVE, preparation pass. Inputs changed (deciduousCoverWeight is estimated again,
  ## and fireSense_spreadFit #29 calibrates its threshold on linear fuel), so every ELF is prepared again
  ## BEFORE any fit starts: a fit that has to rebuild its own inputs took 85-176 GB, and ten at once is
  ## more than mega has. 4 at a time. No fit queue is live, so no whitelist is needed.
  ## Do not launch before fireSenseUtils #70 and fireSense_spreadFit #29 are on development.
  list(queue = "experiment_queue_caches_2026-09-21.rds",  n_workers = 4,
       from = "experiment_queue_fits_2026-09-13.rds",
       ## too few fires by fireSenseUtils::ELFmergePlan()'s 50 / 50 rule over 1985-2024 (13.3: 40 fire
       ## polygons; 5.4: 46 natural ignitions). ELF merging is pinned (global.R), so they are left out.
       skipELFs = c("13.3", "5.4")),
  ## 2026-09-14: the first phase-2 queue fits the 54 ELFs that finished the phase-1 rerun; ELFs from the
  ## all-ELF phase-1 queue above need a later phase-2 queue name (a queue is resumed, never extended)
  ## 2026-09-17 WAVE 2, the same 10 ELFs as the 2026-09-16 wave, so the two can be compared. A queue is
  ## resumed and never edited, so this needs its own name and its own Google Sheet.
  ## NP is the cluster size (clusters:::.clusterNP), and global.R now asks for 40: 10 fits x 40 = 400 of 704
  ## cores, leaving room for the centring trial (2 x 40) and headroom on every host.
  ## What changed since wave 1, all of it deliberate (Eliot, 2026-09-17: "always run the best we have"):
  ## adaptive truncation (fireSenseUtils#61 + clusters#17), population-median convergence (clusters#19),
  ## final-population re-scoring so the ledger keeps 5 DISTINCT best members (fireSenseUtils#63 +
  ## fireSense_spreadFit#25), deterministic buffers (#62) and threshold seed, the calibration cutoff fix
  ## (fireSense_spreadFit#26), SCANFI + CWIM land cover, and Biomass_borealDataPrep #116-#123.
  ## The vegetation inputs therefore differ from wave 1: compare SPEED (wall per generation, generations to
  ## converge), not objective values.
  ## The smoke-test queue experiment_queue_fit_2026-09-17.rds (4.3 alone) FAILED at Biomass_borealDataPrep's
  ## .inputObjects: the pak block above had reinstalled LandR from @development over the local #228 build, so
  ## prepInputs_CWIM() was gone. Both pins now point at the integration branches, and all ten ELFs run here.
  ## (previous phase-2 entry, 2026-09-17: queue "experiment_queue_fit_2026-09-17b.rds", n_workers = 10, from the
  ##  fits queue, refitFitted = TRUE, onlyELFs = 4.1 4.2.2 4.3 5.2.1 5.3.1 5.3.2 6.1.1 6.1.2 6.1.3 6.2.1.)
  ## 2026-09-20 LINEAR-FUEL WAVE, replicate 1 of every ELF: one fuel column per fuel class on the linear
  ## scale, / 1e4 (chosen from 36 model-selection fits + replicates; see
  ## ~/claudeSessions/2026-09-18-fireSense-phase2-fit/appendix-notes.md). Fits only what the preparation pass
  ## above finished. Results go to a NEW parameter object (global.R, "_linearFuel"). Replicates 2 and 3
  ## follow in their own queues once this one is done (Eliot: every ELF gets parameters first).
  list(queue = "experiment_queue_fit_2026-09-21.rds",    n_workers = 10,  # each fit is a 40-worker cluster
       from = "experiment_queue_caches_2026-09-21.rds",
       refitFitted = TRUE,   # harmless with a new parameter object; kept so a resumed queue cannot skip
       skipELFs = c("13.3", "5.4")),
  ## (a third, predict-only entry, "experiment_queue_predict_2026-09-14.rds", is gone: an ELF that has a fit
  ##  now predicts from the [[2]] queue.)
  NULL
)[[.phase]]
## 2026-09-23 FRIDAY MACKENZIE (Eliot): Phase 1 + 2 for the two ELFs along the middle/lower Mackenzie River,
## 4.2.2 and 4.2.1, for a 2025-2044 two-ELF forecast. Selected with FS_SET=mackenzie so the wave entries above
## stay as they are. Their climate caches (canClimateData init) were cleared 2026-09-23: global.R's terra
## memfrac fix is not in any cache key, and the old rasters were smoothed (session log, Addendum 81).
if (identical(Sys.getenv("FS_SET"), "mackenzie")) {
  .phaseSetup <- list(
    list(queue = "experiment_queue_caches_2026-09-23mack.rds", n_workers = 2,
         onlyELFs = c("4.2.2", "4.2.1")),
    ## no `from`: phase 1 was stopped (Eliot, 2026-09-23 evening); a fit run does phase 1's work first anyway
    list(queue = "experiment_queue_fit_2026-09-23mack.rds", n_workers = 2,  # each fit is a 40-worker cluster
         refitFitted = TRUE,
         onlyELFs = c("4.2.2", "4.2.1"))
  )[[.phase]]
  message("FS_SET=mackenzie: ", if (.phase1Only) "phase 1" else "fit or predict", ", queue ", .phaseSetup$queue)
}
## 2026-09-26 OKANAGAN (Eliot): phase 1 for 14.3 (Okanagan valley, south-central BC) and 14.4, to add them to the
## held-out experiment (~/claudeSessions/2026-09-18-fireSense-phase2-fit/centring, tag cyb). Their "phase 2" is
## the experiment's own fits (runArm.R from the caches built here), not a fit queue, so only phase 1 is defined.
## 2026-09-27 (Eliot): their phase 2 runs through the module, not side scripts: fit + the module's own held-out-years
## validation (fireSense_spreadFit mode "validate" -> crossValidate). spreadFitMode reaches global.R's `.spreadFitMode`.
## 2026-09-28 HELD-OUT SET (Eliot): each ELF as two jobs, one per held-out fold (fireSense_spreadFit `heldOutFold`,
## >= 1.0.6.9021): fit on the other fold's years, score the held-out fold, no full fit, no ledger. Largest first by
## escaped-fire count, both folds of an ELF next to each other. Folds ignore the ledger (a full fit does not stop them).
if (identical(Sys.getenv("FS_SET"), "heldout")) {
  if (.phase1Only) stop("FS_SET=heldout has no phase-1 entry")
  ## 2026-09-30: new queue after per-fold SNLL threshold, other_agb removed, annual youngAge (spread + ignition),
  ## cap-hit penalty, fold fits saved as ledger rows. (previous: experiment_queue_heldout_2026-09-29f.rds)
  .phaseSetup <- list(queue = "experiment_queue_heldout_2026-09-30a.rds", n_workers = 14,  # each fold job is a 40-worker cluster
                      onlyELFs = c("6.2.1", "14.4", "4.3", "4.2.2", "4.1", "5.2.1", "14.3", "5.3.1", "5.3.2", "13.1"),
                      keepOrder = TRUE, heldOutFolds = 1:2, ignoreLedger = TRUE)
  message("FS_SET=heldout: ", length(.phaseSetup$onlyELFs), " ELFs x ", length(.phaseSetup$heldOutFolds),
          " folds, queue ", .phaseSetup$queue)
}
if (identical(Sys.getenv("FS_SET"), "heldoutsmoke")) {
  ## one small fold end to end before relaunching the held-out set (2026-09-30)
  if (.phase1Only) stop("FS_SET=heldoutsmoke has no phase-1 entry")
  .phaseSetup <- list(queue = "experiment_queue_heldoutsmoke_2026-09-30a.rds", n_workers = 1,
                      onlyELFs = "13.1", keepOrder = TRUE, heldOutFolds = 1L, ignoreLedger = TRUE)
  message("FS_SET=heldoutsmoke: 13.1 fold 1, queue ", .phaseSetup$queue)
}
if (identical(Sys.getenv("FS_SET"), "okanagan")) {
  .phaseSetup <- list(
    list(queue = "experiment_queue_caches_2026-09-26okan.rds", n_workers = 2,
         onlyELFs = c("14.3", "14.4")),
    list(queue = "experiment_queue_fitValidate_2026-09-28okan.rds", n_workers = 2,  # each fit is a 40-worker cluster; 09-28: fresh queue after the youngAge/hillSlope/fuel fixes
         onlyELFs = c("14.3", "14.4"), spreadFitMode = "fit,validate")
  )[[.phase]]
  message("FS_SET=okanagan: ", if (.phase1Only) "phase 1" else "fit + validate", ", queue ", .phaseSetup$queue)
}
queue_path <- .phaseSetup$queue
outs$params$fireSense_ELFs$queue_path <- queue_path
.ELFinds <- fireSenseUtils::runELFs(outs, whatOut = "allNames")
# Already-fitted ELFs, straight from the shared cloud ledger that fireSense_spreadFit
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

# Only fit what the ledger says is missing -- unless this phase is deliberately REFITTING ELFs whose
# stored parameters are stale because their inputs changed (new land cover, new vegetation parameters,
# a new objective). The ledger row is then no longer an answer to this run's question.
# The queue carries `.refitExisting = TRUE` to global.R, which passes it to fireSense_spreadFit's
# `refitExisting`; without it the module skips a polygon that has a row, wherever the queue came from.
if (!is.null(.phaseSetup$spreadFitMode))
  expt$.spreadFitMode <- .phaseSetup$spreadFitMode   # global.R's `.spreadFitMode` dot, read per job from the queue
if (isTRUE(.phaseSetup$ignoreLedger)) {
  message("Ignoring the ledger: held-out folds never read or write it")
} else if (isTRUE(.phaseSetup$refitFitted)) {
  message("Refitting ", sum(expt$.ELFind %in% .ELFsFitted), " ELFs that already have ledger rows")
  expt$.refitExisting <- TRUE   # global.R's `.refitExisting` dot, read per job from the queue
} else {
  expt <- expt[!expt$.ELFind %in% .ELFsFitted, ]
}

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
  message(if (.phase1Only) "Phase 1" else "Fit or predict", ": ", NROW(expt), " ELFs are DONE in ",
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
  if (isTRUE(.phaseSetup$keepOrder))  # the order given in onlyELFs, not the default priority order
    expt <- expt[order(match(expt$.ELFind, .phaseSetup$onlyELFs)), , drop = FALSE]
  message("Restricting to ", NROW(expt), " named ELFs: ", paste(expt$.ELFind, collapse = " "))
}
if (!is.null(.phaseSetup$heldOutFolds)) {
  ## one row per ELF x fold, the folds of an ELF adjacent; global.R's `.heldOutFold` dot, read per job
  .nf <- length(.phaseSetup$heldOutFolds)
  expt <- expt[rep(seq_len(NROW(expt)), each = .nf), , drop = FALSE]
  expt$.heldOutFold <- rep(as.integer(.phaseSetup$heldOutFolds), times = NROW(expt) / .nf)
}
if (!is.null(.phaseSetup$skipELFs)) {
  ## An explicit blacklist: ELFs left out of this queue whatever else selects them.
  .skipped <- intersect(.phaseSetup$skipELFs, expt$.ELFind)
  expt <- expt[!expt$.ELFind %in% .phaseSetup$skipELFs, , drop = FALSE]
  message("Leaving out ", length(.skipped), " named ELFs: ", paste(.skipped, collapse = " "),
          "; ", NROW(expt), " remain")
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
## Worker panes take the tmux SESSION's environment, not this process's, so FS_PHASE1_ONLY given on this command
## line never reached global.R in the workers: on 2026-09-26 an Okanagan phase-1 launch ran the spread fit. Put the
## switch into the session (TRUE or FALSE, so a stale value from an earlier launch cannot linger).
if (nzchar(Sys.getenv("TMUX"))) {
  system2("tmux", c("set-environment", "FS_PHASE1_ONLY", if (.phase1Only) "TRUE" else "FALSE"))
  ## an expt.R fit queue is a strict phase-2 wave: fit, then stop (global.R otherwise goes on to predict)
  system2("tmux", c("set-environment", "FS_PHASE2_ONLY", if (.phase1Only) "FALSE" else "TRUE"))
} else if (.phase1Only) {
  stop("FS_PHASE1_ONLY=TRUE needs expt.R to run inside tmux, so the worker panes can inherit it")
}
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