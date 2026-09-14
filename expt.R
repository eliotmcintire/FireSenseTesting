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
            "PredictiveEcology/reproducible@development",
            # TEMPORARY PIN until SpaDES.core PR #452 merges: Plots(useCache = TRUE) dies under
            # spades.useCache = "eventsOnly", and its exit handler empties the whole cache. Back to
            # @development once merged.
            "PredictiveEcology/SpaDES.core@fix/plots-usecache-off",
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
queue_path <- "experiment_queue_fits_2026-09-13.rds"
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

# Data problems, not code -- each fails only after a full cold run, so leave them out until
# the data are fixed: tile 39's CNRM-ESM2-1 ssp370 2010s archive lacks 2013-2016 (15.1, 3.2.3,
# 5.1.3); no SCANFI species at all (3.2.1, 3.2.4).
dataBlocked <- c("15.1", "3.2.3", "5.1.3", "3.2.1", "3.2.4")
expt <- expt[!expt$.ELFind %in% dataBlocked, ]

# THIS CAMPAIGN: rerun exactly the ELFs that completed phase 1 on the old fire years.
# The 9 ELFs quarantined in that queue (3.1.2, 3.2.2, 3.2.5, 3.3.1, 3.3.2, 8.2, 10.1,
# 10.3.2, 12.1) are being worked on in a SEPARATE session -- their causes are no-tree /
# missing-land-cover / too-few-fires, none of which this rebuild fixes on its own.
#
# This list is written out rather than read from experiment_queue_fits_2026-09-10.rds:
# that file is being deleted (it was fit on 2002-2022 and is wrong for this campaign), and
# a file.exists() guard would silently fall through to "queue every ELF" once it was gone.
rerunELFs <- c(
  "10.2.1", "10.2.2", "10.3.1", "11.1", "11.2", "11.3", "11.4", "12.2", "12.3", "12.4",
  "13.1", "13.2.1", "13.2.2", "13.3", "14.1", "14.2", "14.3", "14.4", "15.2.1", "15.2.2",
  "3.1.1", "4.1", "4.2.1", "4.2.2", "4.3", "5.1.1", "5.1.2", "5.2.1", "5.2.2", "5.3.1",
  "5.3.2", "5.4", "6.1.1", "6.1.2", "6.1.3", "6.2.1", "6.2.2", "6.2.3", "6.3.1", "6.3.2",
  "6.4", "6.5", "6.6.1", "6.6.2", "7.1", "7.2", "7.3", "8.1", "9.1.1", "9.1.2", "9.2.1",
  "9.2.2", "9.2.3", "9.3"
)
missingFromELFs <- setdiff(rerunELFs, expt$.ELFind)
if (length(missingFromELFs))
  stop("rerunELFs not present in the ELF map or dropped by an exclusion above: ",
       paste(missingFromELFs, collapse = ", "))
expt <- expt[expt$.ELFind %in% rerunELFs, ]
stopifnot(NROW(expt) == length(rerunELFs))
message("Queueing ", NROW(expt), " ELFs for fire years ", .fireYearStart, ":", .fireYearEnd)

# First runs: ELFs that fail in the vecseq fuel-class join, so fireSenseUtils #50's message
# naming the duplicated species arrives early.
firstRuns <- c("14.3", "13.1")
expt <- rbind(expt[expt$.ELFind %in% firstRuns, ], expt[!expt$.ELFind %in% firstRuns, ])

rownames(expt) <- 1:NROW(expt) # re-number each row
####################
# Run the experiment -- this must be run at a command prompt, inside tmux
####################
workers <- SpaDES.project::experimentTmux(
  df                  = expt,          # df provided here
  global_path         = "global.R",
  n_workers           = 15,   # memfrac = 0 keeps per-worker memory down; measured 90th-pct peak 37.7 GB
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