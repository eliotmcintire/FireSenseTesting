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
            "PredictiveEcology/climateData@development",
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
# RUN fireSense_ELFs to get the ELF map
####################

# Get all ELFs.
# NB: a NEW queue file. experimentTmux only materializes the queue from `expt`
# when queue_path does not yet exist (tmux.R ~L855); pointing at the old
# experiment_queue_predict5.rds would silently reuse its March 2026 rows --
# including 6 rows still marked RUNNING -- and ignore `expt` entirely.
queue_path <- "experiment_queue_fits_2026-09-10.rds" # new queue after clearCache: terra now attached before terraOptions() (global.R require)
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