## Package installation happens in expt.R, NOT here -- see its pak::pak() call.
## Every worker sources this file at the start of every job, so installing from here meant
## 13 processes writing one shared library: on 2026-09-09 a worker rewrote SpaDES.tools at
## 10:21:51 and four siblings then died with "lazy-load database ... is corrupt" as each
## next touched a SpaDES.tools function. The launcher installs once, before any worker
## exists; workers only read.
##
## Consequence worth knowing: running this file alone on a new machine needs expt.R's
## preamble first (its pak::pak() and preRunSetupProject()).

# generic absolute path for anybody; but individual can change
projectDir <- "~/GitHub/FireSenseTesting/"
if (Sys.info()["user"] == "ieddy"){
  projectDir <- "~/git/FireSenseTesting"
}
dir.create(projectDir, recursive = TRUE, showWarnings = FALSE)
setwd(projectDir)
# terra enables PROJ network access when it loads, so every projection may try to fetch
# datum-shift grids from cdn.proj.org (CloudFront). With flaky routing to that host, six
# workers sat in SYN-SENT for hours inside terra::project() (2026-09-07). Ballpark datum
# shifts (~1 m) are irrelevant at this resolution, so keep projections offline.
# if (requireNamespace("terra", quietly = TRUE)) terra::projNetwork(FALSE)
# terra assumes it owns the machine: memfrac = 0.6 lets EACH R session use 60% of RAM for
# raster operations, so six workers are entitled to 3.6 TB. On 2026-09-07 21:56 three workers
# reached ~100 GB each, the user slice peaked at 999 GB of 1005, and the OOM killer took the
# tmux server and every job. Cap terra at 60 GB per worker (six workers -> 360 GB); beyond
# that terra processes rasters in chunks from disk, slower but bounded.
# if (requireNamespace("terra", quietly = TRUE)) terra::terraOptions(memfrac = 0.06, memmax = 60)

# Google's OAuth endpoint (oauth2.googleapis.com) resolves to several addresses and one of
# them is intermittently black-holed from this network (2026-09-07: three fits lost to
# "Timeout was reached ... after 10002 ms" during a token refresh). curl only moves on to
# the next address if the connect timeout allows; 10 s does not, 60 s does. gargle and
# googledrive go through httr, so this applies to token refreshes and Drive calls alike.
# DNS here returns a single address for oauth2.googleapis.com, so when that one is black-holed
# there is nothing for curl to fall back to. Pin several Google front ends (all verified to serve
# the token endpoint on 2026-09-07); curl tries them in turn within the connect timeout.
if (requireNamespace("httr", quietly = TRUE))
  httr::set_config(httr::config(connecttimeout = 60L,
                                resolve = "oauth2.googleapis.com:443:172.217.112.4,172.217.114.4"))
# Load the Drive token up front. reproducible's auth cascade only runs when no token is
# loaded, and until reproducible PR #589 is in the library a transient failure inside that
# cascade calls drive_deauth(), which is process-wide and makes every later direct
# googledrive call (LandR's SCANFI drive_ls()) fail with "Does not exist". With a token
# already loaded the cascade short-circuits and nothing can deauthorise the session.
if (requireNamespace("googledrive", quietly = TRUE) && nzchar(getOption("gargle_oauth_email", "")))
  try(suppressMessages(googledrive::drive_auth(email = getOption("gargle_oauth_email"),
                                               cache = getOption("gargle_oauth_cache"))), silent = TRUE)
inSim <- SpaDES.project::setupProject(
  .uploadGSdir = "https://drive.google.com/drive/folders/188ERmd1k6s6YMv3wHtnHQHD7pgLseBjf?usp=drive_link",
  .rep = .rep,
  .ELFind = .ELFind,
  .strategy = .strategy,
  .cc = .cc,
  cores = .cores,
  FRU = FRU,
  .SSP = .SSP,
  .GCM = .GCM,
  .samplingRange = unlist(.samplingRange),
  defaultDots = list(.strategy = 1L, # used to be 3L; but seems to get caught in local minima
                     .cc = 0.5,
                     .objfunFireReps = 25L,
                     .rep = 1,
                     .ELFind = "4.3",
                     .SSP = 370,
                     .GCM = "CNRM-ESM2-1", # "NRV"
                     .samplingRange = 1990:2020, # vector
                     # Fire years for FITTING. 1985 is the first SCANFI V2 year; the end is
                     # the last year every input can supply, which is climate:
                     # climateData::latestHistoricalYear() is 2024 (NBAC reaches 2025, the
                     # local NFDB copy 2024). See latest-fire-year.md sec 2 for why the end
                     # year must be the minimum over inputs.
                     #
                     # NB these are the ONLY definition of the window. There is deliberately
                     # no `.fireYearStart = .fireYearStart` dot in the setupProject() call
                     # above: on 2026-09-11 that self-referencing form resolved to NA in the
                     # workers (the queue columns and the job env both held 1985/2024), so
                     # `.fireYearStart:.fireYearEnd` raised "NA/NaN argument", setupProject
                     # TOLERATED it, and every job then died three frames later in
                     # Require::modifyList2. 12 of 15 workers failed that way. The same shape
                     # is what the `.studyAreaName` comment below warns about.
                     # expt.R still stops unless its resolved end year matches .fireYearEnd,
                     # so the campaign window cannot drift from this default silently.
                     .fireYearStart = 1985L,
                     .fireYearEnd = 2024L,
                     .cores = c("birds", "biomass", "camas", "carbon", "caribou", "coco"
                                , "core", "dougfir", "fire"
                                , "mpb", "sbw", "mega"
                                , "acer"
                                , "abies"
                                , "pinus", "landr"
                                # kodama needs libtbb.so.12, which it has no root to install.
                                # clusters (>= 0.0.24) ships it to ~/.local/lib/clusters and puts
                                # that on the workers' LD_LIBRARY_PATH, so no sudo is required.
                                , "kodama"
                     ),
                     # .studyAreaName = "ELF", #{browser(); paste0("ELF", .ELFind)},
                     FRU = 25,
                     .times = list(start = 2020, end = 3020),
                     .modules = c("PredictiveEcology/canClimateData@development"
                                  ,"PredictiveEcology/climateYear@development"
                                  , "PredictiveEcology/fireSense_ELFs@main"
                                  , "PredictiveEcology/fireSense_dataPrepFit@development"
                                  , "PredictiveEcology/fireSense_IgnitionFit@development"
                                  , "PredictiveEcology/fireSense_SpreadFit@development"
                                  
                                  , "PredictiveEcology/fireSense_dataPrepPredict@development" # prepares data for predictions
                                  , "PredictiveEcology/fireSense_IgnitionPredict@development" # predicts ignitions & escapes
                                  , "PredictiveEcology/fireSense_SpreadPredict@development" # predicts raster of spreadProb
                                  , "PredictiveEcology/fireSense@development" # does burning
                                  # biomass modules
                                  , "PredictiveEcology/Biomass_borealDataPrep@development"
                                  , "PredictiveEcology/Biomass_speciesParameters@development"
                                  , "PredictiveEcology/Biomass_speciesData@development"
                                  , "PredictiveEcology/Biomass_regeneration@development"
                                  , "PredictiveEcology/Biomass_core@development"
                                  # summary modules 
                                  # , "FOR-CAST/NRV_summary@development"
                                  , "PredictiveEcology/NRV_summary@modsForFireSense"
                                  , "PredictiveEcology/burnSummaries@modsForFireSense"
                                  , "PredictiveEcology/fireSense_summary@development"
                                  , "PredictiveEcology/Biomass_summary@main"
                     )),
  # NB there is deliberately no `.studyAreaName` dot. A `...` argument that
  # references ANOTHER `...` argument does not resolve in setupProject(): both
  # `.studyAreaName = .ELFind` and
  # `.studyAreaName = if (exists(".studyAreaName")) .studyAreaName else .ELFind`
  # reach `paths` as their unevaluated expression, which pathBuild() then deparses
  # into a directory name -- that is where
  # `outputs/if_exists(".studyAreaName")_.studyAreaName_.ELFind/...` came from.
  # Referring to `.ELFind` directly at each use site is the form that works.
  # The study area here IS the ELF being fit, and the name must stay the bare ELF
  # id: fireSense_SpreadFit keys the shared cloud fit ledger on it, and
  # fireSense_dataPrepFit matches those keys against the ids on rasterToMatchELF.
  .objfunFireReps = .objfunFireReps,
  # useGit = "eliotmcintire",
  # Restart = TRUE,
  overwrite = FALSE, #!SpaDES.project::machine("A159568") && SpaDES.project::user("emcintir"), # redownload any updates
  paths = list(outputPath = SpaDES.project::pathBuild(.ELFind, .samplingRange, .GCM, .SSP, .rep),
               # cachePath = "/mnt/shared_cache/cache",
               cachePath = "/mnt/fast/cache",
               scratchPath = "/mnt/fast/scratch",
               # use inputPath on the shared drive, so destinationPathShared works
               # inputPath = SpaDES.project::pathBuild(pre = "/mnt/shared_cache/inputs", .studyAreaName, .samplingRange, .GCM, .SSP, .rep)),
               inputPath = SpaDES.project::pathBuild(pre = "/mnt/fast/inputs", .ELFind, .samplingRange, .GCM, .SSP, .rep)),
  runName = gsub("/", "_", fs::path_rel(paths$outputPath)) |>
    gsub(pattern = "outputs_", replacement = ""),
  times = as.list(unlist(.times, recursive = T)), # may be coming in as a slightly deeper list
  modules = unlist(.modules),
  packages = c(
    # "PrectiveEcology/reproducible@development (>=3.1.1.9020)"
    "SpaDES.core (>=3.1.2.9003)"
    , "eliotmcintire/fireregimetools@perf/read-study-area-only (>= 0.1.0.9007)"
    , "reproducible (>= 3.1.1)"
    , "PredictiveEcology/SpaDES.project@main (>= 1.0.1)"
    , "PredictiveEcology/LandR@development (>= 1.2.0)"
    , "PredictiveEcology/clusters@main (>= 0.0.22)"
    , "PredictiveEcology/fireSenseUtils@development (>= 0.2.0)"
    , "PredictiveEcology/pemisc@development (>= 0.0.4.9016)" # needed for LandWebUtils; not sure why
    , "qs2", "filelock"
    , "archive"
    , "googlesheets4"
    # fireSense/combined-fixes = development + PRs #23, #24 (years args, latestHistoricalYear)
    # and #25 (tile-dir regexp, the "[rast] extents do not match" fix), merged 2026-09-12
    # because the package's lead author has not merged them yet.
    , "PredictiveEcology/climateData@fireSense/combined-fixes (>= 2.2.3.9004)"
    , "terra" # "leaflet", "tidyterra",
    , "plyr"#, "scfmutils",
    , "geodata", "usethis"
    , "rvest" # needed for prepIgnitionFitData
    # , "extraPackages.R" # file not used currently; should just skip it
  ),
  require = c("reproducible", "data.table", "terra"), # data.table(): used unqualified in the outputs block; dots see only attached packages (SpaDES.project >= 1.1.0.9009)
  options = list(
    # gargle_oauth_email = "predictiveecology@gmail.com",
    # gargle_oauth_cache = ".secret",
    # gargle_oauth_client_type = "web", # for command line
    "~/googledriveAuthentication.R" # has the above lines in a list; each user can create their own file
    , "LandR.assertions" = FALSE # for production runs; very time consuming
    # , repos = unique(c(repos[[1]]
    #                    # , 'https://dmlc.r-universe.dev'
    #                    , getOption("repos")))
    , reproducible.cacheSaveFormat = "qs2"
    , reproducible.qsFormat = "qs2"
    , reproducible.useTry = FALSE
    , SpaDES.project.fast = FALSE
    , reproducible.shapefileRead = "terra::vect"
    , reproducible.overwrite = TRUE
    , reproducible.destinationPathShared = "/mnt/fast/data"
    , reproducible.gdriveAuthRetries = 6 # retry a transient Drive auth failure (backoff 2..12 s) before falling to anonymous
    # A host that fails clusters' per-host verification (e.g. cannot load a package)
    # is dropped from the DEoptim cluster with a message, instead of killing the job.
    , clusters.onBadHost = "drop"
    # When other live DEoptim builds hold every core, wait (re-measuring each minute) up to
    # this long instead of dying after hours of input preparation (clusters >= 0.0.29).
    , clusters.waitForCores = 8 * 3600
    , reproducible.cloudFolderID = "1oNGYVAV3goXfSzD1dziotKGCdO8P_iV9"
    , reproducible.showSimilarDepth = 8
    , reproducible.objSize = FALSE
    , reproducible.savePreDigest = FALSE
    , fireSenseUtils.runTests = FALSE
    , reproducible.memoisePersist = TRUE # sets the memoise location to .GlobalEnv; persists through a `load_all`
    , reproducible.nThreads = 1 #  When in parallel; can't do >1 ... only a warning
    # climateData::buildClimateMosaics() sizes its PSOCK cluster with parallelly::availableCores(),
    # which is the whole machine (80 here) unless mc.cores caps it: 40 nodes (historical) or 80
    # (future) per process. With 14 concurrent workers that launch failed inside
    # makeClusterPSOCK ("invalid connection", 9.2.1, 2026-09-12). Mosaicking is disk-bound;
    # 8 is plenty. Anything else that consults mc.cores gets the same sane per-process cap.
    , mc.cores = 8
    # STOPGAP until SpaDES.core > 3.2.1.9007 is installed (fleet runs 9005): its progress-tick
    # handler records the time of the last shown tick only on the non-dynamic path, so a
    # dynamic (\r) tick followed by a plain one dies with "argument is of length zero" in
    # `if (as.numeric(now - .pkgEnv$.progressLastShown) >= ...)` (14.4, 2026-09-12 22:32,
    # inside a Google Drive fetch in canClimateData). Non-dynamic cli output never takes the
    # \r path. Remove once the fixed SpaDES.core is installed.
    , cli.dynamic = FALSE
    # , reproducible.prepInputsUrlTiles = "https://drive.google.com/drive/folders/1IfeQ9rZ3-RIQwtcdo2T5Kn51NJJRWeox?usp=drive_link"
    # spades.useRequire is deliberately NOT set here. Its default is
    #   !tolower(Sys.getenv("SPADES_USE_REQUIRE")) %in% "false"
    # so a standalone run of this file installs as it always did, while a worker launched
    # by experimentTmux() -- which exports SPADES_USE_REQUIRE=false -- does not. Setting it
    # here either way would override the environment and put knowledge of the worker fleet
    # into a file that should stay self-contained.
    # , error = recover
    
    , reproducible.urlRemap = {reproducible::makeUrlRemap(
      utils::read.csv("~/GitHub/PredictiveEcology.org/scripts/arbutus_manifest_SCANFI_v2_clean.csv")
    )}
    , reproducible.useCOG = FALSE
    
    
    # For batch runs, these should be off
    , reproducible.showSimilar = FALSE #interactive() && !nzchar(Sys.getenv("TMUX"))
    , reproducible.showCachePreWarm = FALSE # the pre-warm fork only speeds an interactive showCache()
    , reproducible.useMemoise = TRUE # interactive() && !nzchar(Sys.getenv("TMUX"))
    , spades.recoveryMode = 1#(interactive() && !nzchar(Sys.getenv("TMUX"))) + 0
    # Reworked 2026-09-08. This must be ON during the FIRST pass, not just the ones that
    # benefit: cacheChainingPost() writes the chain tags with .addTagsRepo(), and that call
    # is inside `if (cacheChaining)`, so a pass run with it off records nothing for a later
    # pass to chain from. The benefit still only appears from the second run of an ELF
    # onward -- the prediction phase's replicates are where it should show.
    , spades.cacheChaining = TRUE
    # 2026-09-13 (Eliot): event-level caching only. Module-internal Cache() calls wrote 74 GB in
    # 2 h on the previous pass, 57 GB of it never read back; the event caches (19 GB) are what
    # let a failed job resume. Needs SpaDES.core >= 3.2.1.9006 (option added in PR #447).
    , spades.useCache = "eventsOnly"
    , reproducible.cacheChaining = FALSE #interactive()
    
    , reproducible.gdalwarp = FALSE
    , Require.cloneFrom = Sys.getenv("R_LIBS_USER")
    , Require.usePak = TRUE
    , Require.verbose = 1
    , spades.moduleCodeChecks = FALSE
    , spades.allowInitDuringSimInit = TRUE
    , spades.evalPostEvent =  NULL
      # quote(print({co <- capture.output(terra::terraOptions()); co[[1]]}))
      # quote({ print(.robustDigest(sim$studyArea));
      #         print(.robustDigest(sim$studyAreaELF))
      # })
    , warnPartialMatchArgs = TRUE #fireSense has objects that will be fooled by partial matching (rstLCC, rstLCCs)
    , warnPartialMatchAttr = TRUE
    , warnPartialMatchDollar = TRUE
    , spades.debugModule = NULL),
  sideEffects = list(
    {gd <- file.path(paths$inputPath, "geodata"); geodata::geodata_path(gd)} # gadm on a non-interactive sessino needs this
    , terra::gdalCache(size = 2048)   # 2 GB
    , terra::projNetwork(FALSE)
    , terra::terraOptions(memmax = 4, todisk = TRUE, memfrac = 0)
    
    # , "OtherExtras.R" # Eliot has some dev things he does incl pkgload::
  ),
  .climVars = c("CMD_sm", "CMD_sp"),
  climateVariables = {
    climateData::climateLayers(.climVars, fun = quote(calcAsIs),
                               historicalYears = .fireYearStart:.fireYearEnd,
                               projected = ifelse(identical(.GCM, "NRV"), FALSE, TRUE))
  },
  climateVariablesForFire = list(ignition = gsub("_", "", .climVars), # This must match a layer in climateVariables (without 'historical_')
                                 # only sm for spread
                                 spread = gsub("_", "", grep("sm$", .climVars, value = TRUE))), # This must match a layer in climateVariables (without 'historical_')
  saveAndPlotInterval = 100,
  params = list(
    .globals = list(
      # The cloud object holding the fits. The year range is in the name: a different
      # fitting window is a different set of fits, so a new range refits every ELF on
      # purpose and the previous campaign's fits stay on Drive.
      spreadFitFilename = paste0("fireSenseParams_", .fireYearStart, "-", .fireYearEnd, ".rds")
      # dataYear = 2011,
      , .studyAreaName = .ELFind
      , .runName = runName
      , .plotInterval = saveAndPlotInterval
      , .plots = c("png")
      , sppEquivCol = "LandR" # will get a warning if this is not here
      , .useCache = c(".inputObjects", "init", "initPlot", "estimateThreshold", "spreadFitPrepare", "checkData")
      , minCoverThreshold = 0),
    climateYear = list(
      samplingEndYear = max(.samplingRange),
      samplingRange = range(.samplingRange),
      samplingStartYear = min(.samplingRange)
    ),
    # fireSense_ELFs = list(queue_path = "experiment_queue_predict5.rds"),
    fireSense_ELFs = list(.useCloud = FALSE), # cloud cache of the ELF maps off for now (2026-09-10)
    canClimateData = list(
      climateGCM =  ifelse(identical(.GCM, "NRV"), "CNRM-ESM2-1", .GCM) 
      ,climateSSP =  ifelse(identical(.SSP, ""), 370, .SSP) 
      # ,.useCache = ".inputObjects" # init is slow to cache
    ),
    climateYear = list(
      samplingRange = .samplingRange,
      samplingStartYear = max(.samplingRange) +1
    ),
    # fireSense = list(.plots = c("screen", "png")),
    fireSense_SpreadFit = list(
      # Three-stage workflow, each stage sized for what it costs:
      #   "spreadFitPrepare"  -- build inputs + caches; many runs at once
      #   "estimateThreshold" -- adds the forking threshold calibration
      #   "run"               -- adds DEoptim, one run at a time on the cluster
      #   NA                  -- carry on into SpreadPredict and the rest
      DEoptimTests = c("adTest", "SNLL_FS")
      , stopIfNoPreRunFit = SpaDES.project::user("emcintir") %in% FALSE
      # mutuallyExclusiveCols = list(
      #   youngAge = c("nf", unique(makeSppEquiv(ecoprovinceNum = ecoprovince)$fuel))
      # ),
      # .useCache = FALSE,
      , iterDEoptim = 1000
      , rep = .rep # This means that all Cache of DEoptim will now be different name
      , iterStep = 1 # run this many iterations before running again; this should be
      # set to itermax if Cache is not used; it is only useful for Cache
      , cores = cores
      , NP = {if (identical(cores, unique(cores))) 100 else length(cores)} # number of cores of machines
      , trace = 1
      , mode = "fit"# "visualize"),
      # mode = "debug",
      , strategy = .strategy
      , objfunFireReps = .objfunFireReps # this is the lowest that doesn't create an error
      , .c = .cc
      # SNLL_FS_thresh = snll_thresh,
      , doObjFunAssertions = FALSE
    ),
    fireSense_dataPrepFit = list(
      # Fire years and the vegetation/land-cover years they join to. Every fire year uses
      # the dataYear at or before it, so the first dataYear must not postdate .fireYearStart.
      fireYears = .fireYearStart:.fireYearEnd,
      dataYears = c(1985L, 1990L, 2000L, 2010L, 2020L),
      # missingLCCgroup = c("nf_dryland"), # must match fuel class land cover
      .useCache = c(".inputObjects",
                    # "init", # CAN'T cache this one because it is the trigger to "skip" a whole bunch if SpreadParams exist for the StudyArea
                    "dataPrepInit",
                    "prepEscapeFitData",
                    "prepSpreadFitData",
                    "prepIgnitionFitData",
                    "run")
    ),
    fireSense_IgnitionFit = list(
      rescalers = c("CMDsm" = 1000),
      .useCache = c(".inputObjects", "init", "prepIgnitionFitData", "run")
    ),
    burnSummaries = list(mode = "single", reps = .rep), #TODO confirm all params
    NRV_summary = list(mode = "single", reps = .rep), #TODO: confirm if all prams okay 
    fireSense_summary = list(mode = "single",
                             studyAreaName  = .ELFind, 
                             #reps = .rep,  
                             years = c(times$start, times$end)), 
    Biomass_summary = list(years = c(times$start, times$end), 
                           studyAreaName  = .ELFind,
                           mode = "single"
                           #reps = .rep #only needed for multi, and would be the total reps
    )
  ), 
  # objectSynonyms = list(c("flammableRTM", "flammableMap")),
  outputs =  {
    outputs <- rbind(
      data.table(objectName = "pixelGroupMap", saveTime = c(seq(times$start, times$end, saveAndPlotInterval)), 
                 exts = ".tif", fun = "writeRaster", package = "terra"), 
      data.table(objectName = "cohortData", saveTime = c(seq(times$start, times$end, saveAndPlotInterval))), 
      data.table(objectName = "speciesEcoregion", saveTime = times$end), 
      data.table(objectName = "ecoregion", saveTime = times$end), 
      data.table(objectName = "species", saveTime = times$end),
      data.table(objectName = "ecoregionMap", saveTime = times$end, exts = ".tif", 
                 fun = "writeRaster", package = "terra"),
      data.table(objectName =  "standAgeMap", saveTime = times$end, 
                 exts = ".tif", fun = "writeRaster", package = "terra"),
      data.table(objectName =  "nonForest_timeSinceDisturbance", saveTime = times$end, 
                 exts = ".tif", fun = "writeRaster", package = "terra"),
      data.table(objectName =  "rstLCC", saveTime = times$end, 
                 exts = ".tif", fun = "writeRaster", package = "terra"),
      data.table(objectName = "climateYearRecord", saveTime = times$end),
      
      fill = TRUE
    )
    outputs[is.na(fun), c("exts", "fun", "package") := .("rds", "saveRDS", "base")]
    outputs <- as.data.frame(outputs)
    outputs$arguments <- list(overwrite = TRUE)
    return(outputs)
  }# ,
  # studyAreaLarge = {
  #   reproducible::prepInputs(url = 'https://drive.google.com/file/d/1gW6DBurw2uBx5cAZLcmWd6qBD7eMEd-4/view?usp=share_link',
  #                                           fun = 'terra::vect',
  #                                           destinationPath = 'inputs')
  # }
  
)
message(paste0(inSim$runName, ", .strategy:", inSim$.strategy,
               " .objfunFireReps:", inSim$.objfunFireReps))

if (!is(inSim$climateVariables, "list")) browser()
inSimCopy <- reproducible::Copy(inSim)
# inSimCopy$modules <- grep("ELFs", inSimCopy$modules, value = TRUE)
########################################
# WHICH PHASE OF THE WORKFLOW THIS PASS RUNS
#
# The phases cost wildly different amounts per run, so each is its own pass with
# its own worker count:
#
#   1  caches   every input, cache and the threshold calibration, but NOT the
#               DEoptim fit. Cheap per run, so many runs at once.
#   2  fit      adds the DEoptim fit, and stops there. One run at a time, since
#               each takes the whole compute cluster.
#   3  predict  everything, i.e. on into SpreadPredict and the rest.
#
# Phases 1 and 2 are expressed with `spades()`'s `events` barrier (SpaDES.core
# PR #435): whatever the modules schedule still runs, and the call ends at the
# named event. Phase 3 passes no `events` at all.
#
# Set the phase per launch, without editing this file:
#   FS_PHASE=2 <the command that starts the workers>
.phase <- as.integer(Sys.getenv("FS_PHASE", unset = "1"))
if (!isTRUE(.phase %in% 1:3))
  stop("FS_PHASE must be 1 (caches), 2 (fit) or 3 (predict); got '", Sys.getenv("FS_PHASE"), "'")
message("FireSense phase ", .phase, ": ",
        c("build caches, stop before the fit", "fit, stop after it", "everything")[.phase])

# A barrier that silently fails to fire would run the fit in every worker of a pass
# meant to stop before it, so refuse to start when this SpaDES.core cannot honour
# one. `stoppedAt()` arrived with the barrier, so it is the marker.
if (.phase %in% 1:2 && !"stoppedAt" %in% getNamespaceExports("SpaDES.core"))
  stop("This SpaDES.core (", utils::packageVersion("SpaDES.core"), ", sha ",
       utils::packageDescription("SpaDES.core")$RemoteSha, ") has no `events` barrier: ",
       "`.stopBefore` would be read as a module name and the DEoptim fit would run. ",
       "Update SpaDES.core to development >= be8c209e.")

inSimCopy$events <- switch(
  .phase,
  list(.stopBefore = list(fireSense_SpreadFit = "run")),  # 1: caches
  list(.stopAfter  = list(fireSense_SpreadFit = "run")),  # 2: fit
  NULL                                                   # 3: predict (removes `events`)
)

########################################
# THE MAIN simInitAndSpades2 CALL
# pkgload::load_all("~/GitHub/fireSenseUtils/")
# memfrac = 0 (terra 1.9.46 accepts it, verified): every operation goes to disk rather
# than sizing its working buffer off total RAM. terra assumes it owns the machine, so a
# fraction of 1 TB is a per-process ceiling that 6+ concurrent workers cannot all honour.
# terra::terraOptions(memmax = 4, todisk = TRUE, memfrac = 0)
.terraOpts <- function(when) {
  o <- terra::terraOptions(print = FALSE)
  message("terra ", when, ": memfrac=", o$memfrac, " memmax=", o$memmax, " todisk=", o$todisk)
  o[c("memfrac", "memmax", "todisk")]
}
.terraBefore <- .terraOpts("in force after global.R")
suppressPackageStartupMessages(
  simOut <- SpaDES.core::simInitAndSpades2(inSimCopy)
)
## Read back on the way out. If these differ, something inside simInit/spades changed
## them, and the message names which field -- that is the whole diagnostic. Deliberately
## after the call rather than in on.exit(): global.R is source()d at top level, where
## there is no function frame for on.exit() to attach to.
.terraAfter <- .terraOpts("in force after the run")
if (!identical(.terraBefore, .terraAfter))
  message("terra: SOMETHING CHANGED terraOptions DURING THE RUN -- before: ",
          paste(names(.terraBefore), unlist(.terraBefore), sep = "=", collapse = " "),
          " | after: ",
          paste(names(.terraAfter), unlist(.terraAfter), sep = "=", collapse = " "))
########################################


# SAVE AFTERWARDS
if (FALSE) {
  SpaDES.project::outSaveTarUpload(
    runName = inSim$runName, 
    sim = simOut,
    gFolder = inSim$.uploadGSdir)
  
  if (FALSE) {
    prepInputs(targetFile = "fireSenseParams.rds", url = "https://drive.google.com/file/d/1-iD7Pj4cX3kag4TEHeGxGgW42Rf0ag2l/view?usp=drivesdk",
               destinationPath = "/home/emcintir/GitHub/FireSenseTesting/inputs",
               useCache = TRUE, purge = 7, overwrite = TRUE)
    SpaDES.core::Plots(inSim[grep("studyArea|rasterToMatch", names(inSim))],
                       title = paste0("StudyArea ", inSim$.runName),
                       fn = plotSAs,
                       filename = paste0("studyAreas", inSim$.runName),
                       path = inSim$paths$inputPath,
                       types = c("screen", "png")) |>
      reproducible::Cache(.functionName = "Plots_studyAreas",
                          useCache = !identical(names(dev.cur()), "null device"))
    
    SpaDES.project::plotSAsLeaflet(inSim[grep("studyArea|rasterToMatch", names(inSim))])
    
    fn <- "sim_FireSenseSpreadFit.qs2"
    saveState(filename = fn, files = FALSE)
    inSim2 <- SpaDES.core::loadSimList(fn)
    outSims <- restartSpades(inSim2)
    outSims <- restartSpades()
  }
  
}
