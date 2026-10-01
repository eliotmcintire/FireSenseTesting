## Package installation happens in expt.R, NOT here -- see its pak::pak() call.
## Every worker sources this file at the start of every job, so installing from here meant
## 13 processes writing one shared library: on 2026-09-09 a worker rewrote SpaDES.tools at
## 10:21:51 and four siblings then died with "lazy-load database ... is corrupt" as each
## next touched a SpaDES.tools function. The launcher installs once, before any worker
## exists; workers only read.
##
## Running this file alone needs only SpaDES.project and Require installed.

# generic absolute path for anybody; but individual can change
projectDir <- "~/GitHub/FireSenseTesting/"
if (Sys.info()["user"] == "ieddy"){
  projectDir <- "~/git/FireSenseTesting"
}
dir.create(projectDir, recursive = TRUE, showWarnings = FALSE)
setwd(projectDir)
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
userInteractive <- interactive() && !nzchar(Sys.getenv("TMUX"))
inSim <- SpaDES.project::setupProject(
  .uploadGSdir = "https://drive.google.com/drive/folders/188ERmd1k6s6YMv3wHtnHQHD7pgLseBjf?usp=drive_link",
  .rep = .rep,
  .ELFind = .ELFind,
  cores = .cores,
  FRU = FRU,
  .SSP = .SSP,
  .GCM = .GCM,
  .samplingRange = unlist(.samplingRange),
  defaultDots = list(.rep = 1,
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
                     # TRUE fits every ELF again even when the ledger has its parameters (use when the
                     # inputs or the model changed, so the stored fit is stale); FALSE fits only ELFs
                     # without parameters and predicts the rest. expt.R sets it per queue (`refitFitted`).
                     .refitExisting = FALSE,
                     # fireSense_spreadFit `mode`, comma-separated so it fits in one queue cell: "fit" or
                     # "fit,validate" (validate adds the module's held-out-years crossValidate). expt.R sets it per queue.
                     .spreadFitMode = "fit",
                     # fireSense_spreadFit `heldOutFold`: NA = normal fit; 1 or 2 = that held-out fold only (no full fit,
                     # no ledger). expt.R's FS_SET=heldout sets it per job.
                     .heldOutFold = NA_integer_,
                     .cores = c("birds", # "biomass", # TEMPORARY 2026-09-29: Dominique is rerunning BiomeBGC on biomass; put it back when she is done
                                "camas", "carbon", "caribou", "coco"
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
                                  ,"PredictiveEcology/climateYear@modsForFireSense" # development/main + our open PRs; R/updateModsForFireSense.sh
                                  , "PredictiveEcology/fireSense@development" # parent: the 9 fireSense modules (ELFs, dataPrepFit, ignitionFit, spreadFit, dataPrepPredict, ignitionPredict, spreadPredict, burn, summary)
                                  
                                  # biomass modules
                                  , "PredictiveEcology/Biomass_borealDataPrep@modsForFireSense"
                                  , "PredictiveEcology/Biomass_speciesParameters@development"
                                  , "PredictiveEcology/Biomass_speciesData@development"
                                  , "PredictiveEcology/Biomass_regeneration@development"
                                  , "PredictiveEcology/Biomass_core@modsForFireSense" # development/main + our open PRs; R/updateModsForFireSense.sh
                                  # summary modules 
                                  # , "FOR-CAST/NRV_summary@development"
                                  , "PredictiveEcology/NRV_summary@modsForFireSense"
                                  , "PredictiveEcology/burnSummaries@modsForFireSense"
                                  , "PredictiveEcology/Biomass_summary@modsForFireSense" # fireSense commits not yet in development; main was behind
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
  # id: fireSense_spreadFit keys the shared cloud fit ledger on it, and
  # fireSense_dataPrepFit matches those keys against the ids on rasterToMatchELF.
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
    "SpaDES.core (>= 3.2.1.9002)" # the `events` barrier (stoppedAt) used below
    , "reproducible (>= 3.1.1)"
    , "PredictiveEcology/SpaDES.project@development (>= 1.0.1)"
    , "PredictiveEcology/clusters@development (>= 0.0.22)"
    , "PredictiveEcology/fireSenseUtils@development (>= 0.2.3.9015)" # 9015: spread-fit buffers repeat for the same seed (#57)
    , "qs2", "filelock"
    , "archive"
    , "googlesheets4"
    # PRs #22-#25 merged upstream 2026-09-18, so development carries everything the
    # fireSense/combined-fixes branch had, plus the hms Imports fix. NOTE the floor is
    # 9002, not 9004: development's DESCRIPTION is 2.2.3.9002 (the branch bumped further
    # on its own), and a floor development cannot satisfy would fail every launch.
    , "PredictiveEcology/climateData@development (>= 2.2.3.9002)"
    , "terra" # "leaflet", "tidyterra",
    , "plyr"#, "scfmutils",
    , "geodata", "usethis"
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
    # When a Cache call misses, say WHICH argument differed. Without this a miss is silent, and a
    # miss on fireSense_spreadFit's estimateThreshold is expensive: it re-draws the SNLL threshold,
    # which changes the objective and invalidates every cached DEoptim generation for that ELF
    # (4.2.2 lost ~17 h that way on 2026-09-16, while 4.1 hit cache and replayed 797 generations).
    , reproducible.showSimilar = TRUE
    , reproducible.showSimilarDepth = 8
    , reproducible.objSize = FALSE
    , reproducible.memoisePersist = TRUE # sets the memoise location to .GlobalEnv; persists through a `load_all`
    # climateData::buildClimateMosaics() sizes its PSOCK cluster with parallelly::availableCores(),
    # which is the whole machine (80 here) unless mc.cores caps it: 40 nodes (historical) or 80
    # (future) per process. With 14 concurrent workers that launch failed inside
    # makeClusterPSOCK ("invalid connection", 9.2.1, 2026-09-12). Mosaicking is disk-bound;
    # 8 is plenty. Anything else that consults mc.cores gets the same sane per-process cap.
    , mc.cores = 8
    # , reproducible.prepInputsUrlTiles = "https://drive.google.com/drive/folders/1IfeQ9rZ3-RIQwtcdo2T5Kn51NJJRWeox?usp=drive_link"
    # spades.useRequire is deliberately NOT set here. Its default is
    #   !tolower(Sys.getenv("SPADES_USE_REQUIRE")) %in% "false"
    # so a standalone run of this file installs as it always did, while a worker launched
    # by experimentTmux() -- which exports SPADES_USE_REQUIRE=false -- does not. Setting it
    # here either way would override the environment and put knowledge of the worker fleet
    # into a file that should stay self-contained.
    # , error = recover
    
    # SCANFI v2 downloads come from the arbutus mirror (reproducible reads the manifest once, on first download)
    , reproducible.urlRemap = "https://raw.githubusercontent.com/PredictiveEcology/PredictiveEcology.org/main/scripts/arbutus_manifest_SCANFI_v2_clean.csv"
    , reproducible.useCOG = FALSE
    
    
    # For batch runs, these should be off
    , reproducible.showCachePreWarm = FALSE # the pre-warm fork only speeds an interactive showCache()
    , reproducible.useMemoise = userInteractive
    # each module sees only its own reqdPkgs (like a package's imports); nothing is attached (SpaDES.core >= 3.2.1.9030)
    , spades.reqdPkgsAttach = TRUE # FALSE once every module's reqdPkgs covers its calls (Biomass_speciesParameters #68 and others)
    # tmux panes report interactive() == TRUE, so `!interactive()` alone cannot detect a batch
    # runner; recoveryMode copies sim objects every event, which is wasted work for these.
    , spades.recoveryMode = userInteractive + 0
    # Reworked 2026-09-08. This must be ON during the FIRST pass, not just the ones that
    # benefit: cacheChainingPost() writes the chain tags with .addTagsRepo(), and that call
    # is inside `if (cacheChaining)`, so a pass run with it off records nothing for a later
    # pass to chain from. The benefit still only appears from the second run of an ELF
    # onward -- the prediction phase's replicates are where it should show.
    , spades.cacheChaining = TRUE
    # 2026-09-13 (Eliot): event-level caching only. Module-internal Cache() calls wrote 74 GB in
    # 2 h on the previous pass, 57 GB of it never read back; the event caches (19 GB) are what
    # let a failed job resume. Needs SpaDES.core >= 3.2.1.9006 (option added in PR #447).
    # 2026-09-18 (Eliot): back to "all". A failure inside dataPrepFit's .inputObjects lost ~2.5 h
    # of LCC prep per ELF twice (09-17, 09-18), because an unfinished event is never cached.
    # Safe for the LCC only with fireSense_dataPrepFit #36 (makeFireSenseLCCDeps() cache key).
    , spades.useCache = "all"
    
    , spades.allowInitDuringSimInit = TRUE
      # spades.evalPostEvent hooks used while debugging:
      # quote(print({co <- capture.output(terra::terraOptions()); co[[1]]}))
      # quote({ print(.robustDigest(sim$studyArea));
      #         print(.robustDigest(sim$studyAreaELF))
      # })
    , warnPartialMatchArgs = TRUE #fireSense has objects that will be fooled by partial matching (rstLCC, rstLCCs)
    , warnPartialMatchAttr = TRUE
    , warnPartialMatchDollar = TRUE),
  sideEffects = list(
    terra::gdalCache(size = 2048)   # 2 GB
    # terra enables PROJ network access when it loads, so every projection may try to fetch
    # datum-shift grids from cdn.proj.org (CloudFront). With flaky routing to that host, six
    # workers sat in SYN-SENT for hours inside terra::project() (2026-09-07). Ballpark datum
    # shifts (~1 m) are irrelevant at this resolution, so keep projections offline.
    , terra::projNetwork(FALSE)
    # terra assumes it owns the machine: memfrac = 0.6 lets EACH R session use 60% of RAM for
    # raster operations, so six workers are entitled to 3.6 TB. On 2026-09-07 21:56 three workers
    # reached ~100 GB each, the user slice peaked at 999 GB of 1005, and the OOM killer took the
    # tmux server and every job. Cap terra at 60 GB per worker (six workers -> 360 GB); beyond
    # that terra processes rasters in chunks from disk, slower but bounded.
    # todisk = TRUE writes every result to a file, so what a sim keeps is a file pointer: that is
    # what holds the PERSISTENT RAM down. memmax x memfrac only sizes the TRANSIENT block buffer
    # of one operation. It must not be tiny: memfrac = 0 (used until 2026-09-22) made terra warp
    # one output row at a time, and GDAL's bilinear then smooths the result -- every multi-layer
    # climate raster (CMDsm, CMDsp) came out with half its spatial spread (r 0.83 vs correct).
    # memfrac enters terra's block size squared (memory.cpp chunkSize), so 0.01 is still wrong.
    # Measured on 13.1's 40-layer CMDsm: 16 / 0.5 is exact, 4x faster than memfrac = 0, peak
    # +0.75 GB during the call, back to the same RSS after (session log 2026-09-22, Addendum 82).
    , terra::terraOptions(memmax = 16, todisk = TRUE, memfrac = 0.5)
    
    # , "OtherExtras.R" # Eliot has some dev things he does incl pkgload::
  ),
  ## Climate variables: fireSense_dataPrepFit's defaults (2026-09-23): ignition = CMD, cumMDC, CMD_sm, CMD_sp
  ## (xgboost uses all), spread = "auto" (per ELF, the one that best separates the bad fire years). It builds
  ## canClimateData's `climateVariables` from them, for its `fireYears` and, unless the GCM is NRV, the
  ## projected years. Supply `climateVariablesForFire` here only to override.
  saveAndPlotInterval = 100,
  params = list(
    .globals = list(
      # The cloud object holding the fits. The year range is in the name: a different
      # fitting window is a different set of fits, so a new range refits every ELF on
      # purpose and the previous campaign's fits stay on Drive.
      # The tag names the model (fireSenseUtils::spreadFitFileTag): "_linearFuel_esc50" = linear fuel biomass,
      # escaped fires start at 50 ha, and the annual-area and area-distribution objective terms (2026-09-25).
      # Older objects ("_linearFuel", none = log fuel) hold other models' fits. Once fireSense_dataPrepFit #43
      # (`"latest"`) merges, this line can go and every module defaults to the newest file with the ELF.
      spreadFitFilename = paste0("fireSenseParams_", .fireYearStart, "-", .fireYearEnd, fireSenseUtils::spreadFitFileTag, ".rds")
      # dataYear = 2011,
      , .studyAreaName = .ELFind
      # held-out validation fold (NA = normal fit): fireSense_spreadFit fits only that fold, and
      #   fireSense_dataPrepFit and fireSense_ELFs skip the SpreadFit ledger. Each checks the others agree.
      , heldOutFold = as.integer(.heldOutFold)
      , .runName = runName
      , .plotInterval = saveAndPlotInterval
      , .plots = c("png")
      , .useCache = c(".inputObjects", "init", "initPlot", "estimateThreshold", "spreadFitPrepare", "checkData")),
    # fireSense_ELFs = list(queue_path = "experiment_queue_predict5.rds"),
    # `fireYears` doubles as the on/off switch for the too-few-fires gate (fireSense_ELFs.R:237
    # tests `!is.null(Par$fireYears)`), so leaving it unset silently skipped sibling merging
    # altogether. Setting it enables fireSenseUtils::ELFmergePlan(): an ELF with too few fires is
    # merged with the neighbour sharing its base (3.1.2 with 3.1.1), and if the pair is still too
    # thin the base is left out of the queue. Merged ELFs are renamed (3.1.1 -> 3.1.1_2), so this
    # needs a NEW queue file and produces new output paths and ledger keys.
    # PINNED 2026-09-20 (Eliot): not for this wave. `fireYears` stays unset, so no ELF is merged or
    # renamed. The ELFs with too few fires are left out of the queue by hand instead (expt.R,
    # `skipELFs`): of the 54 fitted ELFs, 13.3 (40 fire polygons) and 5.4 (46 natural ignitions) fail
    # ELFmergePlan()'s 50 / 50 rule over 1985-2024. To turn merging on, restore the second line:
    #                       fireYears = .fireYearStart:.fireYearEnd),
    fireSense_ELFs = list(.useCloud = FALSE), # cloud cache of the ELF maps off for now (2026-09-10)
    canClimateData = list(
      climateGCM =  ifelse(identical(.GCM, "NRV"), "CNRM-ESM2-1", .GCM) 
      ,climateSSP =  ifelse(identical(.SSP, ""), 370, .SSP) 
      # ,.useCache = ".inputObjects" # init is slow to cache
    ),
    # A forecast uses each year's projected climate: with no samplingRange, climateYear takes the
    # simulation year whenever that year's climate exists. Only an NRV run samples historical years.
    climateYear = list(
      samplingRange = if (identical(.GCM, "NRV")) .samplingRange else NA_real_
    ),
    # fireSense_burn = list(.plots = c("screen", "png")),
    fireSense_spreadFit = list(
      # Three-stage workflow, each stage sized for what it costs:
      #   "spreadFitPrepare"  -- build inputs + caches; many runs at once
      #   "estimateThreshold" -- adds the forking threshold calibration
      #   "run"               -- adds DEoptim, one run at a time on the cluster
      #   NA                  -- carry on into SpreadPredict and the rest
      refitExisting = .refitExisting
      , stopIfNoPreRunFit = FALSE # fit an ELF that has no fit yet (phase 2), whoever runs this
      # mutuallyExclusiveCols = list(
      #   youngAge = c("nf", unique(makeSppEquiv(ecoprovinceNum = ecoprovince)$fuel))
      # ),
      # .useCache = FALSE,
      # The DEoptim cluster's size IS its population: clusters:::.clusterNP() sets NP to the workers
      # built, discarding any NP asked for. Measured per ELF on 2026-09-16: a generation costs the
      # slowest of NP evaluations and that barely falls with NP (4.1: 68.0 s at 120, 64.9 s at 60),
      # so throughput comes from running more ELFs at once -- 11 at NP 60 against 5 at NP 120.
      # NP is the cluster size (clusters:::.clusterNP). 40 rather than 60: on ELF 4.3, NP 120 needed
      # 74,600 evaluations to reach 58419 while NP 60 reached 58255 in 56,100 -- smaller populations were
      # more evaluation-efficient here, and 40 leaves cores for more ELFs at once (2026-09-17).
      , nCoresNeeded = 40
      , rep = .rep # This means that all Cache of DEoptim will now be different name
      , cores = cores
      ## `NP` removed 2026-09-20: fireSense_spreadFit never read it (not even before its #28, which deleted the
      ## parameter). The population size is the number of workers, i.e. `nCoresNeeded` above: 40.
      # , NP = {if (identical(cores, unique(cores))) 100 else length(cores)}
      , trace = 1
      , mode = strsplit(.spreadFitMode, ",")[[1]] # "visualize"),
      # mode = "debug",
      # SNLL_FS_thresh = snll_thresh,
      , doObjFunAssertions = FALSE
    ),
    fireSense_dataPrepFit = list(
      # missingLCCgroup = c("nf_dryland"), # must match fuel class land cover
      .useCache = c(".inputObjects",
                    # "init", # CAN'T cache this one because it is the trigger to "skip" a whole bunch if SpreadParams exist for the StudyArea
                    "dataPrepBuild", # the land-cover, fuel-class and time-since-disturbance work init used to do (module >= 1.2.0.9004)
                    "dataPrepInit",
                    "prepEscapeFitData",
                    "prepSpreadFitData",
                    "prepIgnitionFitData",
                    "run")
    ),
    fireSense_ignitionFit = list(
      .useCache = c(".inputObjects", "init", "prepIgnitionFitData", "run")
    ),
    ## Summarise every saved map, from start(sim) (year 0) to the end, every 100 years: the time series shows
    ## whether the landscape has stopped changing directionally (Eliot, 2026-10-01). The module default
    ## (start + 700 to start + 1000) showed only the last 300 years.
    burnSummaries = list(mode = "single", reps = .rep, #TODO confirm all params
                         summaryPeriod = as.integer(c(times$start, times$end)), summaryInterval = as.integer(saveAndPlotInterval)),
    NRV_summary = list(mode = "single", reps = .rep, #TODO: confirm if all prams okay
                       summaryPeriod = as.integer(c(times$start, times$end)), summaryInterval = as.integer(saveAndPlotInterval)),
    fireSense_summary = list(mode = "single"), 
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
message(inSim$runName)

inSimCopy <- reproducible::Copy(inSim)
# inSimCopy$modules <- grep("ELFs", inSimCopy$modules, value = TRUE)
########################################
# WHICH PHASE OF THE WORKFLOW THIS PASS RUNS
#
#   1  caches   every input, cache and the threshold calibration, but NOT the DEoptim fit.
#               Cheap per run, so many runs at once. Only when asked for: `.phase1Only`.
#   2  fit      the DEoptim fit, then stop.            } chosen by the state, not by a switch:
#   3  predict  on into SpreadPredict and the rest.    } fireSense_spreadFit's init schedules its
#                                                        `run` (the fit) only when the ledger holds
#   no parameters for this ELF (or `refitExisting = TRUE`). The `.stopAfter` barrier below stops
#   after that fit, so an ELF without a fit is fitted and stops (phase 2); an ELF with a fit has
#   no `run` event, the barrier never fires, and the run carries on (phase 3).
#   NB `.refitExisting = TRUE` (defaultDots above) forces a fit every time, so with it every run is
#   phase 2.
#
# Phases are expressed with `spades()`'s `events` barrier (SpaDES.core PR #435). For phase 1,
# `.stopBefore` alone is not enough: an ELF that already has a fit has no `run` event, so nothing
# would stop it. The run therefore also ends at its start time: everything scheduled then (inputs,
# caches, each module's init) runs, no simulated year does.
#
# Set it here, or per launch without editing this file:   FS_PHASE1_ONLY=TRUE <launch command>
.phase1Only <- FALSE
if (nzchar(Sys.getenv("FS_PHASE1_ONLY"))) .phase1Only <- isTRUE(as.logical(Sys.getenv("FS_PHASE1_ONLY")))
if (nzchar(Sys.getenv("FS_PHASE")))
  stop("FS_PHASE is retired: phases 2 and 3 now follow from whether this ELF has a fit. ",
       "Use FS_PHASE1_ONLY=TRUE for phase 1; leave it unset otherwise.")
## Eliot 2026-09-27: a run does everything it needs -- fits an ELF that has no fit, then predicts. Only a strict
## all-ELF phase-2 wave stops after the fit: FS_PHASE2_ONLY=TRUE (expt.R sets it for its fit queues).
.phase2Only <- isTRUE(as.logical(Sys.getenv("FS_PHASE2_ONLY", unset = "FALSE")))
if (.phase1Only && .phase2Only) stop("FS_PHASE1_ONLY and FS_PHASE2_ONLY are both TRUE")
message("FireSense: ", if (.phase1Only) "phase 1 only (build caches, stop before the fit)" else
          if (.phase2Only) "phase 2 only (fit if this ELF has no fit, then stop)" else
          "fit if this ELF has no fit, then predict")

# A barrier that silently fails to fire would run the fit in every worker of a pass
# meant to stop before it, so refuse to start when this SpaDES.core cannot honour
# one. `stoppedAt()` arrived with the barrier, so it is the marker.
if (!"stoppedAt" %in% getNamespaceExports("SpaDES.core"))
  stop("This SpaDES.core (", utils::packageVersion("SpaDES.core"), ", sha ",
       utils::packageDescription("SpaDES.core")$RemoteSha, ") has no `events` barrier: ",
       "`.stopBefore` would be read as a module name and the DEoptim fit would run. ",
       "Update SpaDES.core to development >= be8c209e.")

if (.phase1Only) {
  ## Stop before the fit, or, for an ELF that already has one (no `run` is scheduled), before the first
  ## prediction event. The time span is left alone: the summary modules check their summaryPeriod against it.
  inSimCopy$events <- list(.stopBefore = list(fireSense_spreadFit = "run",
                                              fireSense_dataPrepPredict = "getClimateRasters"))
} else if (.phase2Only) {
  ## with "validate" or a held-out fold, crossValidate is the fit's last event, after run. The defaultDots
  ## (.spreadFitMode, .heldOutFold) are not variables here, after setupProject(): read the resolved params.
  .sfp <- inSimCopy$params$fireSense_spreadFit
  .heldOut <- inSimCopy$params$.globals$heldOutFold # set in .globals for all three fold-aware modules
  inSimCopy$events <- list(.stopAfter = list(fireSense_spreadFit =
    if ("validate" %in% .sfp$mode || isTRUE(!is.na(.heldOut))) "crossValidate" else "run"))
}

########################################
# THE MAIN simInitAndSpades2 CALL
# pkgload::load_all("~/GitHub/fireSenseUtils/")
# terraOptions are set in sideEffects above (memmax = 16, todisk = TRUE, memfrac = 0.5); see the
# comment there for why memfrac = 0 was dropped. The check below reports them before and after.
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
