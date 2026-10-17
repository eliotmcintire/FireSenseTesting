## Packages are installed by expt.R before any worker starts; workers only read the library.
## Running this file alone needs only SpaDES.project and Require.

projectDir <- if (Sys.info()[["user"]] == "ieddy") "~/git/FireSenseTesting" else "~/GitHub/FireSenseTesting"
setwd(projectDir)

# tmux panes report interactive() == TRUE; batch workers run inside tmux
userInteractive <- interactive() && !nzchar(Sys.getenv("TMUX"))

inSim <- SpaDES.project::setupProject(
  defaultDots = list(.basePath = "/mnt/fast", # inputs, outputs, cache, scratch and shared data live here
                     .rep = 1,
                     .ELFind = "4.3",
                     .SSP = 370,
                     .GCM = "CNRM-ESM2-1", # or "NRV"
                     .samplingRange = 1985:2024, # NRV climate years
                     # fitting window: vegetation is strictly before the fire year, so the first SCANFI V2 year
                     # (1985) feeds 1986; the end is the last year of historical climate.
                     # expt_draft.R checks this line against climateData::latestHistoricalYear().
                     .fireYears = c(1986L, 2024L),
                     .doFit = NA,                     # NA: fit only if no ledger fit; TRUE: always refit; FALSE: never fit
                     .spreadFitMode = "fit",          # or "fit,validate"
                     .heldOutFold = NA_integer_,      # 1 or 2 fits only that fold
                     .spreadFitFileSuffix = "",       # "" is the production ledger
                     .hindcastYear = NA_integer_,     # a SCANFI year makes this job a hindcast
                     .cores = c("birds", "camas", "carbon", "caribou", "coco", "core", "dougfir",
                                "fire", "mpb", "sbw", "acer", "abies", "pinus", "landr", "kodama"),
                     .times = list(start = 2020, end = 3020),
                     # parent: canClimateData, climateYear, fireSense, localEcozones,
                     # Biomass_borealDataPrep, Biomass_regeneration, Biomass_core
                     .modules = c("PredictiveEcology/LandR_ClimateSensitiveSimulator@development",
                                  # not in the parent: they need restricted PSP data
                                  "PredictiveEcology/Biomass_speciesFactorial@development",
                                  "PredictiveEcology/Biomass_speciesParameters@development",
                                  "PredictiveEcology/NRV_summary@modsForFireSense",
                                  "PredictiveEcology/burnSummaries@modsForFireSense",
                                  "PredictiveEcology/Biomass_summary@modsForFireSense")),
  # A hindcast holds year Y's vegetation static and draws Y's climate, 10 reps as decimal years
  # Y to Y.009. It runs no vegetation or summary modules.
  .hc = !is.na(.hindcastYear),
  .doFit = if (.hc) FALSE else .doFit, # a hindcast only predicts from a fit in the ledger; it stops if there is none
  .hcStep = 0.001, # years between hindcast reps
  .NRV = identical(.GCM, "NRV"),
  .ELFind = .ELFind,
  .rep = .rep, # without it, a .rep from the queue does not reach params$.globals
  .uploadGSdir = "https://drive.google.com/drive/folders/188ERmd1k6s6YMv3wHtnHQHD7pgLseBjf?usp=drive_link",
  paths = list(outputPath = if (.hc) SpaDES.project::pathBuild(pre = file.path(.basePath, "outputs", paste0("hindcast", .spreadFitFileSuffix)), .ELFind, .hindcastYear, .rep)
                            else SpaDES.project::pathBuild(pre = file.path(.basePath, "outputs"), .ELFind, .GCM, .SSP, .rep),
               cachePath = file.path(.basePath, "cache"),
               scratchPath = file.path(.basePath, "scratch"),
               inputPath = SpaDES.project::pathBuild(pre = file.path(.basePath, "inputs"), .ELFind, .GCM, .SSP, .rep)),
  # the outputs folder may arrive as its symlink target, so name the run by what is below ".../outputs/"
  runName = gsub("/", "_", sub("^.*/outputs/", "", paths$outputPath)),
  # a hindcast's 10 reps: end at 9.5 steps, as nine additions of .hcStep land just below 9 steps
  times = if (.hc) list(start = .hindcastYear, end = .hindcastYear + 9.5 * .hcStep) else .times,
  # a hindcast cannot use the parent: it would bring the vegetation modules
  modules = if (.hc) c("PredictiveEcology/canClimateData@modsForFireSense",
                       "PredictiveEcology/climateYear@modsForFireSense",
                       "PredictiveEcology/fireSense@development")
            else .modules,
  # only what this file itself calls or configures; modules list their own in reqdPkgs
  packages = c("PredictiveEcology/SpaDES.core@development (>= 3.2.1.9002)",      # simInitAndSpades2(), the events barrier
               "PredictiveEcology/reproducible@development (>= 3.2.1.9072)",     # Copy(), the reproducible.* options and their defaults
               "PredictiveEcology/SpaDES.project@development (>= 1.2.0.9018)",   # params merge, .rep from the caller, per-worker terra/GDAL defaults
               "PredictiveEcology/fireSenseUtils@development (>= 0.2.3.9097)",   # spreadFitFileTag, ignitionFitFilenameFor(), DEoptim default
               "qs2"),                                                           # reproducible.cacheSaveFormat
  require = c("reproducible", "terra"),
  options = list(
    "~/googledriveAuthentication.R", # each user's gargle settings
    LandR.assertions = FALSE,
    reproducible.cacheSaveFormat = "qs2",
    reproducible.shapefileRead = "terra::vect",
    reproducible.overwrite = TRUE,
    reproducible.destinationPathShared = file.path(.basePath, "data"),
    reproducible.cloudFolderID = "1oNGYVAV3goXfSzD1dziotKGCdO8P_iV9",
    reproducible.urlRemap = "https://raw.githubusercontent.com/PredictiveEcology/PredictiveEcology.org/main/scripts/arbutus_manifest_SCANFI_v2_clean.csv",
    reproducible.useCOG = FALSE,
    # showSimilar scans the whole cache on every miss: minutes per miss in batch workers
    reproducible.showSimilar = userInteractive,
    reproducible.showSimilarDepth = 8,
    reproducible.objSize = FALSE,
    reproducible.memoisePersist = TRUE,
    reproducible.useMemoise = userInteractive,
    clusters.deoptimConvergenceWindow = 400L, # DEoptim early stop: judge over 400 generations ...
    clusters.deoptimMinGenerations = 600L,    # ... and never before 600 (350 always stopped too early)
    mc.cores = 8, # per-process cap on climate mosaics, fire buffers and threshold forks; otherwise each takes the whole machine
    spades.recoveryMode = userInteractive + 0,
    spades.cacheChaining = !isTRUE(as.logical(Sys.getenv("FS_NO_CACHECHAINING", "FALSE"))),
    spades.allowInitDuringSimInit = TRUE,
    warnPartialMatchArgs = TRUE, # fireSense has objects partial matching would confuse (rstLCC, rstLCCs)
    warnPartialMatchAttr = TRUE,
    warnPartialMatchDollar = TRUE),
  params = Require::modifyList2(
    list(
      .globals = list(
        spreadFitFilename = paste0("fireSenseParams_", paste(.fireYears, collapse = "-"),
                                   fireSenseUtils::spreadFitFileTag, .spreadFitFileSuffix, ".rds"),
        .studyAreaName = .ELFind,
        heldOutFold = .heldOutFold,
        .runName = runName,
        .plotInterval = 100,
        .plots = "png",
        .useCache = c(".inputObjects", "init", "initPlot", "estimateThreshold", "spreadFitPrepare", "checkData")),
      canClimateData = list(climateGCM = .GCM, climateSSP = .SSP),
      fireSense_spreadFit = list(
        doFit = .doFit,
        cores = .cores,
        mode = strsplit(.spreadFitMode, ",")[[1]]),
      fireSense_dataPrepFit = list(
        dataYears = seq(1985L, 2020L, 5L), # every SCANFI year
        .useCache = c(".inputObjects", "dataPrepBuild", "prepEscapeFitData", "prepSpreadFitData",
                      "prepIgnitionFitData")),
      Biomass_speciesFactorial = list(.plots = NULL),
      Biomass_speciesParameters = list(standAgesForFitting = c(0, 100), balanceGrowth = TRUE),
      fireSense_ignitionFit = list(.useCache = TRUE, doFit = .doFit),
      burnSummaries = list(reps = .rep, summaryPeriod = as.integer(c(times$start, times$end))),
      NRV_summary = list(reps = .rep)),
    # NRV samples observed climate, so no projected years (canClimateData still needs a valid GCM)
    if (.NRV) list(
      canClimateData = list(climateGCM = "CNRM-ESM2-1", projectedClimateYears = integer(0)),
      climateYear = list(samplingRange = .samplingRange)),
    ## a hindcast plots nothing in the modules that run every 0.001-year rep; the data modules keep the fit's
    ## `.plots`, which is in their cache keys, so a hindcast reuses the fit's data prep (2026-10-17)
    if (.hc) list(
      climateYear = list(samplingRange = .hindcastYear),
      fireSense_dataPrepPredict = list(fireTimeStep = .hcStep, .plots = NA),
      fireSense_ignitionPredict = list(.runInterval = .hcStep, .plots = NA),
      fireSense_spreadPredict = list(.runInterval = .hcStep, .plots = NA),
      fireSense_burn = list(.runInterval = .hcStep, .plots = NA),
      fireSense_summary = list(hindcastYear = .hindcastYear, hindcastInterval = .hcStep, .plots = NA)))
)
message(inSim$runName)
inSimCopy <- reproducible::Copy(inSim)

## Phase 1 (FS_PHASE1_ONLY): build every input and cache, stop before the fit.
## Phase 2 (FS_PHASE2_ONLY): fit if this ELF has no fit, then stop.
## Neither: fit if this ELF has no fit, then predict.
.phase1Only <- isTRUE(as.logical(Sys.getenv("FS_PHASE1_ONLY", "FALSE")))
.phase2Only <- isTRUE(as.logical(Sys.getenv("FS_PHASE2_ONLY", "FALSE")))
stopifnot(!(.phase1Only && .phase2Only))
if (.phase1Only) {
  inSimCopy$events <- list(.stopBefore = list(fireSense_spreadFit = "run",
                                              fireSense_dataPrepPredict = "getClimateRasters"))
} else if (.phase2Only) {
  .validates <- "validate" %in% inSimCopy$params$fireSense_spreadFit$mode ||
    !is.na(inSimCopy$params$.globals$heldOutFold)
  inSimCopy$events <- list(.stopAfter = list(fireSense_spreadFit = if (.validates) "crossValidate" else "run"),
                           .stopBefore = list(fireSense_dataPrepPredict = "getClimateRasters"))
}

simOut <- SpaDES.core::simInitAndSpades2(inSimCopy)

## Runs that predict save their simList to Drive for multi.R; an upload failure only warns
if (!.phase1Only && !.phase2Only && !inSim$.hc)
  tryCatch(SpaDES.project::outSaveTarUpload(runName = inSim$runName, sim = simOut, gFolder = inSim$.uploadGSdir),
           error = function(e) warning("outSaveTarUpload failed: ", conditionMessage(e), call. = FALSE))
