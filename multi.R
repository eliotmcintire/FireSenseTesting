pathParse <- function(path, pre = "outputs") {
  # Strip common archive/file extensions first, so "rep4.tar.gz" -> "rep4"
  clean <- sub("\\.tar\\.gz$", "", path)
  clean <- sub("\\.(zip|tar|gz|rds|qs)$", "", clean)
  # Strip leading "outputs_" or "outputs/" if present
  clean <- sub(paste0("^", pre, "[/_]"), "", clean)
  # Split on / first (preserves underscore-containing components like "6.1.2"),
  # then on _ within each piece. This keeps us honest about the path structure
  # when it's still slash-delimited, and falls back to pure _ split for runName.
  if (grepl("/", clean)) {
    parts <- unlist(strsplit(clean, "/"))
    # Further split only the GCM/SSP component on "_ssp" if needed, handled below
  } else {
    parts <- strsplit(clean, "_")[[1]]
  }
  # Peel from the right: rep is always last, then GCM(_ssp SSP), then range,
  # and .ELFind is whatever's left at the front.
  n <- length(parts)
  # Find the rep component (must match ^rep\d+$)
  repIdx <- which(grepl("^rep\\d+$", parts))
  if (length(repIdx) == 0) stop("No rep component found in: ", path)
  repIdx <- repIdx[length(repIdx)]  # rightmost, in case .ELFind weirdly contains "rep1"
  .rep <- as.integer(sub("^rep", "", parts[repIdx]))
  # Find the range component (must match ^\d+-\d+$)
  rangeIdx <- which(grepl("^\\d+-\\d+$", parts))
  if (length(rangeIdx) == 0) stop("No sampling range found in: ", path)
  rangeIdx <- rangeIdx[length(rangeIdx)]
  rangeNums <- as.numeric(strsplit(parts[rangeIdx], "-")[[1]])
  .samplingRange <- rangeNums[1]:rangeNums[2]
  # GCM/SSP lives between rangeIdx and repIdx.
  # If the input was slash-delimited, it's a single element like "NRV_ssp370".
  # If it was runName-flattened, "_ssp" got split so it's two elements.
  gcmSspParts <- parts[(rangeIdx + 1):(repIdx - 1)]
  gcmSspStr   <- paste(gcmSspParts, collapse = "_")
  if (grepl("_ssp", gcmSspStr)) {
    gs <- strsplit(gcmSspStr, "_ssp")[[1]]
    .GCM <- gs[1]
    .SSP <- gs[2]
  } else {
    .GCM <- gcmSspStr
    .SSP <- NA
  }
  # .ELFind is everything before the range
  .ELFind <- paste(parts[seq_len(rangeIdx - 1)], collapse = "_")
  list(
    .ELFind = .ELFind,
    .samplingRange = .samplingRange,
    .GCM = .GCM,
    .SSP = .SSP,
    .rep = .rep
  )
}

outs <- SpaDES.project::preRunSetupProject(file = "global.R", upTo = "params")
allOnGS <- googledrive::drive_ls(outs$.uploadGSdir)
# dd <- grep(pattern = "6.5\\_.*1991-2020\\_.*NRV\\_.*tar.gz", allOnGS$name)
multiFolder <- "multi/"
dir.create(multiFolder)
files <- by(allOnGS, INDICES = seq(NROW(allOnGS)), function(d) { 
  st <- system.time(
    out <- googledrive::drive_download(d, path = file.path(multiFolder, d$name)) |>
      Cache(.cacheExtra = getRemoteMetadata(url = googledriveIDtoHumanURL(d$id))$remoteHash)
  )
  print(st)
  out}) |> data.table::rbindlist()

listInTar <- by(files, INDICES = seq(NROW(files)), 
                function(d) untar(list = TRUE, d$local_path))
simNames <- lapply(listInTar, function(lit) lit[1])
untarred <- Map(d = files$local_path, 
            sn = simNames, function(d, sn) {
    untar(d, extras = "--absolute-names")
}) 

sims <- Map(d = files$local_path, sn = simNames, function(d, sn) {
  sim <- readRDS(sn) # don't use loadSimList because the file-backed objects won't have their files
  sim
})

# don't get . objects
# also modifyList doens't work:
# Error in x[[v]] <- if (v %in% xnames && is.list(x[[v]]) && is.list(val[[v]])) modifyList(x[[v]],  : 
#   ALTLIST classes must provide a Set_elt method [class: XGBAltrepPointerClass, pkg: xgboost]
lapply(sims, function(sim, outs) {
  needObjs <- setdiff(ls(sim), names(outs)) |> grep(pattern = "^.\\_", value = TRUE, invert = TRUE)
  if (length(needObjs))
    outs <- append(outs, mget(needObjs, envir(sim)))
  outs
})

# Can be anything
# outs$reportingPolygons <- sim$studyAreaReporting

# outs$runName <- gsub("/", "_", dirname(fs::path_rel(simName))) |>()
#   gsub(pattern = "outputs/", replacement = "")
# outs$params <- sim@params
outs$firePolys <- .unwrap(sims[[1]]$firePolys)
outs$outputsDF <- lapply(sims, outputs) |> data.table::rbindlist()
# outs$outputs <- sim@outputs
outs$reportingPolygons <- .unwrap(sims[[1]]$studyAreaReporting)
summaryMods <- grep("summar", outs$modules, ignore.case = TRUE, value = TRUE)
summaryMods <- setdiff(summaryMods, "Biomass_summary")
outs$modules <- summarMods
nams <- Map(nam = basename(names(sims)), function(nam) pathParse(nam))
reps <- Map(nam = nams, function(nam) nam[[".rep"]])
for (mod in summaryMods) {
  outs$params[[mod]]$mode <- "multi"
  outs$params[[mod]]$reps <- reps
}
# outs$paths$outputPath <- sim@paths$outputPath
options(spades.evalPostEvent = #NULL
  quote({# print(.robustDigest(sim$spreadFirePolys));
    #   # print(.robustDigest(sim$rasterToMatch_biomassParam));
    tryCatch(print(sim@params$burnSummaries$mode), error = function(x) NULL)
  }))

outs$params$.globals$reps <- unname(unlist(reps))
outs$paths$outputPath <- 
  do.call(pathBuild, modifyList2(nams[[1]], list(.rep = "_all")))
outs$params$.globals$.studyAreaName <- sims[[1]]@params$.globals$.studyAreaName
# outs$params$burnSummaries$reps <- unname(unlist(reps))
# It gets the original reps from the sim$modules params
simOut <- simInitAndSpades2(outs)

