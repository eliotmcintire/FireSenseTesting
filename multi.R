pak::pak(c("PredictiveEcology/Require@development",
           "PredictiveEcology/SpaDES.project@development",
           "PredictiveEcology/reproducible@development",
           "PredictiveEcology/SpaDES.core@development"), ask = FALSE)
outs <- SpaDES.project::preRunSetupProject(file = "global.R", upTo = "params")

#####################
# Getting files of scenarios from Google Drive
#####################

# List + filter on the Drive side. outList() already does name-pattern filtering.
allOnGS <- SpaDES.project::outList(outs$.uploadGSdir, pattern = "6\\.2\\.2.+NRV")

# get tar from GDrive, untar all objs, change the filenames from
#   the old /home/emcintir/GitHub (where the runs were made) to this user's ~/GitHub
sims <- SpaDES.project::reGetUntarLoad(
  allOnGS,
  destDir   = "/mnt/shared_cache/multi"
  , pathRemap = c(old = "/home/emcintir/GitHub", new = path.expand("~/GitHub"))
)

# SpaDES.project::outSave(lazy = TRUE, sim = sims[[1]], runName = sims[[1]]@params$.globals$.runName,
#                         simFilename = "test.rds")
# Now we have many simList objects in `sims`. Some of the parts of these are needed
#   but not all. What is needed is based on what the Summary modules need
outs$firePolys <- .unwrap(sims[[1]]$firePolys)
outs$outputsDF <- lapply(sims, SpaDES.core::outputs) |> data.table::rbindlist()
outs$studyAreaReporting <- .unwrap(sims[[1]]$studyAreaReporting)
## The sims hold no `reportingPolygons`. NRV_summary wants a named list of polygons, each with a label
## column given by attr(, "field"). Until real ones are built (LandWebUtils::buildReportingPolygons()),
## report on the ELF itself.
rp <- outs$studyAreaReporting
rp$Name <- sims[[1]]@params$.globals$.studyAreaName
attr(rp, "field") <- "Name"
outs$reportingPolygons <- list(ELF = rp)


summaryMods <- grep("summar", outs$modules, ignore.case = TRUE, value = TRUE)
summaryMods <- setdiff(summaryMods, "Biomass_summary")
outs$modules <- summaryMods


nams <- Map(nam = basename(names(sims)), function(nam) SpaDES.project::pathParse(nam))
## pathParse() gives .rep = "rep1"; the summary modules match integer rep ids
reps <- Map(nam = nams, function(nam) as.integer(sub("^rep", "", nam[[".rep"]])))
for (mod in summaryMods) {
  outs$params[[mod]]$mode <- "multi"
  outs$params[[mod]]$reps <- reps
}

# useful to use if needing to debug; otherwise leave as FALSE
if (FALSE)
  options(spades.evalPostEvent = #NULL
            quote({# print(.robustDigest(sim$spreadFirePolys));
              #   # print(.robustDigest(sim$rasterToMatch_biomassParam));
              tryCatch(print(sim@params$burnSummaries$mode), error = function(x) NULL)
            }))

outs$params$.globals$reps <- unname(unlist(reps))
outs$paths$outputPath <- 
  do.call(SpaDES.project::pathBuild, Require::modifyList2(nams[[1]], list(.rep = "_all")))
outs$params$.globals$.studyAreaName <- sims[[1]]@params$.globals$.studyAreaName
# outs$params$burnSummaries$reps <- unname(unlist(reps))
# It gets the original reps from the sim$modules params
simOut <- SpaDES.core::simInitAndSpades2(outs)

