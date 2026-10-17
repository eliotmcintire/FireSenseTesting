## Test (2026-10-07): localEcozones + Biomass_speciesFactorial + Biomass_speciesParameters@development in the
## FireSense run, for one ELF, up to the end of the vegetation modules' init. No fitting, no prediction, no upload.
## Runs global.R's own statements up to its `inSimCopy <- reproducible::Copy(inSim)` line (so the run is built
## exactly as a worker builds it), then replaces global.R's tail: an events barrier that ends after
## Biomass_core's init, guards that end it before any fit or prediction event, and a local save instead of
## outSaveTarUpload(). Usage (from the project dir):  SPADES_USE_REQUIRE=false Rscript R/testVegInit.R 14.3
.args <- commandArgs(trailingOnly = TRUE)
.ELFind <- .args[1]
.rep <- 99
.GCM <- "NRV"
.areaMultiplierScale <- 10   # as the latest NRV runs (expt.R FS_SET=nrv), so dataPrepFit's caches match
options(reproducible.cacheSaveFormat = "qs2")
.t0 <- Sys.time()

.exprs <- parse("~/GitHub/FireSenseTesting/global.R", keep.source = FALSE)
.cut <- which(vapply(.exprs, function(e) identical(deparse(e)[1], "inSimCopy <- reproducible::Copy(inSim)"),
                     logical(1)))
stopifnot(length(.cut) == 1)
for (.e in .exprs[seq_len(.cut)]) eval(.e, envir = globalenv())

## the run must write only to new rep-99 folders
message("outputPath: ", inSim$paths$outputPath, "\ninputPath: ", inSim$paths$inputPath)
stopifnot(grepl(file.path(.ELFind, "1990-2020/NRV/370/99$"), inSim$paths$outputPath),
          grepl(file.path(.ELFind, "1990-2020/NRV/370/99$"), inSim$paths$inputPath))

inSimCopy$events <- list(
  .stopAfter = list(Biomass_core = "init"),
  ## guards, in case Biomass_core's init ran inside simInit (spades.allowInitDuringSimInit)
  .stopBefore = list(fireSense_spreadFit = c("spreadFitPrepare", "estimateThreshold", "run", "crossValidate"),
                     fireSense_ignitionFit = c("checkData", "run"),
                     fireSense_dataPrepPredict = "getClimateRasters",
                     Biomass_core = c("mortalityAndGrowth", "Dispersal")))

simOut <- SpaDES.core::simInitAndSpades2(inSimCopy)
.t1 <- Sys.time()
message("WALL: ", format(difftime(.t1, .t0, units = "mins")))
message("stoppedAt: ", paste(capture.output(print(SpaDES.core::stoppedAt(simOut))), collapse = "\n"))
saveRDS(list(speciesEcoregion = simOut$speciesEcoregion, species = simOut$species, ecoregion = simOut$ecoregion,
             ecoregionMap = terra::wrap(simOut$ecoregionMap), ecoregionLayer = simOut$ecoregionLayer,
             cohortData = simOut$cohortData, pixelGroupMap = terra::wrap(simOut$pixelGroupMap),
             completed = SpaDES.core::completed(simOut), elapsed = SpaDES.core::elapsedTime(simOut),
             wall = difftime(.t1, .t0, units = "mins")),
        file.path(inSim$paths$outputPath, "vegInitTest.rds"))
print(SpaDES.core::completed(simOut), nrows = 500)
print(SpaDES.core::elapsedTime(simOut, units = "mins"))
message("DONE")
