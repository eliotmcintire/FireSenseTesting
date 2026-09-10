## Reclaim orphaned terra scratch files from a scratch directory shared by many workers.
##
## Why not terra's own cleaner. `terra::tmpFiles()` cannot do this safely here:
##   * `orphan = TRUE` is only consulted in an `else if (orphan)` branch, so it is
##     silently ignored whenever `current = TRUE` (the default).
##   * `current = TRUE` means "every spat_* file in opt$tempdir". Our tempdir is
##     /mnt/fast/scratch/terra, shared by every worker, so that call from any session
##     would delete the live files of all the others.
##   * `terra:::.orphanTmpFiles()` decides "orphan" by scanning the CALLING session's
##     globalenv for SpatRasters. It cannot see other processes, so with a shared
##     tempdir every other worker's live file looks orphaned to it.
##
## The safe criterion, which is what this implements: the pid embedded in the file name
## (`spat_<hash>_<pid>_<rand>.tif`) is no longer alive, the file has exactly one link, so
## nothing else -- notably no file-backed object in the Cache -- references it, and no
## live process holds it open.
##
## Link counts come from find(1), not file.info(): file.info() has no `nlink` column, and
## reading a missing column gives NULL, which recycles to logical(0) and silently selects
## nothing at all.
##
## Usage:
##   source("R/cleanTerraScratch.R")
##   cleanTerraScratch()                 # dry run: reports what it would remove
##   cleanTerraScratch(dryRun = FALSE)   # actually removes

cleanTerraScratch <- function(dir = "/mnt/fast/scratch/terra", dryRun = TRUE,
                              minAgeMinutes = 60) {
  stopifnot(dir.exists(dir))
  raw <- system2("find", c(shQuote(dir), "-maxdepth", "1", "-type", "f",
                           "-name", shQuote("spat_*.tif"),
                           "-printf", shQuote("%n\t%s\t%T@\t%f\n")), stdout = TRUE)
  if (!length(raw)) {
    message("no spat_*.tif files in ", dir)
    return(invisible(character()))
  }
  p <- do.call(rbind, strsplit(raw, "\t", fixed = TRUE))
  d <- data.frame(nlink = as.integer(p[, 1]), size = as.numeric(p[, 2]),
                  mtime = as.numeric(p[, 3]), name = p[, 4], stringsAsFactors = FALSE)
  pid <- suppressWarnings(as.integer(sub("^spat_[0-9a-f]+_([0-9]+)_.*$", "\\1", d$name)))

  alive <- dir.exists(file.path("/proc", pid))
  alive[is.na(pid)] <- TRUE                      # unparsed name: never touch
  linked <- d$nlink != 1L                        # referenced elsewhere, e.g. by the Cache
  ## A just-reaped pid or a clock skew is the only way a recent file looks orphaned,
  ## so keep a grace period.
  fresh <- (as.numeric(Sys.time()) - d$mtime) / 60 < minAgeMinutes

  ## Held open by any live process, including by a pid other than the creator.
  openFiles <- unlist(lapply(list.files("/proc", pattern = "^[0-9]+$"), function(pp) {
    fds <- suppressWarnings(list.files(file.path("/proc", pp, "fd"), full.names = TRUE))
    if (!length(fds)) return(character())
    tgt <- suppressWarnings(Sys.readlink(fds))
    basename(sub(" \\(deleted\\)$", "", tgt[startsWith(tgt, dir)]))
  }), use.names = FALSE)
  held <- d$name %in% openFiles

  drop <- !alive & !linked & !fresh & !held
  gb <- function(i) sum(d$size[i]) / 1024^3
  message(sprintf("%s: %d files, %.1f GB", dir, nrow(d), gb(TRUE)))
  message(sprintf("  keep %d live-pid (%.1f GB), %d hardlinked, %d younger than %d min, %d open",
                  sum(alive), gb(alive), sum(!alive & linked),
                  sum(!alive & !linked & fresh), minAgeMinutes,
                  sum(!alive & !linked & !fresh & held)))
  message(sprintf("  %s %d orphans, %.1f GB",
                  if (dryRun) "would remove" else "removing", sum(drop), gb(drop)))
  files <- file.path(dir, d$name[drop])
  if (!dryRun && length(files)) {
    ok <- file.remove(files)
    message(sprintf("  removed %d of %d", sum(ok), length(files)))
  }
  invisible(files)
}
