## Prune reproducible cache entries that predate a cutoff and have not been read since.
##
## This cache is in multi-file mode: /mnt/fast/cache/multifileDB.txt exists and each entry
## in cacheOutputs/ is a self-contained set of files sharing one cacheId prefix --
## `<id>.qs2` (the artefact), `<id>.dbFile.{qs2,rds}` (its database record), any
## `<id>_<name>.tif` file backings, and `<id>.lock`. There is no shared SQLite, so
## deleting a whole cacheId group removes the record along with the artefact and cannot
## leave a dangling row that a later "hit" would fail to load.
##
## An entry is pruned only when EVERY file in its group is cold: written before `before`
## AND not read since `before`. /mnt/fast is mounted relatime, so a read of a file whose
## atime is more than a day old does update atime -- a genuinely reused entry will show it.
## Groups with a file open in a live process are skipped regardless.
##
## Usage:
##   source("R/pruneColdCache.R")
##   pruneColdCache()                 # dry run
##   pruneColdCache(dryRun = FALSE)

pruneColdCache <- function(cachePath = "/mnt/fast/cache",
                           before = as.POSIXct(format(Sys.Date(), "%Y-%m-%d 00:00:00")),
                           dryRun = TRUE) {
  dir <- file.path(cachePath, "cacheOutputs")
  stopifnot(dir.exists(dir))
  if (!file.exists(file.path(cachePath, "multifileDB.txt")))
    stop("no multifileDB.txt in ", cachePath,
         ": this cache may use a shared SQLite, where deleting files leaves dangling rows. ",
         "Use reproducible::clearCache() instead.")

  before <- as.numeric(before)
  raw <- system2("find", c(shQuote(dir), "-maxdepth", "1", "-type", "f",
                           "-printf", shQuote("%s\t%T@\t%A@\t%f\n")), stdout = TRUE)
  if (!length(raw)) {
    message("no files in ", dir)
    return(invisible(character()))
  }
  p <- do.call(rbind, strsplit(raw, "\t", fixed = TRUE))
  d <- data.frame(size = as.numeric(p[, 1]), mtime = as.numeric(p[, 2]),
                  atime = as.numeric(p[, 3]), name = p[, 4], stringsAsFactors = FALSE)
  ## Everything before the first "." or "_" is the cacheId; that is what groups an
  ## artefact with its own db record and file backings.
  d$id <- sub("[._].*$", "", d$name)

  ## Files any live process holds open, whatever wrote them.
  openFiles <- unlist(lapply(list.files("/proc", pattern = "^[0-9]+$"), function(pp) {
    fds <- suppressWarnings(list.files(file.path("/proc", pp, "fd"), full.names = TRUE))
    if (!length(fds)) return(character())
    tgt <- suppressWarnings(Sys.readlink(fds))
    basename(sub(" \\(deleted\\)$", "", tgt[startsWith(tgt, dir)]))
  }), use.names = FALSE)

  warm <- d$mtime >= before | d$atime >= before | d$name %in% openFiles
  ## One warm file protects its whole entry.
  warmIds <- unique(d$id[warm])
  drop <- !(d$id %in% warmIds)

  gb <- function(i) sum(d$size[i]) / 1024^3
  message(sprintf("%s: %d files in %d entries, %.1f GB",
                  dir, nrow(d), length(unique(d$id)), gb(TRUE)))
  message(sprintf("  keep %d entries (%.1f GB): written or read since %s, or open now",
                  length(warmIds), gb(d$id %in% warmIds),
                  format(as.POSIXct(before, origin = "1970-01-01"), "%Y-%m-%d %H:%M")))
  message(sprintf("  %s %d entries, %d files, %.1f GB",
                  if (dryRun) "would prune" else "pruning",
                  length(unique(d$id[drop])), sum(drop), gb(drop)))

  files <- file.path(dir, d$name[drop])
  if (!dryRun && length(files)) {
    ok <- unlist(lapply(split(files, ceiling(seq_along(files) / 5000)), file.remove))
    message(sprintf("  removed %d of %d", sum(ok), length(files)))
  }
  invisible(files)
}
