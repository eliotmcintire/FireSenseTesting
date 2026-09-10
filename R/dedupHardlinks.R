## Replace duplicate copies of identical files with hardlinks to a single inode.
##
## Written for the aftermath of PredictiveEcology/reproducible#593, where
## `destinationPathShared` failed to link anything extracted from an archive, so every
## study area kept its own copy: 148 large files, 588 GB, e.g. CA_FAO_forest_2019.tif
## present as 71 paths across 50 inodes. Fixing the cause stops new duplicates; this
## reclaims the ones already on disk.
##
## Identity is established by content, never by name: files are grouped by (device, size),
## a cheap `headBytes` prefix digest splits the obvious non-matches, and only then is a
## full `digest::digest(file = )` computed per INODE (not per path). Two files are linked
## only when their full digests agree.
##
## Safety, in the order it is applied:
##   * same device only -- a hardlink cannot cross filesystems;
##   * regular files only, no symlinks;
##   * nothing modified within `minAgeMinutes` (default 60), so a file still being written
##     is never touched;
##   * nothing any live process holds open;
##   * the replacement is atomic: link to a temporary name in the same directory, then
##     rename over the target, so an interruption leaves the original in place;
##   * `dryRun = TRUE` by default.
##
## The caveat worth knowing: after linking, a process that opens one path and writes to it
## IN PLACE changes every other path too. Programs that write a new file and rename it (the
## norm, including GDAL and terra) are unaffected. Do not run this over a tree where
## something rewrites inputs in place.
##
## Pass every root that can hold the same file in ONE call, not one call each: the
## duplicates this is here to collapse are cross-tree -- a copy under `inputPath` and its
## twin under `destinationPathShared` -- and separate passes can only ever see within their
## own root. All roots must be on the same filesystem for a hardlink to be possible.
##
## Usage:
##   source("R/dedupHardlinks.R")
##   dedupHardlinks(c("/mnt/fast/cache", "/mnt/fast/inputs", "/mnt/fast/data"))
##   dedupHardlinks(c("/mnt/fast/cache", "/mnt/fast/inputs", "/mnt/fast/data"), dryRun = FALSE)
##
## Deliberately NOT the terra scratch directory: those files are transient, actively being
## written, and cleanTerraScratch() deletes them anyway.

dedupHardlinks <- function(path, dryRun = TRUE, minSizeMB = 1, minAgeMinutes = 60,
                           headBytes = 1e6, verbose = TRUE) {
  stopifnot(length(path) >= 1L, all(dir.exists(path)),
            requireNamespace("digest", quietly = TRUE))
  path <- normalizePath(path, mustWork = TRUE)
  minSize <- minSizeMB * 1024^2

  raw <- system2("find", c(shQuote(path), "-type", "f", "-size", paste0("+", floor(minSizeMB * 1024), "k"),
                           "-printf", shQuote("%D\t%i\t%n\t%s\t%T@\t%p\n")), stdout = TRUE)
  if (!length(raw)) {
    message("no files over ", minSizeMB, " MB under ", paste(path, collapse = ", "))
    return(invisible(NULL))
  }
  p <- do.call(rbind, strsplit(raw, "\t", fixed = TRUE))
  d <- data.frame(dev = p[, 1], ino = p[, 2], nlink = as.integer(p[, 3]),
                  size = as.numeric(p[, 4]), mtime = as.numeric(p[, 5]),
                  file = p[, 6], stringsAsFactors = FALSE)
  d <- d[d$size >= minSize, , drop = FALSE]

  ## A file still being written must never be linked.
  fresh <- (as.numeric(Sys.time()) - d$mtime) / 60 < minAgeMinutes
  openFiles <- unlist(lapply(list.files("/proc", pattern = "^[0-9]+$"), function(pp) {
    fds <- suppressWarnings(list.files(file.path("/proc", pp, "fd"), full.names = TRUE))
    if (!length(fds)) return(character())
    tgt <- suppressWarnings(Sys.readlink(fds))
    keep <- Reduce(`|`, lapply(path, function(r) startsWith(tgt, r)))
    sub(" \\(deleted\\)$", "", tgt[which(keep)])
  }), use.names = FALSE)
  d$usable <- !fresh & !(d$file %in% openFiles)

  ## Candidates: a (device, size) group holding more than one distinct inode.
  d$grp <- paste(d$dev, d$size, sep = ":")
  nIno <- tapply(d$ino, d$grp, function(x) length(unique(x)))
  cand <- d[d$grp %in% names(nIno)[nIno > 1L], , drop = FALSE]
  if (!nrow(cand)) {
    message("no duplicate candidates under ", paste(path, collapse = ", "))
    return(invisible(NULL))
  }

  ## One representative path per inode -- digest the inode, not each of its links.
  reps <- cand[!duplicated(cand$ino), c("dev", "ino", "size", "grp", "file", "nlink", "usable")]
  if (verbose)
    message(sprintf("%s: %d files in %d candidate groups, %d distinct inodes to hash",
                    paste(basename(path), collapse = "+"), nrow(cand),
                    length(unique(cand$grp)), nrow(reps)))

  ## Cheap prefix digest first: differing files usually differ early, and this avoids
  ## reading terabytes to prove that two same-sized rasters are not the same raster.
  headDigest <- function(f, n) {
    con <- tryCatch(file(f, "rb"), error = function(e) NULL)
    if (is.null(con)) return(NA_character_)
    on.exit(close(con))
    digest::digest(readBin(con, "raw", n = n), algo = "xxhash64", serialize = FALSE)
  }
  reps$head <- vapply(reps$file, headDigest, character(1), n = headBytes, USE.NAMES = FALSE)
  reps$hgrp <- paste(reps$grp, reps$head, sep = "|")
  keep <- reps$hgrp %in% names(which(table(reps$hgrp) > 1L))
  reps <- reps[keep & !is.na(reps$head), , drop = FALSE]
  if (!nrow(reps)) {
    message("prefix digests split every group: no duplicates")
    return(invisible(NULL))
  }

  ## Full digest, only for inodes that survived the prefix test.
  if (verbose)
    message(sprintf("  hashing %d inodes in full (%.1f GB of reads)",
                    nrow(reps), sum(reps$size) / 1024^3))
  reps$full <- vapply(reps$file, function(f)
    tryCatch(digest::digest(file = f, algo = "xxhash64"), error = function(e) NA_character_),
    character(1), USE.NAMES = FALSE)
  reps <- reps[!is.na(reps$full), , drop = FALSE]
  reps$key <- paste(reps$grp, reps$full, sep = "|")
  reps <- reps[reps$key %in% names(which(table(reps$key) > 1L)), , drop = FALSE]
  if (!nrow(reps)) {
    message("no byte-identical duplicates after full digest")
    return(invisible(NULL))
  }

  ## Keeper per content group: the inode already carrying the most links, so the fewest
  ## renames are needed and the most-shared copy stays canonical.
  reps <- reps[order(reps$key, -reps$nlink), ]
  keeperOf <- reps$file[!duplicated(reps$key)]
  names(keeperOf) <- reps$key[!duplicated(reps$key)]
  keeperIno <- reps$ino[!duplicated(reps$key)]
  names(keeperIno) <- names(keeperOf)

  ino2key <- setNames(reps$key, reps$ino)
  work <- cand[cand$ino %in% reps$ino, , drop = FALSE]
  work$key <- ino2key[work$ino]
  work <- work[work$ino != keeperIno[work$key], , drop = FALSE]   # every path not on the keeper inode
  work <- work[work$usable, , drop = FALSE]

  saved <- sum(reps$size[!duplicated(reps$ino) & reps$ino %in% work$ino])
  message(sprintf("  %s %d paths onto %d keeper inodes; frees %.1f GB",
                  if (dryRun) "would relink" else "relinking",
                  nrow(work), length(keeperOf), saved / 1024^3))
  if (dryRun || !nrow(work)) return(invisible(work))

  n <- 0L
  for (i in seq_len(nrow(work))) {
    target <- work$file[i]
    keeper <- keeperOf[[work$key[i]]]
    tmp <- file.path(dirname(target), paste0(".dedup_", basename(target), "_", Sys.getpid()))
    ok <- suppressWarnings(file.link(keeper, tmp))
    if (!isTRUE(ok)) next                       # cross-device, permissions: leave it alone
    if (isTRUE(file.rename(tmp, target))) n <- n + 1L else unlink(tmp)
  }
  message(sprintf("  relinked %d of %d", n, nrow(work)))
  invisible(work)
}
