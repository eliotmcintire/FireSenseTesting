## Measure what one ELF actually costs, so the next pass can be sized from evidence
## instead of guessed at.
##
## The fitting workflow runs in stages (see fireSense_SpreadFit's `stopAfterEvent`)
## whose resource profiles differ by an order of magnitude. Deciding how many runs
## fit on this machine needs the *per-run* cost of the stage about to be run, and
## the honest number for that is private memory: the resident-set size of a worker
## and its children double-counts pages they share with each other and with the
## rest of the machine, while `Private_Clean + Private_Dirty` from
## /proc/<pid>/smaps_rollup is what would actually be freed if that worker died.
##
## Usage
##   source("R/workerResources.R")
##   sampleWorkerResources()                      # one sample, as a data.frame
##   recordWorkerResources("logs/res.csv")        # loop, appending until stopped
##   summariseWorkerResources("logs/res.csv")     # peaks per ELF and per stage
##   workerBudget("logs/res.csv")                 # how many workers would fit

.eventNames <- c("init", "spreadFitPrepare", "estimateThreshold", "run", "plot",
                 ".inputObjects", "prepIgnitionFitData", "prepEscapeFitData",
                 "prepSpreadFitData", "initPlot")

## pid -> ppid for every live process, read once per sample.
.procParents <- function() {
  pids <- suppressWarnings(as.integer(list.files("/proc", pattern = "^[0-9]+$")))
  pids <- pids[!is.na(pids)]
  ppid <- vapply(pids, function(p) {
    l <- tryCatch(readLines(file.path("/proc", p, "stat"), warn = FALSE)[1], error = function(e) NA_character_)
    if (is.na(l)) return(NA_integer_)
    ## comm can contain spaces and brackets, so start after the closing paren
    suppressWarnings(as.integer(strsplit(sub("^.*\\) ", "", l), " ")[[1]][2]))
  }, integer(1))
  data.frame(pid = pids, ppid = ppid)
}

.descendants <- function(root, parents) {
  out <- root
  repeat {
    kids <- parents$pid[parents$ppid %in% out]
    new <- setdiff(kids, out)
    if (!length(new)) break
    out <- c(out, new)
  }
  out
}

## Private (USS) and resident memory of a process tree, in GB.
.treeMemory <- function(pids) {
  priv <- 0; rss <- 0
  for (p in pids) {
    f <- file.path("/proc", p, "smaps_rollup")
    if (!file.exists(f)) next
    l <- tryCatch(readLines(f, warn = FALSE), error = function(e) character())
    val <- function(key) {
      h <- grep(paste0("^", key, ":"), l, value = TRUE)
      if (!length(h)) 0 else suppressWarnings(as.numeric(gsub("[^0-9]", "", h[1])))
    }
    priv <- priv + val("Private_Clean") + val("Private_Dirty")
    rss <- rss + val("Rss")
  }
  c(privateGB = priv / 1048576, rssGB = rss / 1048576)
}

.meminfo <- function(key) {
  l <- grep(paste0("^", key, ":"), readLines("/proc/meminfo", warn = FALSE), value = TRUE)
  if (!length(l)) NA_real_ else as.numeric(gsub("[^0-9]", "", l[1])) / 1048576
}

## Everything not ours, so a shrinking headroom can be blamed on the right party.
.otherUsersGB <- function(me = Sys.info()[["user"]]) {
  x <- tryCatch(system2("ps", c("-eo", "user:32,rss", "--no-headers"), stdout = TRUE), error = function(e) character())
  if (!length(x)) return(NA_real_)
  parts <- strsplit(trimws(x), "\\s+")
  users <- vapply(parts, `[`, character(1), 1)
  rss <- suppressWarnings(as.numeric(vapply(parts, `[`, character(1), 2)))
  sum(rss[users != me], na.rm = TRUE) / 1048576
}

## Which ELF each worker is on, from the queue the workers themselves maintain.
.elfByPid <- function(queue_path) {
  if (is.null(queue_path) || !file.exists(queue_path)) return(setNames(character(), character()))
  q <- tryCatch(readRDS(queue_path), error = function(e) NULL)
  if (is.null(q) || !all(c("process_id") %in% names(q))) return(setNames(character(), character()))
  elfCol <- if (".ELFind" %in% names(q)) ".ELFind" else names(q)[1]
  keep <- !is.na(q$process_id) & nzchar(as.character(q$process_id))
  setNames(as.character(q[[elfCol]])[keep], as.character(q$process_id)[keep])
}

## The last module event a pane reported: the stage the cost belongs to.
.paneStage <- function(session, pane, lines = 200L) {
  out <- tryCatch(system2("tmux", c("capture-pane", "-p", "-J", "-S", paste0("-", lines),
                                    "-t", shQuote(paste0(session, ":0.", pane))), stdout = TRUE),
                  error = function(e) character())
  if (!length(out)) return(NA_character_)
  hits <- regmatches(out, regexpr(paste0(":(", paste(.eventNames, collapse = "|"), ")\\b"), out))
  if (!length(hits)) return(NA_character_)
  sub("^:", "", hits[length(hits)])
}

#' One sample of every R worker in a tmux session
#' @param session tmux session name.
#' @param queue_path the experiment queue `.rds`, to attribute workers to ELFs.
#' @return a data.frame, one row per worker, plus machine-level columns.
sampleWorkerResources <- function(session = "fits", queue_path = NULL) {
  panes <- tryCatch(system2("tmux", c("list-panes", "-t", shQuote(paste0(session, ":0")), "-F",
                                      shQuote("#{pane_index} #{pane_pid} #{pane_current_command}")), stdout = TRUE),
                    error = function(e) character())
  panes <- panes[grepl(" R$", panes)]
  parents <- .procParents()
  elf <- .elfByPid(queue_path)
  avail <- .meminfo("MemAvailable"); total <- .meminfo("MemTotal"); others <- .otherUsersGB()
  now <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
  if (!length(panes))
    return(data.frame(time = now, pane = NA_integer_, pid = NA_integer_, elf = NA_character_,
                      stage = NA_character_, nproc = 0L, privateGB = 0, rssGB = 0,
                      availGB = avail, totalGB = total, otherUsersGB = others))
  do.call(rbind, lapply(panes, function(p) {
    f <- strsplit(p, " ")[[1]]
    pane <- as.integer(f[1]); pid <- as.integer(f[2])
    tree <- .descendants(pid, parents)
    m <- .treeMemory(tree)
    ## The queue records the worker's own R pid, which is a child of the pane pid
    ## (the pane runs `env ... R`), so match anywhere in the tree, not just the top.
    inQueue <- intersect(as.character(tree), names(elf))
    data.frame(time = now, pane = pane, pid = pid,
               elf = if (length(inQueue)) elf[[inQueue[1]]] else NA_character_,
               stage = .paneStage(session, pane), nproc = length(tree),
               privateGB = round(m[["privateGB"]], 2), rssGB = round(m[["rssGB"]], 2),
               availGB = avail, totalGB = total, otherUsersGB = round(others, 1))
  }))
}

#' Sample on a loop, appending to a CSV
#' @param file where to append.
#' @param interval seconds between samples.
#' @param iterations number of samples; `Inf` to run until killed.
recordWorkerResources <- function(file, session = "fits", queue_path = NULL,
                                  interval = 30, iterations = Inf) {
  i <- 0
  while (i < iterations) {
    d <- try(sampleWorkerResources(session, queue_path), silent = TRUE)
    if (!inherits(d, "try-error"))
      utils::write.table(d, file, sep = ",", row.names = FALSE, append = file.exists(file),
                         col.names = !file.exists(file))
    i <- i + 1
    if (i < iterations) Sys.sleep(interval)
  }
  invisible(file)
}

#' Peak cost per ELF and per stage
#' @param file a CSV written by [recordWorkerResources()].
summariseWorkerResources <- function(file) {
  d <- utils::read.csv(file, stringsAsFactors = FALSE)
  d <- d[!is.na(d$pid), ]
  if (!NROW(d)) return(d)
  d$time <- as.POSIXct(d$time)
  agg <- do.call(rbind, lapply(split(d, list(d$elf, d$stage), drop = TRUE), function(x) {
    data.frame(elf = x$elf[1], stage = x$stage[1], samples = NROW(x),
               peakPrivateGB = max(x$privateGB), medPrivateGB = stats::median(x$privateGB),
               peakRssGB = max(x$rssGB), maxProc = max(x$nproc),
               minutes = round(as.numeric(difftime(max(x$time), min(x$time), units = "mins")), 1))
  }))
  agg[order(-agg$peakPrivateGB), ]
}

#' How many workers would fit
#'
#' Sizes on the worst stage each ELF has actually reached, because a worker holds its
#' own peak whatever stage produced it. Naming a `stage` restricts it to that stage;
#' the default (`NULL`) uses every stage sampled so far, so this is usable mid-run
#' instead of erroring until some particular event arrives.
#'
#' Two counts come back, and they answer different questions. `additionalWorkers` comes
#' from MemAvailable, which already excludes what the running workers hold, so it is how
#' many MORE could start now. `totalWorkers` comes from MemTotal and is the steady-state
#' ceiling, including the workers already running.
#'
#' @param file a CSV written by [recordWorkerResources()].
#' @param stage restrict to these stages; `NULL` (default) uses every stage sampled.
#' @param reserveGB memory to leave for other users and the OS.
#' @param quantile use this quantile of observed per-worker peaks, not the mean:
#'   ELFs differ several-fold in size, and the biggest one still has to fit.
workerBudget <- function(file, stage = NULL, reserveGB = 150,
                         quantile = 0.9, availGB = NULL, totalGB = NULL) {
  s <- summariseWorkerResources(file)
  if (!is.null(stage)) {
    s <- s[s$stage %in% stage, ]
    if (!NROW(s)) stop("no samples for stage ", paste(stage, collapse = "/"),
                       "; stages present: ",
                       paste(sort(unique(summariseWorkerResources(file)$stage)), collapse = ", "))
  }
  if (!NROW(s)) stop("no samples in ", file)
  ## One number per ELF: its worst stage so far, not one row per (elf, stage).
  perELF <- vapply(split(s$peakPrivateGB, s$elf), max, numeric(1))
  per <- stats::quantile(perELF, probs = quantile, names = FALSE)
  if (is.null(availGB)) availGB <- .meminfo("MemAvailable")
  if (is.null(totalGB)) totalGB <- .meminfo("MemTotal")
  list(stages = paste(sort(unique(s$stage)), collapse = ", "),
       nELFsObserved = length(perELF),
       perWorkerGB = round(per, 1), peakObservedGB = round(max(perELF), 1),
       availGB = round(availGB), totalGB = round(totalGB), reserveGB = reserveGB,
       additionalWorkers = max(0L, floor((availGB - reserveGB) / per)),
       totalWorkers = max(1L, floor((totalGB - reserveGB) / per)))
}
