## Re-assert a queue claim that was lost while its worker kept running.
##
## Why this exists: the queue's authority is the Google Sheet ("Status" tab in the
## Drive folder passed to experimentTmux as `ss_id`). Re-running expt.R rebuilds the
## local queue from `df` and pushes it to the sheet, which clears `status` /
## `claimed_by` on rows that a still-live worker is working on. The row then reads
## PENDING while the process builds it, so the next worker to finish a job can claim
## the same ELF -- duplicate compute, and two processes racing on the same cache keys.
##
## `.gs_reclaim_dead_jobs()` cannot repair this: it only demotes RUNNING rows whose
## process is dead. It has no path from PENDING back to RUNNING.
##
## Usage:
##   source("R/reclaimStrayClaim.R")
##   reclaimStrayClaim("4.1", pid = 2976992)
##
## Only writes when the row currently reads PENDING and the pid is alive locally.

reclaimStrayClaim <- function(elf, pid,
                              queue_path = "experiment_queue_fits_2026-09-08.rds",
                              folder = "https://drive.google.com/drive/folders/1X9-mRjyLMNpgkP_cfqhbr_AQEPOsVCHf",
                              started_at = NULL,
                              dryRun = FALSE) {
  stopifnot(length(elf) == 1L, length(pid) == 1L)
  pid <- as.integer(pid)
  if (!dir.exists(file.path("/proc", pid)))
    stop("pid ", pid, " is not alive on this machine; nothing to reclaim.")

  authFile <- "~/googledriveAuthentication.R"
  if (file.exists(authFile))
    options(eval(parse(text = paste(readLines(authFile), collapse = "\n"))))

  sheet_name <- gsub("\\.rds$", "", basename(queue_path))
  ss <- googledrive::drive_ls(googledrive::as_id(folder), pattern = sheet_name)
  if (!NROW(ss))
    stop("no sheet named '", sheet_name, "' in that Drive folder.")
  ss_id <- ss$id[1L]
  message("sheet: ", ss$name[1L], " (", ss_id, ")")

  gsq <- SpaDES.project:::.gs_read_queue(ss_id)
  ## GS strips leading dots, so `.ELFind` is stored as `dotELFind`.
  elfcol <- grep("ELFind$", names(gsq), value = TRUE)[1L]
  i <- which(gsq[[elfcol]] == as.character(elf))
  if (length(i) != 1L)
    stop("expected exactly one row for ELF ", elf, "; found ", length(i))
  message("row ", i, ": status=", gsq$status[i], " claimed_by=", gsq$claimed_by[i])

  if (!identical(gsq$status[i], "PENDING")) {
    message("row is not PENDING -- leaving it alone.")
    return(invisible(FALSE))
  }
  if (isTRUE(dryRun)) {
    message("dryRun: would set row ", i, " to RUNNING for pid ", pid)
    return(invisible(NA))
  }

  worker_id <- paste0(Sys.info()[["nodename"]], "-", pid)
  ## Default the start time to the process's own start, not now, so elapsed times stay honest.
  if (is.null(started_at)) {
    secs <- as.numeric(system2("ps", c("-o", "etimes=", "-p", pid), stdout = TRUE))
    started_at <- format(Sys.time() - secs, "%Y-%m-%d %H:%M:%S")
  }
  ## Same field set, and the same stale-field scrubbing, as .gs_claim_next_job().
  upd <- list(status = "RUNNING", claimed_by = worker_id, started_at = started_at,
              finished_at = NA_character_, DEoptimElapsedTime = NA_character_,
              machine_name = Sys.info()[["nodename"]], process_id = as.character(pid),
              heartbeat_at = NA_character_, heartbeat_iter = NA_character_,
              iterationsTotal = NA_character_, interrupted_at = NA_character_)
  col_pos <- stats::setNames(seq_along(names(gsq)), names(gsq))
  SpaDES.project:::.gs_write_cells(ss_id, i + 1L, updates = upd,
                                   col_positions = col_pos,
                                   current_row = as.list(gsq[i, ]))
  SpaDES.project:::.mirror_local_queue(queue_path, i, upd)

  gsq2 <- SpaDES.project:::.gs_read_queue(ss_id)
  message("after: status=", gsq2$status[i], " claimed_by=", gsq2$claimed_by[i])
  invisible(TRUE)
}
