#!/bin/bash
# Take an ELF that has already failed out of rotation, once it returns to the queue.
#
# A failed job goes back to PENDING, so the fleet re-claims it, fails again, and never
# reaches the ELFs that can still succeed. These failures are deterministic -- same error,
# same row counts, on a completely fresh cache -- so retrying buys nothing.
#
# It acts only on rows that BOTH carry a `last_error` and are PENDING/INTERRUPTED, so a
# RUNNING row is never altered under the worker holding it. QUARANTINED is not a status
# `.gs_claim_next_job()` will claim, and reversing it is a single cell edit.
#
# This is triage, not a fix: it keeps the run moving while the underlying per-ELF bugs
# (the vecseq many-to-many join, `all(dir.exists(allDirs))`) are still open.
#
# Usage:  R/quarantineFailedELFs.sh [interval_seconds] [queue.rds]
#         The Google Sheet is the queue file name without ".rds", as experimentTmux names it.
INTERVAL=${1:-600}
QUEUE=${2:-experiment_queue_fits_2026-09-10.rds}
LOG=~/claudeSessions/2026-09-04-spreadfit-elf-cluster/quarantine.log
cd ~/GitHub/FireSenseTesting || exit 1
while true; do
  QUEUE_RDS="$QUEUE" \
  R_LIBS_USER=/home/emcintir/.local/share/R/FireSenseTesting/packages/x86_64-pc-linux-gnu/4.6 \
  Rscript -e '
    suppressMessages({eval(parse(text = readLines("~/googledriveAuthentication.R"))) |> options()
                      library(googlesheets4)})
    qp <- Sys.getenv("QUEUE_RDS")
    if (!file.exists(qp)) quit(save = "no")
    q <- as.data.frame(readRDS(qp))
    idx <- which(!is.na(q$last_error) & q$status %in% c("PENDING", "INTERRUPTED"))
    if (length(idx)) {
      q$status[idx] <- "QUARANTINED"; saveRDS(q, qp)
      ss <- googledrive::drive_get(path = sub("\\.rds$", "", qp))$id[1]
      for (i in idx)
        try(range_write(ss = ss, data = data.frame(x = "QUARANTINED"), sheet = "Status",
                        range = cell_limits(c(i + 1L, 3L), c(i + 1L, 3L)), col_names = FALSE),
            silent = TRUE)
      cat(format(Sys.time(), "%F %T"), "quarantined:", paste(q$.ELFind[idx], collapse=", "), "\n")
    }' 2>/dev/null | grep -v "^$" >> "$LOG"
  sleep "$INTERVAL"
done
