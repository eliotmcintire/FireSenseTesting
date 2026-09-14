#!/bin/bash
# Reclaim terra scratch files that belong to jobs the queue says are DONE -- and nothing else.
#
# A DONE row's process_id is the R process that ran that job; it has exited, and no live job can
# reference its temp files (cache-restored rasters no longer land in scratch since reproducible
# 3.2.1.9028). Helper-node files carry the NODE pid and are left alone on purpose: a live job may
# still hold their outputs. This is the only scratch reclaim that is safe while a fleet runs.
#
# Usage:  R/reclaimFinishedJobScratch.sh <queue.rds> [--apply]      (dry run by default)
Q=${1:?queue rds}; APPLY=${2:-}
cd /mnt/fast/scratch/terra || exit 1
pids=$(Rscript --vanilla -e 'q <- readRDS(commandArgs(TRUE)[1]); cat(na.omit(q$process_id[q$status == "DONE"]))' "$HOME/GitHub/FireSenseTesting/$Q" 2>/dev/null)
tot=0; n=0
for pid in $pids; do
  [ -d /proc/$pid ] && { echo "pid $pid still alive?! skipping"; continue; }
  for f in spat_*_${pid}_*; do
    [ -e "$f" ] || continue
    tot=$((tot + $(stat --printf %s "$f"))); n=$((n+1))
    [ "$APPLY" = "--apply" ] && rm -f "$f"
  done
done
echo "$(date '+%F %T') $([ "$APPLY" = "--apply" ] && echo removed || echo would remove) $n files, $((tot / 1073741824)) GB, from $(echo $pids | wc -w) finished jobs"
