#!/bin/bash
# Remove terra temp files in /mnt/fast/scratch/terra left by processes that no longer exist.
#
# Each fits worker job runs in its own R process (experimentTmux pane_mode = "killAndNewPane"),
# and terra only removes its temp files when R exits normally; a pane kill, a crash or a
# stop leaves them behind (939 GB found on 2026-09-12). The file names carry the PID
# (spat_<hex>_<PID>_<session>.tif), so "no such live PID" identifies an orphan exactly.
# Files younger than 10 minutes are left alone, in case a process is still starting.
#
# RUN THIS ONLY WHEN NO JOB IS RUNNING (the relaunch window). "No such live PID" is NOT
# proof that nothing uses the file: on a cache hit reproducible < 3.2.1.9028 hardlinks a
# cached raster back to the path it was produced at -- for a terra temp file, a path named
# for the (dead) worker that produced it -- so a live job can hold a raster under a dead
# PID's name. Sweeping while jobs ran killed 9.2.1 three times on 2026-09-12
# ("[project] cannot create dataset from source"); see reproducible PR #601.
#
# Usage:  R/sweepTerraTemp.sh [interval_seconds]     (0 = one pass, then exit)
INTERVAL=${1:-1200}
LOG=~/claudeSessions/2026-09-04-spreadfit-elf-cluster/terra-sweep.log
cd /mnt/fast/scratch/terra || exit 1
while true; do
  n=0; bytes=0
  for pid in $(ls | grep -oE "^spat_[0-9a-f]+_[0-9]+_" | awk -F_ '{print $3}' | sort -u); do
    [ -d /proc/$pid ] && continue
    for f in spat_*_${pid}_*; do
      [ -e "$f" ] || continue
      [ -n "$(find "$f" -maxdepth 0 -mmin -10)" ] && continue
      ## Per-file record so a later "source file missing" error can be checked against it.
      echo "$(date '+%F %T') $f $(stat --printf '%s %y' "$f")" >> "${LOG%.log}-files.log"
      bytes=$((bytes + $(stat --printf %s "$f"))); rm -f "$f" && n=$((n+1))
    done
  done
  echo "$(date '+%F %T') removed $n files, $((bytes / 1073741824)) GB; scratch/terra now $(du -sh . | cut -f1); /mnt/fast free $(df -h /mnt/fast | tail -1 | awk '{print $4}')" >> "$LOG"
  [ "$INTERVAL" -eq 0 ] && break
  sleep "$INTERVAL"
done
