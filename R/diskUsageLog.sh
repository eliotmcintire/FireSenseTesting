#!/bin/bash
# Log disk use of everything a campaign touches, once per interval, as CSV.
#
# Two sizes per directory: `real` counts each inode once (what the disk actually holds),
# `apparent` counts every hard link as its own copy (what the files would cost without the
# stash's hardlinks). apparent/real >> 1 in inputs/ means the hardlinks are doing their job;
# a ratio near 1 means files are being duplicated.
#
# Usage:  R/diskUsageLog.sh <out.csv> [interval_seconds]     (0 = one sample, then exit)
OUT=${1:?csv path}
INTERVAL=${2:-3600}
DIRS="/mnt/fast/cache /mnt/fast/inputs /mnt/fast/data /mnt/fast/scratch/terra $HOME/GitHub/FireSenseTesting/outputs"
[ -s "$OUT" ] || echo "time,dir,realGB,apparentGB,nFiles,fastFreeGB,rootFreeGB" > "$OUT"
while true; do
  now=$(date '+%F %T')
  fastFree=$(df -BG /mnt/fast | awk 'NR==2{gsub("G","",$4); print $4}')
  rootFree=$(df -BG / | awk 'NR==2{gsub("G","",$4); print $4}')
  for d in $DIRS; do
    [ -d "$d" ] || continue
    real=$(du -sB1 "$d" 2>/dev/null | cut -f1)
    apparent=$(du -sB1 -l "$d" 2>/dev/null | cut -f1)
    n=$(find "$d" -type f 2>/dev/null | wc -l)
    echo "$now,$d,$(awk -v b="$real" 'BEGIN{printf "%.1f", b/1073741824}'),$(awk -v b="$apparent" 'BEGIN{printf "%.1f", b/1073741824}'),$n,$fastFree,$rootFree" >> "$OUT"
  done
  [ "$INTERVAL" -eq 0 ] && break
  sleep "$INTERVAL"
done
