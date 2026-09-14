#!/bin/bash
# Did the shared stash's hardlinks work? Every input file should exist as ONE real copy
# (one inode in /mnt/fast/data, reproducible.destinationPathShared) with a pointer from each
# ELF's inputs directory. Two reports:
#   1. stash files by link count: nlink == 1 means no ELF links to it (unused or copied instead);
#   2. inputs files whose basename exists under more than one inode = real duplicates, with the
#      space they waste.
#
# Usage:  R/auditHardlinks.sh [stash] [inputsRoot]
STASH=${1:-/mnt/fast/data}
INPUTS=${2:-/mnt/fast/inputs}
echo "== stash $STASH: files by number of hard links (nlink 1 = nothing points at it)"
find "$STASH" -maxdepth 1 -type f -printf '%n\n' | sort -n | uniq -c | awk '{printf "  nlink=%s: %s files\n", $2, $1}' | head -20
echo "== stash files that are NOT linked from anywhere (nlink 1), largest first"
find "$STASH" -maxdepth 1 -type f -links 1 -printf '%s %f\n' | sort -rn | head -10 | awk '{printf "  %.2f GB  %s\n", $1/1073741824, $2}'
echo "== inputs $INPUTS: basenames present under more than one inode (real duplicates)"
find "$INPUTS" -type f -printf '%i %s %f\n' 2>/dev/null \
  | sort -k3,3 -u -k1,1 \
  | awk '{n[$3]++; s[$3]+=$2; if (n[$3]==1) first[$3]=$2} END {tot=0; k=0; for (b in n) if (n[b] > 1) {k++; tot += s[b]-first[b]; printf "  %d inodes  %.2f GB wasted  %s\n", n[b], (s[b]-first[b])/1073741824, b}; printf "== %d duplicated basenames, %.1f GB wasted in total\n", k, tot/1073741824}' \
  | sort -k4 -rn | head -25
echo "== inputs: real vs apparent size (apparent counts each hard link)"
real=$(du -sB1 "$INPUTS" | cut -f1); app=$(du -sB1 -l "$INPUTS" | cut -f1)
awk -v r="$real" -v a="$app" 'BEGIN{printf "  real %.1f GB, apparent %.1f GB, ratio %.1f\n", r/1073741824, a/1073741824, a/r}'
