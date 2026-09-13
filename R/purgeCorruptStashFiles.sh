#!/bin/bash
# Remove a corrupt file from the shared stash AND every place reproducible knows about it, so the
# next prepInputs() re-downloads it once (under the stash lock) instead of trusting the old copy.
#
# Two SCANFI v2 layers on the arbutus mirror were corrupt (2026-09-12): same size as the Google
# Drive original, different bytes, one undecodable tile each. Every ELF that uses the species
# failed in cropTo() with "[crop] too few values for writing: 0 < N". A hardlink of the stash file
# sits in each ELF input dir, with a CHECKSUMS.txt row and a .hash sidecar beside it, so deleting
# the stash copy alone is not enough: the ELF copies keep the corrupt inode and their checksums
# still match it.
#
# Usage:  R/purgeCorruptStashFiles.sh [--apply] <fileName> [<fileName> ...]
#         Without --apply it only prints what it would remove.
APPLY=0; [ "$1" = "--apply" ] && { APPLY=1; shift; }
ROOTS="/mnt/fast/data /mnt/fast/inputs"
for f in "$@"; do
  stash="/mnt/fast/data/$f"
  [ -e "$stash" ] || { echo "not in stash: $f"; continue; }
  ino=$(stat --printf '%i' "$stash")
  echo "## $f (inode $ino, $(stat --printf '%h' "$stash") links)"
  links=$(find $ROOTS -maxdepth 6 -inum "$ino" 2>/dev/null)
  sidecars=$(find $ROOTS -maxdepth 6 \( -name ".${f}_*.hash" -o -name "${f}.aux.xml" -o -name "${f}.ovr" \) 2>/dev/null)
  sums=$(grep -l -- "$f" /mnt/fast/data/CHECKSUMS.txt /mnt/fast/inputs/*/*/*/*/*/CHECKSUMS.txt 2>/dev/null)
  printf '%s\n' $links $sidecars | sed 's/^/  rm /'
  printf '%s\n' $sums | sed "s/^/  drop rows for $f from /"
  if [ $APPLY -eq 1 ]; then
    printf '%s\n' $links $sidecars | xargs -r rm -f
    # rows name the file either bare ("<file>" ...) or with its full path ("/mnt/fast/data/<file>" ...)
    for s in $sums; do grep -vE -- "^\"([^\"]*/)?${f//./\\.}" "$s" > "$s.tmp" && mv "$s.tmp" "$s"; done
    rm -f /mnt/fast/data/.stashLocks/"${f}"_*.lock
    echo "  applied"
  fi
done
