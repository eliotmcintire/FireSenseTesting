#!/bin/bash
# Put parked workers back to work.
#
# When a job errors, tmuxRunWorkerLoop prints
#     Worker idle: res=error
#       q(status=1L) to retry  |  q() to stop the loop
# and waits at an R prompt. Unattended, every failure permanently costs a worker.
#
# It RESPAWNS; it does not retry. `q(status=1L)` re-runs THE SAME job, so a job that fails
# every time spins forever -- an earlier version of this script did exactly that, hammering
# 6.1.1 every five minutes. Quarantining the queue row does not help either: a parked
# worker retries from memory and never consults the queue. Ending the R process and
# relaunching from the pane's own startup script is the only way to make it claim
# something else.
#
# Killing a parked worker loses nothing: "Worker idle" means the job has already ended.
# Panes that are working are never touched.
#
# The startup script comes from the live process's own R_PROFILE_USER, not from the pane
# index -- tmux renumbers panes when one closes, so any index->script mapping goes stale.
#
# Usage:  R/reviveParkedWorkers.sh [session:window] [interval_seconds] [max_per_pane]
TARGET=${1:-fits:0}
INTERVAL=${2:-300}
MAXRESPAWN=${3:-6}
LOG=~/claudeSessions/2026-09-04-spreadfit-elf-cluster/revive.log
declare -A COUNT

while true; do
  for p in $(tmux list-panes -t "$TARGET" -F '#{pane_index}' 2>/dev/null | sort -n); do
    [ "$p" -lt 2 ] && continue
    tail=$(tmux capture-pane -p -t "$TARGET.$p" -S -10 2>/dev/null)
    echo "$tail" | grep -q "Worker idle" || continue

    n=${COUNT[$p]:-0}
    if [ "$n" -ge "$MAXRESPAWN" ]; then
      echo "$(date '+%F %T') pane $p parked again (respawned $n times already) -- LEAVING IT, needs a human" >> "$LOG"
      continue
    fi

    bash_pid=$(tmux list-panes -t "$TARGET" -F '#{pane_index} #{pane_pid}' | awk -v i="$p" '$1==i{print $2}')
    ## Skip defunct children: a zombie R still appears under the pane's bash, and
    ## /proc/<zombie>/environ is empty, so picking it loses the startup script.
    rpid=$(ps -o pid=,stat= --ppid "$bash_pid" 2>/dev/null | awk '$2 !~ /Z/ {print $1; exit}')
    [ -z "$rpid" ] && { echo "$(date '+%F %T') pane $p parked but no R child found; left alone" >> "$LOG"; continue; }
    prof=$(tr '\0' '\n' < /proc/$rpid/environ 2>/dev/null | sed -n 's/^R_PROFILE_USER=//p')
    [ -z "$prof" ] && { echo "$(date '+%F %T') pane $p has no R_PROFILE_USER; left alone" >> "$LOG"; continue; }

    elf=$(tmux capture-pane -p -t "$TARGET.$p" -S -2500 2>/dev/null | grep -oE "outputs/[0-9]+\.[0-9.]*[0-9]" | tail -1)
    err=$(tmux capture-pane -p -t "$TARGET.$p" -S -80 2>/dev/null | tr -d '\n' | grep -oE "Error[^|]{0,110}" | head -1)
    echo "$(date '+%F %T') pane $p parked on ${elf:-?} -- respawning (#$((n+1))) | ${err:0:110}" >> "$LOG"

    kill -TERM "$rpid" 2>/dev/null
    for _ in 1 2 3 4 5 6 7 8 9 10; do ps -p "$rpid" >/dev/null 2>&1 || break; sleep 1; done
    ps -p "$rpid" >/dev/null 2>&1 && kill -KILL "$rpid" 2>/dev/null
    sleep 2
    tmux send-keys -t "$TARGET.$p" \
      "env SPADES_USE_REQUIRE=false FS_PHASE=${FS_PHASE:-1} R_TESTS= R_BROWSER= R_PDFVIEWER= R_DEFAULT_PACKAGES=datasets,utils,grDevices,graphics,stats,methods R_PROFILE_USER='$prof' R --quiet --no-save --no-restore --interactive" Enter
    COUNT[$p]=$((n+1))
  done
  sleep "$INTERVAL"
done
