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
    ## Parked = the pane sits at an R prompt AND "Worker idle" was printed after the last
    ## "Claimed job". Warnings printed after "Worker idle" can push it well above the last
    ## 10 lines (13:03 pane 15: six warnings), so look further back but anchor on the prompt.
    tail=$(tmux capture-pane -p -J -t "$TARGET.$p" -S -80 2>/dev/null | grep -v '^[[:space:]]*$')
    [ "$(echo "$tail" | tail -1 | sed 's/[[:space:]]*$//')" = ">" ] || continue
    echo "$tail" | awk '/Claimed job/{idle=0} /Worker idle/{idle=1} END{exit !idle}' || continue

    n=${COUNT[$p]:-0}
    if [ "$n" -ge "$MAXRESPAWN" ]; then
      echo "$(date '+%F %T') pane $p parked again (respawned $n times already) -- LEAVING IT, needs a human" >> "$LOG"
      continue
    fi

    pane_pid=$(tmux list-panes -t "$TARGET" -F '#{pane_index} #{pane_pid}' | awk -v i="$p" '$1==i{print $2}')
    ## The worker loop's killAndNewPane mode starts new panes with R as the pane's own process;
    ## the launch panes run R under bash. Under R, a child is a pre-warm fork or a callr process,
    ## not the worker: picking one (20:54 and 20:59, pane 12) killed the wrong process.
    if [ "$(ps -o comm= -p "$pane_pid" 2>/dev/null)" = "R" ]; then
      rpid=$pane_pid
    else
      ## Skip defunct children: a zombie R still appears under the pane's bash, and
      ## /proc/<zombie>/environ is empty, so picking it loses the startup script.
      rpid=$(ps -o pid=,stat= --ppid "$pane_pid" 2>/dev/null | awk '$2 !~ /Z/ {print $1; exit}')
    fi
    [ -z "$rpid" ] && { echo "$(date '+%F %T') pane $p parked but no R process found; left alone" >> "$LOG"; continue; }
    prof=$(tr '\0' '\n' < /proc/$rpid/environ 2>/dev/null | sed -n 's/^R_PROFILE_USER=//p')
    [ -z "$prof" ] && { echo "$(date '+%F %T') pane $p has no R_PROFILE_USER; left alone" >> "$LOG"; continue; }

    elf=$(tmux capture-pane -p -t "$TARGET.$p" -S -2500 2>/dev/null | grep -oE "outputs/[0-9]+\.[0-9.]*[0-9]" | tail -1)
    err=$(tmux capture-pane -p -t "$TARGET.$p" -S -80 2>/dev/null | tr -d '\n' | grep -oE "Error[^|]{0,110}" | head -1)
    echo "$(date '+%F %T') pane $p parked on ${elf:-?} -- respawning (#$((n+1))) | ${err:0:110}" >> "$LOG"

    ## respawn-pane replaces whatever runs in the pane -- R directly, or bash with R under it --
    ## with a fresh worker. Typing the command instead only works at a shell prompt: typed into
    ## an R-as-pane process it is just an R syntax error.
    tmux respawn-pane -k -c "$HOME/GitHub/FireSenseTesting" -t "$TARGET.$p" \
      "env SPADES_USE_REQUIRE=false FS_PHASE=${FS_PHASE:-1} R_TESTS= R_BROWSER= R_PDFVIEWER= R_DEFAULT_PACKAGES=datasets,utils,grDevices,graphics,stats,methods R_PROFILE_USER='$prof' R --quiet --no-save --no-restore --interactive"
    COUNT[$p]=$((n+1))
  done
  sleep "$INTERVAL"
done
