#!/bin/bash
## One-screen view of the run. Ctrl-C to stop.
cd /home/emcintir/GitHub/FireSenseTesting
while true; do
  clear
  echo "=== FireSense run  $(date '+%H:%M:%S') ==============================="
  Rscript -e 'q <- as.data.frame(readRDS("experiment_queue_fits_2026-09-08.rds"))
    print(table(q$status))
    r <- q[q$status == "RUNNING", c(".ELFind","process_id","started_at")]
    if (nrow(r)) { r$mins <- round(as.numeric(difftime(Sys.time(), as.POSIXct(r$started_at), units="mins")))
      print(r[order(r$started_at), c(".ELFind","mins")], row.names = FALSE) }' 2>/dev/null | grep -vE "libPaths|setupOff"
  echo
  free -g | awk 'NR==2{printf "mem  %s GB free of %s\n", $7, $2}'
  df -h /mnt/fast | awk 'NR==2{printf "disk %s free (%s used)\n", $4, $5}'
  printf "workers busy: %s\n" "$(tmux list-panes -t fits:1 -F '#{pane_current_command}' 2>/dev/null | grep -c '^R$')"
  sleep 30
done
