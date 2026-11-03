## What differs per machine. global_draft.R and expt_draft.R read it from the project folder, as the workers and the IDE run them.
## Returns a list: basePath (inputs, outputs, cache, scratch and shared data), cores (the machines a spread fit
## may use), maxWorkersPerHost, mcCores (per-process cap on climate mosaics, fire buffers and threshold forks;
## otherwise each takes the whole machine).
user <- Sys.info()[["user"]]

list(
  basePath = if (user == "elm001") "/gpfs/fs7/nrcan/nrcan_forsite/elm001/firesense/fast" # GPSC: no /mnt/fast; GPFS, which the compute nodes see
             else "/mnt/fast",
  cores = c("birds", "camas", "caribou", "coco", "core", "dougfir", "fire", "mpb", "sbw", "acer", "abies",
            "pinus", "landr", "kodama", "mega"), # mega added 2026-10-20 as phase 1 empties; biomass stays out; carbon (lent 2026-10-23) returned 2026-10-25
  maxWorkersPerHost = c(mega = 60), # mega is also the master; 60 (Eliot 2026-11-01; was 55 since 2026-10-21)
  mcCores = 8
)
