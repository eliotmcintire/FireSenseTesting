## What differs per machine. global.R and expt.R read it from the project folder, as the workers and the IDE run them.
## Returns a list: basePath (inputs, outputs, cache, scratch and shared data), cores (the machines a spread fit
## may use), maxWorkersPerHost, mcCores (per-process cap on climate mosaics, fire buffers and threshold forks;
## otherwise each takes the whole machine).
user <- Sys.info()[["user"]]

list(
  basePath = if (user == "elm001") "/gpfs/fs7/nrcan/nrcan_forsite/elm001/firesense/fast" # GPSC: no /mnt/fast; GPFS, which the compute nodes see
             else "/mnt/fast",
  cores = c("birds", "camas", "caribou", "coco", "core", "dougfir", "fire", "mpb", "sbw", "acer", "abies",
            "pinus", "landr", "kodama", "mega", "biomass", # mega added 2026-10-20; biomass added 2026-11-06 (Eliot)
            "carbon"), # carbon: lent 2026-11-06 to 11-09; back in the pool 2026-11-20 (Eliot)
  maxWorkersPerHost = c(mega = 60,  # mega is also the master; 60 (Eliot 2026-11-01; was 55 since 2026-10-21)
                        dougfir = 40), # sdas works there interactively (RStudio, VS Code); 8 of 48 threads left for them (Eliot 2026-11-20)
  mcCores = 8
)
