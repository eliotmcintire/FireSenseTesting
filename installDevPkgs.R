
lib_path <- .libPaths()[[1]]

# --- Local install ---
lapply(devPkgs, pak::pak, lib = lib_path, ask = FALSE, upgrade = FALSE, dependencies = FALSE)
# pak::pak(devPkgs, lib = lib_path, ask = FALSE, upgrade = FALSE)

# --- Cluster install ---
library(parallelly)
library(parallel)

machines <- c("birds", "biomass", "camas", "carbon", "caribou", "coco",
              "core", "dougfir", "fire", "mpb", "sbw",
              "acer", "abies", "pinus")

workers <- makeClusterPSOCK(machines, user = "emcintir", homogeneous = TRUE)

results <- tryCatch({
  clusterExport(workers, c("devPkgs", "lib_path"))
  parLapply(workers, seq_along(workers), function(i) {
    tryCatch({
      pak::pak(devPkgs, lib = lib_path, ask = FALSE, upgrade = FALSE, dependencies = FALSE)
      "SUCCESS"
    }, error = function(e) {
      paste("FAILED:", conditionMessage(e))
    })
  })
}, finally = stopCluster(workers))

names(results) <- machines
print(results)