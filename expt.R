# .run <- 2 # 2 is right
# Comment this out to run just one ELF at a time
# .ELFinds <- c("6.3.1")#, "6.2.2");
# DONE
.ELFinds <- c("4.3")
.ELFinds <- c("6.1.1")
.ELFinds <- c("6.1.2")
.ELFinds <- c("6.1.3")
.ELFinds <- c("6.2.1")
.ELFinds <- c("6.2.2")
.ELFinds <- c("6.2.3")
.ELFinds <- c("4.1")
.ELFinds <- c("4.2.1")
.ELFinds <- c("5.2.1")
# DONE above here

# biomass x 8, birds x 8, camas x 7, carbon x 8, caribou x 8, coco x 8, core x 7,
# dougfir x 7, fire x 7, localhost x 12, mpb x 7, n14 x 3, n54 x 3, sbw x 7
.ELFinds <- c(#"4.1", "4.2.1", "4.2.2", "4.3", "5.1.1", "5.1.2", "5.1.3",
  #"5.2.1", "5.2.2", "5.3.1", "5.3.2", "5.4", "6.1.1", "6.1.2",
  #"6.1.3", "6.2.1", "6.2.2", "6.2.3", "6.3.1", "6.3.2", "6.4", "6.5",
  # "6.6.1",  # interactive see below # still need to do
  # "6.6.2", "7.1", "7.2", "7.3", "9.1.1", "9.1.2", "9.2.1", "9.2.2",
  "9.2.3", # running on left
  "9.3", # running on right
  "11.1",
  "11.2", "11.3", "11.4",
  "12.1", # "12.2",
  "12.3", "12.4", "13.1", "13.2.1", "13.2.2", "13.3",
  "14.1", "14.2", "14.3", "14.4", "15.1", "15.2.1", "15.2.2")

# if (isTRUE("tools:rstudio" %in% search())) .ELFinds <- "7.3"
# .ELFinds <- grep("^4\\.", .ELFinds, value = TRUE, invert = TRUE)
# .ELFinds <- grep("^5\\.", .ELFinds, value = TRUE, invert = TRUE)
# .ELFinds <- grep("^6\\.1\\.", .ELFinds, value = TRUE, invert = TRUE)
# .ELFinds <- grep("^6\\.2\\.", .ELFinds, value = TRUE, invert = TRUE)
# .ELFinds <- grep("^6\\.3", .ELFinds, value = TRUE, invert = TRUE)
# .ELFinds <- c("6.2.2")# OK for canClimateData from clean folders
# .ELFinds <- grep("^5\\.", .ELFinds, value = TRUE, invert = FALSE)
# .ELFinds <- c("5.1.1")# Done SpreadFit
#
# # .ELFinds <- c("6.1.1") # top left # fail:
# # Aug09 15:33:13 fireSense   :burn total elpsd: 11 mins | 2011 fireSense burn 5.13
# # Error in `[<-.bit`(`*tmp*`, successCells[!potentialNotAvailable], value = TRUE) :
# #   NAs are not allowed in subscripted assignments
#
# # .ELFinds <- c("6.1.3") # Rstudio server
#
# # .ELFinds <- c("14.1") #
# .ELFinds <- c("5.1.1") #  error in climate layers -- CMDsm has chunk in all years
# .ELFinds <- c("5.1.2") # error
# .ELFinds <- c("5.2.2") # error

# .ELFinds <- c("5.3.1") # error
# .ELFinds <- c("5.3.2") # error
# .ELFinds <- c("6.3.1") # topright
# .ELFinds <- c("6.3.2") # topleft


if (FALSE) {
  # Known fails:
  #  .ELFind <- "4.2.1" # Elements 3661396 of is.na(as.vector(sim$ecoregionMap[])) == is.na(as.vector(sim$pixelGroupMap[])) are not true
  # .ELFinds <- names(inSim$ELFs$rasCentered)
  .reps <- 1;

  if (length(.ELFinds) == 1) {
    .ELFind <- .ELFinds[1]
    .rep <- .reps[1]
    source("global.R", local = TRUE)
  } else {
    # If you can run them in parallel on the same linux machine:
    expt <- expand.grid(.rep = .reps, .ELFind = .ELFinds)
    expt <- data.frame(iter = seq_len(NROW(expt)), expt)
    # expt$.coreListIndex = expt$iter %% 2 + 1

    Require::Require(c("mirai", "future.mirai", "future", "future.apply"))
    runFit <- TRUE
    if (runFit) {
      # mm1 <- mirai::daemons(1, .compute = ".plots", dispatcher = FALSE)
      if (!mirai::daemons_set()) {
        # mm2 <- mirai::daemons(2, .compute = "SpaDES", dispatcher = FALSE)
        mm1 <- daemons(url = host_url(), dispatcher = TRUE)
        Sys.sleep(0.5)
        mm2 <- mirai::launch_local(2)
      }
      # mirai::daemons(0, .compute = "SpaDES")
      plan(mirai_cluster)
    } else {
      # furrr::future_map(1:10, function(x) Sys.getpid())
      plan(sequential)
    }

    rr <- furrr::future_pmap(.progress = TRUE, # seed = TRUE, # earlySignal = TRUE,
                             expt, # .compute = "SpaDES",
                             # INDICES = seq_len(NROW(expt)),
                             # rr <- furrr::future_pmap(.l = expt, # .compute = "SpaDES",
                             function(iter, .rep, .ELFind) {
                               runFit <- FALSE
                               # print(.ELFind)
                               Require::Install("XML")
                               # zz <- file("globalLog.txt", open = "wt")
                               # sink(zz, append = TRUE, type = "message")
                               try(source("global.R", local = TRUE))
                               # sink()

                             }
    )
  }
} else {
  .ELFindsList <- split(.ELFinds, rep(1:2, length.out = length(.ELFinds)))
  .ELFinds <- .ELFindsList[.run]
  for (.ELFind in .ELFinds[[1]]) {
    .rep <- 1;
    runFit <- TRUE
    message(paste0(.ELFind, ", .rep:", .rep))
    try(source("global.R", local = TRUE))
  }
}

