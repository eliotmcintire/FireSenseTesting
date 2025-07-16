.ELFinds <- c("4.1", "4.2.1", "4.2.2", "4.3", "5.1.1", "5.1.2", "5.1.3",
              "5.2.1", "5.2.2", "5.3.1", "5.3.2", "5.4", "6.1.1", "6.1.2",
              "6.1.3", "6.2.1", "6.2.2", "6.2.3", "6.3.1", "6.3.2", "6.4",
              "6.5", "6.6.1", "6.6.2", "7.1", "7.2", "7.3", "9.1.1", "9.1.2",
              "9.2.1", "9.2.2", "9.2.3", "9.3", "11.1", "11.2", "11.3", "11.4",
              "12.1", "12.2", "12.3", "12.4", "13.1", "13.2.1", "13.2.2", "13.3",
              "14.1", "14.2", "14.3", "14.4", "15.1", "15.2.1", "15.2.2")
# Comment this out to run just one ELF at a time
.ELFinds <- c("5.1.3");

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
  expt$.coreListIndex = expt$iter %% 2 + 1

  # mm1 <- mirai::daemons(1, .compute = ".plots", dispatcher = FALSE)
  if (!mirai::daemons_set("SpaDES"))
    mm2 <- mirai::daemons(2, .compute = "SpaDES", dispatcher = FALSE)
  # mirai::daemons(0, .compute = "SpaDES")


  rr <- mirai::mirai_map(.x = expt, .compute = "SpaDES", {
    .f = function(iter, .rep, .ELFind) {
      # zz <- file("globalLog.txt", open = "wt")
      # sink(zz, append = TRUE, type = "message")
      try(source("global.R", local = TRUE))
      # sink()

    }
  })
}
