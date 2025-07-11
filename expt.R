.ELFinds <- c("ELF4.3", "ELF4.2");
.reps <- 1:2;

# If you can run them in parallel on the same linux machine:
expt <- expand.grid(.rep = .reps, .ELFind = .ELFinds)
expt <- data.frame(iter = seq_len(NROW(expt)), expt)


mm <- mirai::daemons(4, dispatcher = FALSE)


rr <- mirai::mirai_map(.x = expt, {
  .f = function(iter, .rep, .ELFind) {


    source("global.R", local = TRUE)


  }
}
)


