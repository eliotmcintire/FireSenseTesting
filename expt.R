# .cs <- c(0.5)
# .strategies <- c(3L)
# .rep <- 12
# for (.c in .cs) {
#   for (.strategy in .strategies) {
#     for (.objfunFireReps in objfunFireReps) {
#       if (.rep > 10)
#         source("global.R", local = TRUE)
#       .rep <- .rep + 1
#     }
#   }
# }

iter <- 1
expt <- read.csv("expt.csv")

expt$.cores <- list(c(rep("localhost", 40), rep("n174", 30), rep("n54", 10), rep("n14", 10), rep("n105", 10)),
               paste0("n", rep(sort(c(181, 171, 42)), each = 33)),
               paste0("n", rep(sort(c(18, 161, 168)), each = 33)))#, 179,



# data.frame(.cs = 0.5)
# If you can run them in parallel on the same linux machine:
# expt <- expand.grid(.c = .cs, .strategy = .strategies)
# expt <- data.frame(.rep = seq_len(NROW(expt)), expt)
lala <- mclapply(mc.cores = NROW(expt),
  seq_along(expt$.rep),
  FUN = function(iter) {
    .c <- expt$.c[iter]
    .rep <- expt$.rep[iter]
    .strategy <- expt$.strategy[iter]
    .objfunFireReps <- expt$.objfunFireReps[iter]
    .cores <- expt$.cores[iter]
    source("global.R", local = TRUE)
  })
