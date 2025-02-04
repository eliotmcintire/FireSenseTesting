.rep <- 6
for (.c in c(0.1, 0.5)) {
  for (.strategy in c(3L, 6L)) {
    .rep <- .rep + 1
    source("global.R")
  }
}
