climateLayers <- function(.climVars = "CMD_sm", historical = TRUE, projected = TRUE,
                          fun = quote(calcAsIs)) {
  hps <- c()
  if (isTRUE(historical))
    hps <- c(historical = "historical")
  if (isTRUE(projected))
    hps <- c(hps, future = "projected")

  rr <- Map(hp = unname(hps), nam = names(hps), function(hp, nam) {
    Map(cv = .climVars, function(cv) {
      ll <- list(vars = paste0(nam, "_", cv),
                 fun = fun)
      .dots = if (nam == "historical")
        list(1991:2022)
      else
        list(2011:2100)
      ll <- append(ll, list(.dots = .dots |> setNames(paste0(nam, "_years"))))
    })
  }) |> unlist(recursive = FALSE)

  names(rr) <- gsub("_", "", names(rr)) # removes _ in e.g., CMD_sm
  names(rr) <- gsub("\\.", "_", names(rr)) # converts remaining . to a _ in e.g., projected_

  rr
}

plotStack <- function(stk, omi = c(0,0,0,0), mai = c(0,0,0,0), mar = c(0,0,0,0),
                      maxcell = 50000) {
  ds <- dev.size()
  numLayers <- if (is(stk, "list")) length(stk) else nlyr(stk)
  # (xmax(stk) - xmin(stk))/(ymax(stk) - ymin(stk))
  wid <- floor(ds[1] / (prod(ds) / numLayers / 1))
  nlr <- numLayers
  hei <- (ceiling(nlr/wid))
  parOrig <- par(mfrow = c(hei, wid), omi = omi, mai = mai)
  on.exit(par(parOrig))
  for (i in seq(numLayers)) {
    numInLayer <- sum(terra::freq(stk[[i]])[-1,]$count)
    maxcellHere <- ifelse(numInLayer < 500, 5e5, maxcell)
    terra::plot(stk[[i]], maxnl = 1, legend = FALSE, #cex = 0.5,
                axes = FALSE, mar = mar, main = "", maxcell=maxcellHere)
    mtext(side = 3, line = -2, names(stk)[i])
  }
}

