bufferOut <- function(v, spatRasSeg, spatRas, mask, field = "FRU") {

  if (missing(spatRasSeg) && missing(spatRas)) {
    tmpl <- terra::rast(v, res = 5000)
    spatRas <- {tmpl |>
        terra::rasterize(v, y = _, field = "FRU")} |> reproducible::Cache(omitArgs = "x")
    spatRasSeg <- terra::segregate(spatRas)
  }

  crsThis <- if (is(spatRasSeg, "list")) terra::crs(spatRasSeg[[1]]) else terra::crs(spatRasSeg)

  vCentred <- list()
  r <- list()
  ca <- list()
  if (!missing(v)) {
    vdf <- as.data.frame(v)
    layers <- sort(vdf[[field]])
  } else {
    layers <- names(spatRasSeg)
  }
  for (i in layers) {
    if (missing(v)) {
      tmpsrs <- spatRasSeg[[i]]
      tmpsrs[!tmpsrs[] > 0] <- NA
      nf <- as.polygons(tmpsrs)
      nf[[field]] <- names(tmpsrs)
    } else {
      nf <- v[vdf[[field]] %in% i, 1]
    }

    # center the individual polygon
    nfll <- terra::project(nf, "epsg:4943")
    exts <- round(terra::ext(nfll))[]
    middle <- mean(c(exts[["xmin"]],exts[["xmax"]]))
    prj <- paste0("+proj=lcc +lat_0=", exts[["ymin"]], " +lon_0=",middle," +lat_1=",exts[["ymax"]],
                  " +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs")
    vCentred[[i]] <- terra::project(nfll, prj)

    b41 <- vCentred[[i]]
    tmpl <- terra::rast(b41, res = 5000)
    tmpl1 <- terra::extend(tmpl, 20000/res(tmpl)[1])
    b41Orig <- terra::rasterize(b41, y = tmpl, field = field)
    b41Extended <- terra::rasterize(b41, y = tmpl1, field = field)
    b41ToUse <- b41Extended


    b41 <- b41ToUse
    if (is.factor(b41)) {
      cat <- cats(b41)[[1]]$ID
      b41[!b41[] %in% cat] <- NA
    } else {
      # b41[!b41[] > as.integer(i)] <- NA
    }
    # do buffer on the centred polygon
    b41 <- terra::buffer(b41, 80000)
    b41[b41[] %in% TRUE] <- NA
    b41 <- terra::buffer(b41, 60000)
    b41[] <- b41[] %in% FALSE
    b41[b41[] %in% FALSE] <- NA
    b41[] <- b41 + 0L # convert to numeric/integer
    b41[b41ToUse[] %in% as.integer(i)] <- 2L
    names(b41) <- i
    if (is.factor(b41Orig)) {
      cat <- cats(b41Orig)[[1]]$ID
      b41OrigNew <- resample(b41Orig, b41, method = "near")
      b41[b41OrigNew[] %in% cat & !is.na(b41OrigNew[])] <- 2
    } else {
      # b41[!b41Orig[] > as.integer(i)] <- NA
    }

    r[[i]] <- b41

    # Add it to the national map
    b41b <- terra::project(b41, crsThis, method = "near")
    b41b <- terra::resample(b41b, spatRasSeg[[i]], method = "near")
    ca[[i]] <- spatRasSeg[[i]]
    ca[[i]][b41b > 0] <- b41b
    if (missing(mask)) {
      mask <- spatRasSeg[[i]]
    }
    r[[i]] <- maskTo(r[[i]], mask, verbose = FALSE, touches = FALSE)
    ca[[i]] <- maskTo(ca[[i]], mask, verbose = FALSE, touches = FALSE)
    print(paste0("Done ", i))
  }
  list(rasCentered = r, rasWhole = ca)
}


segregateKeepNames <- function(ecopR, omitClasses, classes = NULL) {
  uniquVals <- unique(ecopR[]) |> na.omit()
  if (!missing(omitClasses)) {
    classes <- setdiff(uniquVals, omitClasses)
  }
  if (length(setdiff(uniquVals, 0)) > 1) {
    ecopRseg <- segregate(ecopR, classes = classes)
    if (is.factor(ecopR)) {
      names(ecopRseg) <- cats(ecopR)[[1]][match(as.numeric(names(ecopRseg)), cats(ecopR)[[1]]$ID), 2]
      dt <- data.table(nam = names(ecopRseg), num = as.numeric(names(ecopRseg)))
    } else {
      nam <- names(ecopR)
      namOrig <- names(ecopRseg)
      names(ecopRseg) <- paste0(nam, ".", names(ecopRseg))
      dt <- data.table(nam = names(ecopRseg), num = as.numeric(namOrig))
    }
    setorder(dt, "num")
    ecopR <- ecopRseg[[match(dt$nam, names(ecopRseg))]]
  }
  ecopR

}

split_poly <- function(sf_poly, n_areas) {
  # Create random points
  wasTerra <- FALSE
  if (is(sf_poly, "SpatVector")) {
    sf_poly <- sf::st_as_sf(sf_poly)
    wasTerra <- TRUE
  }
  points_rnd <- st_sample(sf_poly, size = 10000)
  # k-means clustering
  points <- do.call(rbind, st_geometry(points_rnd)) %>%
    as.data.frame() %>% setNames(c("lon","lat"))
  k_means <- kmeans(points, centers = n_areas)
  # Create voronoi polygons
  voronoi_polys <- dismo::voronoi(k_means$centers, ext = sf_poly)
  # Clip to sf_poly
  crs(voronoi_polys) <- crs(sf_poly)
  voronoi_sf <- st_as_sf(voronoi_polys)
  equal_areas <- st_intersection(voronoi_sf, sf_poly)
  equal_areas$area <- st_area(equal_areas)
  if (wasTerra)
    equal_areas <- terra::vect(equal_areas)
  return(equal_areas)
}

mergeAndSplitRas <- function(ecopRseg, ecopLCC, biggestWeWant = 2.4e+11,
                             field = "ECOPROVINC") { # FRU 27 ... or FRU 26 is 4.02075e+11
  Require::Require(c(sf, dismo))
  prov1 <- as.character(sort(as.numeric(unique(names(ecopRseg)))))
  out <- Map(prov = prov1, function(prov) {
    provChar <- as.character(prov)
    tmp <- ecopLCC[ecopLCC$ECOPROVINC == provChar, ]
    tmp[, "AREA"] <- terra::expanse(tmp)
    needUpdate <- FALSE
    # merge multiples into 1
    if (NROW(tmp) > 1) {
      tmp <- terra::hull(tmp)
      values(tmp) <- data.frame(AREA = terra::expanse(tmp))
      # tmp[, "AREA"] <- terra::expanse(tmp)
      needUpdate <- TRUE
      tmp[, field] <- paste0(provChar, seq_len(NROW(tmp)))
    }
    tooBig <- tmp$AREA > biggestWeWant
    # split too large into 2 or more
    if (any(tooBig)) {
      numAreas <- tmp$AREA/ biggestWeWant
      message("Splitting ", provChar, " in ", ceiling(numAreas))
      tmp <- split_poly(tmp, n_areas = ceiling(numAreas))
      tmp[, "AREA"] <- NULL
      needUpdate <- TRUE
      tmp[, field] <- tmp[, "id"]
    }
    if (needUpdate) {
      tmpRas <- try(rasterize(tmp, ecopRseg[[provChar]], field = field, touches = TRUE))
      whChange <- tmpRas[] > 0
      a <- ecopRseg[[provChar]]
      a[whChange] <- tmpRas[whChange]
      tmp <- a
    } else {
      tmp <- ecopRseg[[provChar]]
    }
    message("Done ", prov)
    tmp
  })
  out
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
    numInLayer <- sum(freq(stk[[i]])[-1,]$count)
    maxcellHere <- ifelse(numInLayer < 500, 5e5, maxcell)
    terra::plot(stk[[i]], maxnl = 1, legend = FALSE, #cex = 0.5,
                axes = FALSE, mar = mar, main = "", maxcell=maxcellHere)
    mtext(side = 3, line = -2, names(stk)[i])
  }
}
