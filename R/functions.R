bufferOut <- function(v, spatRasSeg, spatRas, mask, field = "FRU", desiredBuffer = 20000) {

  if (missing(spatRasSeg) && missing(spatRas)) {
    tmpl <- terra::rast(v, res = 5000)
    spatRas <- {tmpl |>
        terra::rasterize(v, y = _, field = "FRU", touches = TRUE)} |>
      reproducible::Cache(omitArgs = "x")
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

  lostPixels <- list()
  for (i in layers) {
    if (missing(v)) {
      tmpsrs <- spatRasSeg[[i]]
      tmpsrs[!tmpsrs[] > 0] <- NA
      nf <- terra::as.polygons(tmpsrs)
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
    largestBuffer <- 8e4
    tmpl1 <- terra::extend(tmpl, largestBuffer/terra::res(tmpl)[1])
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

    b41 <- terra::buffer(b41, largestBuffer) # goes well past the edge of the map
    b41[b41[] %in% TRUE] <- NA
    b41 <- terra::buffer(b41, largestBuffer - desiredBuffer)
    b41[] <- b41[] %in% FALSE
    b41[b41[] %in% FALSE] <- NA
    b41[] <- b41 + 0L # convert to numeric/integer
    # b41[b41ToUse[] %in% as.integer(i)] <- 2L
    b41[!is.na(b41ToUse[])] <- 2L
    b41 <- terra::trim(b41)

    names(b41) <- i
    if (is.factor(b41Orig)) {
      cat <- cats(b41Orig)[[1]]$ID
      b41OrigNew <- terra::resample(b41Orig, b41, method = "near")
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


    # need to mask 2x maybe
    r[[i]] <- maskTo(r[[i]], mask, verbose = FALSE, touches = FALSE)
    ca[[i]] <- maskTo(ca[[i]], mask, verbose = FALSE, touches = FALSE)

    patchs <- terra::patches(r[[i]], allowGaps = FALSE, values = FALSE, directions = 8)
    tab <- terra::freq(patchs)

    if (NROW(tab) > 1) {
      tooSmall <- tab$count < 500
      if (any(tooSmall)) {

        r[[i]][patchs[] %in% tab$value[tooSmall]] <- NA
        r[[i]] <- terra::trim(r[[i]])
        a <- ca[[i]]
        a[ca[[i]] == 0] <- NA
        patchsCA <- terra::patches(a, allowGaps = FALSE, values = FALSE, directions = 8)
        # patchsCA <- terra::patches(ca[[i]] > 0, allowGaps = FALSE, values = FALSE, directions = 8)
        tab <- terra::freq(patchsCA)
        tooSmall <- tab$count < 500
        wh <- which(patchsCA[] %in% tab$value[tooSmall])
        dtLost <- data.table(pixelID = wh, value = terra::values(ca[[i]])[wh])

        lostPixels[[i]] <- dtLost
        ca[[i]][wh] <- 0
      }
    }
    # r[[i]] <- maskTo(r[[i]], mask, verbose = FALSE, touches = FALSE)
    # ca[[i]] <- maskTo(ca[[i]], mask, verbose = FALSE, touches = FALSE)

    print(paste0("Done ", i))
  }

  ll <- moveSliversToOtherELFs(lostPixels, lp, ca, i, r)

  list(rasCentered = ll$r, rasWhole = ll$ca)
}

segregateKeepNames <- function(ecopR, omitClasses, classes = NULL) {
  uniquVals <- unique(ecopR[]) |> na.omit()
  if (!missing(omitClasses)) {
    classes <- setdiff(uniquVals, omitClasses)
  }
  if (length(setdiff(uniquVals, 0)) > 1) {
    ecopRseg <- terra::segregate(ecopR, classes = classes)
    if (is.factor(ecopR)) {
      names(ecopRseg) <- cats(ecopR)[[1]][match(as.numeric(names(ecopRseg)), cats(ecopR)[[1]]$ID), 2]
      dt <- data.table(nam = names(ecopRseg), num = as.numeric(names(ecopRseg)))
    } else {
      nam <- names(ecopR)
      namOrig <- names(ecopRseg)
      names(ecopRseg) <- paste0(nam, ".", names(ecopRseg))
      dt <- data.table(nam = names(ecopRseg), num = as.numeric(namOrig))
    }
    data.table::setorder(dt, "num")
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
      terra::values(tmp) <- data.frame(AREA = terra::expanse(tmp))
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
      tmpRas <- try(terra::rasterize(tmp, ecopRseg[[provChar]], field = field, touches = TRUE))
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
    numInLayer <- sum(terra::freq(stk[[i]])[-1,]$count)
    maxcellHere <- ifelse(numInLayer < 500, 5e5, maxcell)
    terra::plot(stk[[i]], maxnl = 1, legend = FALSE, #cex = 0.5,
                axes = FALSE, mar = mar, main = "", maxcell=maxcellHere)
    mtext(side = 3, line = -2, names(stk)[i])
  }
}

makeELFs <- function(nationalForestPolygon, desiredBuffer = 20000, inputPath) {
  digNFP <- reproducible::.robustDigest(nationalForestPolygon)
  dv <- terra::vect(nationalForestPolygon) |>
    reproducible::Cache(omitArgs = "x", .cacheExtra = digNFP)
  ecoNames <- c("zone", "region", "province", "district")
  makeEcoURLs <- function(ecoNames) {
    vapply(ecoNames, FUN.VALUE = character(1), function(en)
      paste0(paste0("https://sis.agr.gc.ca/cansis/nsdb/ecostrat/", en, "/eco", en, "_shp.zip")))
  }
  urls <- makeEcoURLs(ecoNames)
  ecosLCC <- Map(nam = ecoNames, url = urls, function(nam, url) {
    reproducible::prepInputs(url = url,
                             destinationPath = inputPath,
                             projectTo = nationalForestPolygon) |>
      reproducible::Cache(omitArgs = "projectTo",
                          .cacheExtra = list(nationalForestPolygon = digNFP))
  })
  tmpl <- terra::rast(ecosLCC[[1]], res = 5000)
  hf <- terra::rasterize(nationalForestPolygon, tmpl, field = "FRU") |>
    reproducible::Cache(omitArgs = "x", .cacheExtra = digNFP)
  ecosR <- Map(ecoLCC = ecosLCC, nam = names(ecosLCC), function(ecoLCC, nam)
  {
    terra::rasterize(ecoLCC, tmpl, field = toupper(paste0("eco", substr(nam, 1, 7)))) |>
      postProcess(maskTo = hf)
  } |>
    reproducible::Cache(omitArgs = "maskTo",
                        .cacheExtra = list(hf = attr(hf, "tags"))))

  ecopR <- ecosR$province
  # Some are on the edge, e.g., in the tundra --> remove if less than 100 pixels
  fre <- terra::freq(ecopR)
  fre <- fre[fre$count > 100, ]
  ecopRseg <- terra::segregate(ecopR)
  categories <- terra::cats(ecopR)[[1]]
  names(ecopRseg) <- categories[match(names(ecopRseg), categories$ID), "ECOPROVINC"]
  ord <- order(as.numeric(intersect(categories$ECOPROVINC, fre$value)))
  ecopRseg <- ecopRseg[[names(ecopRseg) %in% fre$value]][[ord]]

  # plotStack(ecopRseg)
  out <- bufferOut(dv, desiredBuffer = desiredBuffer) |>
    reproducible::Cache(omitArgs = "v", .cacheExtra = digNFP)
  out2 <- mergeAndSplitRas(ecopRseg, ecosLCC$province) |> Cache()

  out3 <- lapply(out2, function(x) as.list(segregateKeepNames(x, omitClasses = 0))) |>
    unlist(recursive = FALSE)
  names(out3) <- sapply(out3, names)

  ELFs <- bufferOut(spatRasSeg = out3, mask = out$rasWhole[[1]],
                    desiredBuffer = desiredBuffer) |>
    reproducible::Cache(omitArgs = c("spatRasSeg", "mask"),
                        .cacheExtra = list(nationalForestPolygon = digNFP,
                                           attr(out2, "tags"),
                                           attr(out, "tags")
                        ))

  ELFs
}




moveSliversToOtherELFs <- function(lostPixels, lp, ca, i, r) {

  if (exists("aaaa", envir = .GlobalEnv)) browser()
  if (NROW(unlist(lostPixels))) {
    if (is.null(names(lostPixels))) {
      hasNames <- FALSE
      haveLostPixels <- which(lengths(lostPixels) > 0)
    } else {
      hasNames <- TRUE
      haveLostPixels <- names(lostPixels)
    }
    for (lp in haveLostPixels) {
      numOverlap <- list()
      for (i in seq(ca)) {
        whPixelIDsHere <- which(ca[[i]][] > 0)
        lostPixelsElsewhere2 <- lostPixels[[lp]][value == 2]$pixelID %in% whPixelIDsHere
        lostPixelsElsewhere1 <- lostPixels[[lp]][value == 1]$pixelID %in% whPixelIDsHere
        if (any(lostPixelsElsewhere2)) {
          numOverlap[[i]] <- sum(lostPixelsElsewhere2)
          print(paste(i, ": ", sum(lostPixelsElsewhere2), "pixels"))
        }
      }

      # If the pixels that were lost do not show up in any other ELF blob, they are lost forever
      #   In the examples I saw, it was tiny islands off the coast of Newfoundland
      if (NROW(numOverlap)) {
        if (exists("aaaa", envir = .GlobalEnv)) browser()
        addTo <- which(sapply(numOverlap, function(x) !is.null(x)))
        if (hasNames) {
          addTo <- names(ca)[addTo]
        }

        a <- list()
        sums <- list()
        for (addToInd in addTo) {
          curPixelVal <- ca[[addToInd]][] != 2
          a[[addToInd]] <- ca[[addToInd]]
          a[[addToInd]][lostPixels[[lp]]$pixelID] <- pmax(terra::values(a[[addToInd]])[lostPixels[[lp]]$pixelID], lostPixels[[lp]]$value, na.rm = TRUE)
          theA <- terra::freq(a[[addToInd]])
          # theA <- lapply(a, function(x) if (!is.null(x)) terra::freq(x))
          sums[[addToInd]] <- sum(theA[-1,] - terra::freq(ca[[addToInd]])[-1,])
        }
        addTo <- which.min(unlist(lapply(sums, function(ss) if (is.null(ss)) 1e7 else ss )))
        if (hasNames)
          addTo <- names(addTo)
        newVals <- pmax(terra::values(ca[[addTo]])[lostPixels[[lp]]$pixelID], lostPixels[[lp]]$value, na.rm = TRUE)
        ca[[addTo]][lostPixels[[lp]]$pixelID] <- newVals

        # dealt with the lp-th element in lostPixels; remove it: next line doesn't work. It shrinks the list.
        # lostPixels[[lp]] <- NULL
        a <- ca[[addTo]]
        a[] <- NA
        a[lostPixels[[lp]]$pixelID] <- newVals
        # a[a[] == 0] <- NA
        a <- terra::trim(a)
        a <- terra::project(a, terra::crs(r[[addTo]]), method = "near")
        bb <- terra::resample(a, r[[addTo]], method = "near")
        whVals <- which(terra::values(bb) > 0)
        r[[addTo]][whVals] <- terra::values(bb)[whVals]
      } else {
        numLostPixelsForever <- try(sum(lostPixels[[lp]]$value))
        if (is(numLostPixelsForever, "try-error")) browser()
        if (is.null(names(ca))) {
          message("From ELF ", lp, ", lost ", numLostPixelsForever," isolated pixels that do not exist in another ELF")
        }
      }
      if (exists("aaaa", envir = .GlobalEnv)) browser()
      nam <- if (is.null(names(lostPixels))) seq(lostPixels) else names(lostPixels)
      lostPixels <- Map(na = nam, function(na) if (na == lp) NULL else lostPixels[[na]])
    }
  }
  list(ca = ca, r = r)
}


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
