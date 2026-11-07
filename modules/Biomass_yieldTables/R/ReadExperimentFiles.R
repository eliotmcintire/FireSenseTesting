ReadExperimentFiles <- function(factorialOutputs) {
  factorialOutputs <- as.data.table(factorialOutputs)[objectName == "cohortData"]
  fEs <- .fileExtensions()
  cdsList <- by(factorialOutputs, factorialOutputs[, "saveTime"], function(x) {
    fE <- reproducible:::fileExt(x$file)
    wh <- fEs[fEs$exts %in% fE,]
    message(crayon::green("reading: "))
    cat(crayon::green(x$file, "..."))
    cd <- getFromNamespace(wh$fun, ns = asNamespace(wh$package))(x$file)[, .(speciesCode, age, B, pixelGroup)]
    cat(crayon::green(" Done!\n"))
    return(cd)
  })
  gc() # need to clear memory
  message("rbindlisting the cohortData objects")
  cds <- rbindlist(cdsList, use.names = TRUE, fill = TRUE)
  setnames(cds, c("pixelGroup", "B"), c("yieldTableIndex", "biomass"))
  setkeyv(cds, c("speciesCode", "yieldTableIndex"))
  return(invisible(cds))
}