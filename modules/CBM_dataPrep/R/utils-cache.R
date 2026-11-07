
masterRasterDigest <- function(sim){
  if (is.null(sim$masterRasterDigest)){
    list(
      crs = terra::crs(sim$masterRaster) |> as.character(),
      res = terra::res(sim$masterRaster) |> as.numeric(),
      ext = terra::ext(sim$masterRaster) |> as.list()
    ) |> lapply(digest::digest)
  }else sim$masterRasterDigest
}
