## Load installed rasterLayer
require(terra)
require(ggplot2)
require(tidyterra)

# Import and prepare data
r <- rast(file.path(path.package("spatialist"), "binras.tif"))
r[is.na(r)] <- 0
r <- as.factor(r)
coltab(r) <- data.frame(value = c(0, 1), col = c("white", "black"))

## Make only erosion or only dilation
r2 <- rast(c(
    original = r,
    eroded = erodil_raster(r, dilation = FALSE),
    dilated = erodil_raster(r, erosion = FALSE)
))

ggplot() + geom_spatraster(data = r2) + facet_wrap(~lyr)

## Erode two times
r2 <- rast(c(
    original = r,
    dilated = erodil_raster(r, erosion = FALSE),
    dilated2 = erodil_raster(r, erosion = FALSE, nt = 2)
))

ggplot() + geom_spatraster(data = r2) + facet_wrap(~lyr)

## By default erosion will be done before dilation
r2 <- rast(c(
  original = r,
  eroded_first = erodil_raster(r),
  dilated_first = erodil_raster(r, erosion_first = FALSE),
  both = erodil_raster(erodil_raster(r, erosion_first = FALSE))
))

ggplot() + geom_spatraster(data = r2) + facet_wrap(~lyr)
