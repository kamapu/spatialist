library(mmand)
library(terra)
library(ggplot2)
library(tidyterra)

r <- raster <- rast(file.path("inst", "binras.tif"))
width = c(3, 3)
type = "diamond"
erosion = TRUE
dilation = TRUE
erosion_first = TRUE
nt = 1


ggplot() + geom_spatraster(data = raster)


?dilation

x <- c(0,0,1,0,0,0,1,1,1,0,0)
k <- c(1,1,1)
erode(x,k)
dilate(x,k)

ra <- as.array(r)
ra

ra[is.nan(ra)] <- 0
ra

ra <- as(r, "array")
k <- shapeKernel(width = c(3, 3), type = "diamond")

ra
dilate(ra, k)
erode(ra, k)

ra


r[is.nan(r)] <- 0
r

writeRaster(r, "inst/binrast.tif", overwrite = TRUE)


