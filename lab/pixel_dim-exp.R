library(terra)
library(tidyterra)
library(ggplot2)

f <- system.file("ex/elev.tif", package="terra")
r <- rast(f)

ggplot() +
    geom_spatraster(data = r)

raster <- r
extent <- ext(raster)
