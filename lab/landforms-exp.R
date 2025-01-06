library(spatialist)
library(terra)
library(tidyterra)
library(ggplot2)

r <- rast(file.path("inst", "CS2.tif"))

#r <- rast(system.file("ex/test.grd", package="terra"))

# Draw rings
small <- draw_ring(5, 11)
large <- draw_ring(24, 101)

small_r <- focal(r, w = small, fun = mean)
small_r <- r - small_r

large_r <- focal(r, w = large, fun = mean)
large_r <- r - large_r

r2 <- c(r, small_r, large_r)
names(r2) <- c("elev", "tpi_s", "tpi_l")

ggplot() +
    geom_spatraster(data = r2) +
    facet_wrap(~lyr)

# For security, write Rasters
writeRaster(small_r, "lab/small_r.tif")
writeRaster(large_r, "lab/large_r.tif")



small <- small_r
large <- large_r

lf_classes[[1]] <- as.factor(lf_classes[[1]])


ggplot() +
    geom_spatraster(data = lf_classes)
