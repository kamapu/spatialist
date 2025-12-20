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
writeRaster(small_r, "lab/small_r.tif", overwrite = TRUE)
writeRaster(large_r, "lab/large_r.tif", overwrite = TRUE)









small <- small_r
large <- large_r

lf_classes[[1]] <- as.factor(lf_classes[[1]])


ggplot() +
    geom_spatraster(data = lf_classes)





# Load packages
library(elevatr)
library(sf)
library(terra)
library(ggplot2)
library(tidyterra)
library(magick)

# create an sf polygon for the bbox (CRS WGS84)
bbox <- st_as_sfc(st_bbox(c(
    xmin = 7.18,
    ymin = 50.64,
    xmax = 7.3,
    ymax = 50.70
  ), crs = st_crs(4326)))
bbox <- st_sf(geometry = bbox)

# z = zoom level / resolution control (higher = finer). Typical values 9-14.
dem_raster <- get_elev_raster(locations = bbox, z = 14, clip = "locations")