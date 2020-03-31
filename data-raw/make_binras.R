# TODO:   Produce a binary data set from a matrix
# 
# Author: Miguel Alvarez
################################################################################

library(raster)

# Read matrix
r <- as.matrix(read.csv("data-raw/binras.csv", header=FALSE))
r[r == 0] <- NA

# Convert to rasterLayer
r <- raster(r)
plot(r)

# Save
writeRaster(r, "inst/binras.tif", overwrite=TRUE)
