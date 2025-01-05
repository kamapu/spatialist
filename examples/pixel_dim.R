## Determine resolution
r <- terra::rast(system.file("ex/elev.tif", package="terra"))
pixel_dim(r)

## More appropriate for projected coordinates
pixel_dim(r, projected = TRUE)
