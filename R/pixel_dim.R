#' @name pixel_dim
#' @rdname pixel_dim
#'
#' @title Estimating metric dimensions of pixels in raster object
#'
#' @description
#' Pixel dimension in raster objects (class `Raster*`) in projected space.
#'
#' For non projected rasters, the bounding boxes will be reprojected to UTM
#' coordinates for the calculation of height (latitude) and width (longitude).
#' For projected rasters, the calculation will be directly done in the units of
#' the respective spacial reference system.
#'
#' @param raster An object of class `Raster*`.
#' @param projected Logical value indicating whether the coordinates of raster
#'     input are projected or not.
#' @param ... Further arguments passed among methods (Not yet used).
#'
#' @return A named vector with the estimated height (latitude) and width
#' (longitude) of pixels either in meters for non-projected rasters or in the
#' own spacial units for projected ones.
#'
#' @author Lukas Trübenbach and Miguel Alvarez (\email{kamapu78@@gmail.com}).
#'
#' @example examples/pixel_dim.R
#'
#' @export
pixel_dim <- function(raster, ...) {
  UseMethod("pixel_dim", raster)
}

#' @rdname pixel_dim
#' @aliases pixel_dim,SpatRaster-method
#' @method pixel_dim SpatRaster
#' @export
pixel_dim.SpatRaster <- function(raster, projected = FALSE, ...) {
  extent <- ext(raster)
  r_crs <- crs(raster)
  if (!projected) {
    extent2 <- project(extent, from = r_crs, to = "epsg:4326")
    centr <- c(mean(extent2[1:2]), mean(extent2[3:4]))
    # UTM conversion
    zones <- cbind(c(1:60), seq(-177, 177, length.out = 60))
    zone <- zones[order(abs(zones[, 2] - centr[1]))[1], 1]
    if (centr[2] >= 0) hem <- "north" else hem <- "south"
    extent <- project(extent,
      from = r_crs,
      to = paste0("+proj=utm +zone=", zone, " +", hem, "
						+datum=WGS84")
    )
  }
  # euclidean distance
  OUT <- c(
    mean_height = mean(c(
      dist(extent[c(2, 1), ]),
      dist(extent[c(4, 3), ])
    )),
    mean_width = mean(c(
      dist(extent[c(1, 4), ]),
      dist(extent[c(2, 3), ])
    ))
  )
  OUT <- OUT / dim(raster)[1:2]
  return(OUT)
}
