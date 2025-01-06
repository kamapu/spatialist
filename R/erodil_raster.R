#' @name erodil_raster
#' @rdname erodil_raster
#'
#' @title Raster erosion and dilation
#'
#' @description
#' Mixing erosion and dilation processes to clean binary images, getting rid of
#' isolated pixels and little holes.
#'
#' For the single steps, see [erode()].
#'
#' @param raster An object of class [SpatRaster-class] containing 1 and 0
#'     (or NAs) values.
#' @param width An integer vector indicating the width and the heigth applied
#'     for the kernel. This is passed to [shapeKernel()].
#' @param type A character value indicating the type of shape used for the
#'     kernel (see [shapeKernel()])
#' @param erosion A logical value indicating whether the data will be eroded or
#'     not.
#' @param dilation A logical value indicating whether the data will be dilated
#'     or not.
#' @param erosion_first A logical value indicating whether erosion should be
#'     carried out before dilation or vice versa.
#' @param nt Number of times to be processed.
#' @param ... Further arguments passed to [rast()].
#'
#' @return
#' A [SpatRaster-class] object with value 1 for the processed features and 0
#' for the background.
#' The output is already factorized and includes a color table for the two
#' categories (white and black).
#'
#' @author Jan Blöthe and Miguel Alvarez (\email{kamapu78@@gmail.com}).
#'
#' @export
erodil_raster <- function(raster, ...) {
  UseMethod("erodil_raster", raster)
}

#' @rdname erodil_raster
#' @aliases erodil_raster,SpatRaster-method
#' @method erodil_raster SpatRaster
#' @export
erodil_raster.SpatRaster <- function(
    raster, width = c(3, 3), type = "diamond",
    erosion = TRUE, dilation = TRUE, erosion_first = TRUE, nt = 1, ...) {
  # shape kernel
  kernel <- shapeKernel(width = width, type = type)
  # extract crs and extent
  crs <- crs(raster)
  extent <- ext(raster)
  # convert to binary array
  if (dim(raster)[3] > 1) {
    warning("Only the first layer in 'raster' will be processed.")
    raster <- raster[[1]]
  }
  raster <- as.array(raster)
  raster[is.na(raster)] <- 0
  raster[raster != 0] <- 1
  # Apply morphology
  if (erosion_first) {
    if (erosion) {
      for (i in 1:nt) {
        raster <- erode(raster, kernel)
      }
    }
    if (dilation) {
      for (i in 1:nt) {
        raster <- dilate(raster, kernel)
      }
    }
  } else {
    if (dilation) {
      for (i in 1:nt) {
        raster <- dilate(raster, kernel)
      }
    }
    if (erosion) {
      for (i in 1:nt) {
        raster <- erode(raster, kernel)
      }
    }
  }
  # convert back to spatraster
  raster <- as.factor(rast(raster, crs = crs, extent = extent))
  coltab(raster) <- data.frame(value = c(0, 1), col = c("white", "black"))
  return()
}
