#' @name landforms
#' @rdname landforms
#'
#' @title Classification of land forms
#'
#' @description
#' Add description.
#'
#' @details
#' Details.
#'
#' @param small,large Raster layers with topographic position index at small
#'     and large scale, respectively.
#' @param mean_s,mean_l Reference mean value for small and large scale,
#'     respectively.
#' @param sd_s,sd_l Reference standard deviation for small and large scale,
#'     respectively.
#' @param ... Further arguments passed to [writeRaster()].
#'
#' @return A raster.
#'
#' @author Miguel Alvarez (\email{kamapu78@@gmail.com}).
#'
#' @references Link to Weiss (2001)
#'
#' @examples
#' ## Add Example
#'
#' @exportMethod landforms
#'
setGeneric(
  "landforms",
  function(small, large, ...) {
    standardGeneric("landforms")
  }
)

#' @rdname landforms
#' @aliases landforms,RasterLayer,RasterLayer-method
#'
setMethod(
  "landforms", signature(small = "RasterLayer", large = "RasterLayer"),
  function(small, large, mean_s, mean_l, sd_s, sd_l, ...) {
    if (missing(mean_s)) mean_s <- cellStats(small, "mean")
    if (missing(mean_l)) mean_l <- cellStats(large, "mean")
    if (missing(sd_s)) sd_s <- cellStats(small, "sd")
    if (missing(sd_l)) sd_l <- cellStats(large, "sd")
    # Conditions
    C1 <- calc(small, function(x) {
      cut(x, c(
        cellStats(small, "min"),
        mean_s - sd_s, mean_s + sd_s,
        cellStats(small, "max")
      ),
      right = TRUE, include.lowest = TRUE, labels = FALSE
      )
    })
    C2 <- calc(large, function(x) {
      cut(x, c(
        cellStats(large, "min"),
        mean_l - sd_l, mean_l + sd_l,
        cellStats(large, "max")
      ),
      right = TRUE, include.lowest = TRUE, labels = FALSE
      )
    })
    # Classification
    lf_classes <- overlay(C1, C2, fun = function(C1, C2) {
      (C1 == 1 & C2 == 1) * 1 +
        (C1 == 1 & C2 == 2) * 2 +
        (C1 == 1 & C2 == 3) * 3 +
        (C1 == 2 & C2 == 1) * 4 +
        (C1 == 2 & C2 == 2) * 5 +
        (C1 == 2 & C2 == 3) * 6 +
        (C1 == 3 & C2 == 1) * 7 +
        (C1 == 3 & C2 == 2) * 8 +
        (C1 == 3 & C2 == 3) * 9
    }, ...)
    return(lf_classes)
  }
)
