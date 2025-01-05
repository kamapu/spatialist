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
setGeneric(
  "landforms",
  function(small, large, ...) {
    standardGeneric("landforms")
  }
)

#' @rdname landforms
#' @aliases landforms,RasterLayer,RasterLayer-method
setMethod(
  "landforms", signature(small = "SpatRaster", large = "SpatRaster"),
  function(small, large, mean_s, mean_l, sd_s, sd_l, ...) {
    # Parameters for cut levels
    if (missing(mean_s)) mean_s <- global(small, "mean", na.rm = TRUE)
    if (missing(mean_l)) mean_l <- global(large, "mean", na.rm = TRUE)
    if (missing(sd_s)) sd_s <- global(small, "sd", na.rm = TRUE)
    if (missing(sd_l)) sd_l <- global(large, "sd", na.rm = TRUE)
    # Ranges of input data
    range_s <- global(small, "range", na.rm = TRUE)
    range_l <- global(large, "range", na.rm = TRUE)
    # Conditions
    C1 <- app(small, fun = function(x) {
      cut(x, unlist(c(
        range_s[1],
        mean_s - sd_s, mean_s + sd_s, range_s[2]
      )),
      include.lowest = TRUE, right = TRUE, labels = FALSE
      )
    })
    C2 <- app(large, fun = function(x) {
      cut(x, unlist(c(
        range_l[1],
        mean_l - sd_l, mean_l + sd_l, range_l[2]
      )),
      include.lowest = TRUE, right = TRUE, labels = FALSE
      )
    })
    # Classification
    lf_classes <- app(x = c(C1, C2), fun = function(x) {
      (x[1] == 1 & x[2] == 1) * 1 +
        (x[1] == 1 & x[2] == 2) * 2 +
        (x[1] == 1 & x[2] == 3) * 3 +
        (x[1] == 2 & x[2] == 1) * 4 +
        (x[1] == 2 & x[2] == 2) * 5 +
        (x[1] == 2 & x[2] == 3) * 6 +
        (x[1] == 3 & x[2] == 1) * 7 +
        (x[1] == 3 & x[2] == 2) * 8 +
        (x[1] == 3 & x[2] == 3) * 9
    }, ...)
    return(lf_classes)
  }
)
