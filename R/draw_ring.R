#' @name draw_ring
#'
#' @title Ring matrix for moving window
#'
#' @description
#' This function was written to produce rings to be used as moving windows in
#' the function [focal()].
#' Such rings are required for instance to calculate the topographic position
#' index (TPI) at variable scales according to Weiss (2001,
#' \url{http://www.jennessent.com/downloads/tpi-poster-tnc_18x22.pdf}). Values
#' of inner and outer annulus have to be provided as integers and will
#' correspond to the respective radial length in pixel (cell) number.
#'
#' @param inner Inner annulus in pixel (cell) number.
#' @param outer Outer annulus in pixel (cell) number.
#' @param squared Logical value, whether the ring should be squared or round.
#'
#' @return A square matrix of dimensions `outer*2 + 1` containing values 0
#' and 1 (1 for cells inside of the ring). Remember that the function
#' [focal()] will calculate by default the sum of the cells inside the ring
#' when provided a matrix as window.
#' Thus you may change the values 1 by their relative weight in order to get
#' the mean value as output (i.e. `ring <- ring/sum(ring)`).
#'
#' @author Miguel Alvarez \email{kamapu78@@gmail.com}
#'
#' @seealso [focal()].
#'
#' @example examples/draw_ring.R
#'
#' @export
draw_ring <- function(inner, outer, squared = FALSE) {
  if (inner > outer - 1) {
    stop("'outer' should be higher than 'inner'")
  }
  r_ind <- matrix(rep(1:(outer * 2 + 1), outer * 2 + 1),
    nrow = outer * 2 + 1,
    ncol = outer * 2 + 1
  )
  c_ind <- matrix(rep(1:(outer * 2 + 1), outer * 2 + 1),
    nrow = outer * 2 + 1,
    ncol = outer * 2 + 1, byrow = TRUE
  )
  if (squared) {
    r_dist <- abs(r_ind - outer - 1) >= inner
    c_dist <- abs(c_ind - outer - 1) >= inner
    win <- r_dist | c_dist
  } else {
    Dist <- sqrt(((outer + 1) - r_ind)^2 + ((outer + 1) - c_ind)^2)
    win <- Dist <= outer & Dist >= inner
  }
  return(matrix(as.numeric(win), ncol = ncol(win), nrow = nrow(win)))
}
