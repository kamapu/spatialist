#' @name mask_window
#'
#' @title Extract portions of windows
#'
#' @description
#' Some calculations using [terra::focal()] may require partial windows, for instance
#' calculating northness or eastness.
#' Portions may correspond to halves or quarters of the window (see argument `option`).
#'
#' @param window A matrix including a window to be masked, for instance a window generated
#'     by [draw_ring()]. If missing, you need to specify the dimensions of the mask.
#' @param option A character value indicating the portion to be extracted. The only
#'     alternatives are "top", "bottom", "left", "right", "topleft", "topright",
#'     "bottomleft", and "bottomright".
#' @param dim An integer vector of lenght 2. The first value corresponds to the height
#'     (number of rows), and the second value corresponds to the width (number of
#'     columns). Only odd values are accepted. If a 'window' is provided, the dimensions
#'     of this window will be used and this argument will be ignored.
#'
#' @return
#' If window is not provided, a matrix with the specified dimentions and values 1 and
#' `NA`.
#' If window is provided, It will be masked to the respective portion and excluded cells
#' will become `NA`s.
#'
#' @author Miguel Alvarez \email{kamapu@@posteo.de}
#'
#' @seealso [draw_ring()].
#'
#' @example examples/mask_window.R
#'
#' @export
mask_window <- function(window, option = "top", dim = c(5, 5)) {
  if (!missing(window)) {
    dim <- dim(window)
  }
  # Check for even numbers
  if (any(!as.logical(dim %% 2))) {
    stop("Only odd numbers are allowed for argument 'dim'.")
  }
  # Check for proper options
  opt_list <- c(
    "top", "bottom", "left", "right", "topleft", "topright", "bottomleft",
    "bottomright"
  )
  option <- pmatch(option, opt_list)
  if (is.na(option)) {
    stop(paste0(
      "Only following values are valid for argument 'option':\n  ",
      paste(opt_list, collapse = "\n  ")
    ))
  }
  # option 'top'
  if (option == 1) {
    win <- rbind(
      matrix(rep(1, times = floor(dim[1] / 2) * dim[2]), ncol = dim[2]),
      matrix(rep(NA, times = ceiling(dim[1] / 2) * dim[2]), ncol = dim[2])
    )
  }
  # option 'bottom'
  if (option == 2) {
    win <- rbind(
      matrix(rep(NA, times = ceiling(dim[1] / 2) * dim[2]), ncol = dim[2]),
      matrix(rep(1, times = floor(dim[1] / 2) * dim[2]), ncol = dim[2])
    )
  }
  # option 'left'
  if (option == 3) {
    win <- cbind(
      matrix(rep(1, times = dim[1] * floor(dim[2] / 2)), nrow = dim[1]),
      matrix(rep(NA, times = dim[1] * ceiling(dim[2] / 2)), nrow = dim[1])
    )
  }
  # option 'right'
  if (option == 4) {
    win <- cbind(
      matrix(rep(NA, times = dim[1] * ceiling(dim[2] / 2)), nrow = dim[1]),
      matrix(rep(1, times = dim[1] * floor(dim[2] / 2)), nrow = dim[1])
    )
  }
  # option 'topleft'
  if (option == 5) {
    win <- rbind(
      cbind(
        matrix(rep(1, times = floor(dim[1] / 2) * floor(dim[2] / 2)), ncol = floor(dim[2] / 2)),
        matrix(rep(NA, times = floor(dim[1] / 2) * ceiling(dim[2] / 2)),
          ncol = ceiling(dim[2] / 2)
        )
      ),
      matrix(rep(NA, times = ceiling(dim[1] / 2) * dim[2]), ncol = dim[2])
    )
  }
  # option 'topright'
  if (option == 6) {
    win <- rbind(
      cbind(
        matrix(rep(NA, times = floor(dim[1] / 2) * ceiling(dim[2] / 2)),
          ncol = ceiling(dim[2] / 2)
        ),
        matrix(rep(1, times = floor(dim[1] / 2) * floor(dim[2] / 2)),
          ncol = floor(dim[2] / 2)
        )
      ),
      matrix(rep(NA, times = ceiling(dim[1] / 2) * dim[2]), ncol = dim[2])
    )
  } # option 'bottomleft'
  if (option == 7) {
    win <- rbind(
      matrix(rep(NA, times = ceiling(dim[1] / 2) * dim[2]), ncol = dim[2]),
      cbind(
        matrix(rep(1, times = floor(dim[1] / 2) * floor(dim[2] / 2)), ncol = floor(dim[2] / 2)),
        matrix(rep(NA, times = floor(dim[1] / 2) * ceiling(dim[2] / 2)),
          ncol = ceiling(dim[2] / 2)
        )
      )
    )
  }
  # option 'bottomright'
  if (option == 8) {
    win <- rbind(
      matrix(rep(NA, times = ceiling(dim[1] / 2) * dim[2]), ncol = dim[2]),
      cbind(
        matrix(rep(NA, times = floor(dim[1] / 2) * ceiling(dim[2] / 2)),
          ncol = ceiling(dim[2] / 2)
        ),
        matrix(rep(1, times = floor(dim[1] / 2) * floor(dim[2] / 2)),
          ncol = floor(dim[2] / 2)
        )
      )
    )
  }
  # Process window
  if (!missing(window)) {
    win <- window * win
  }
  return(win)
}
