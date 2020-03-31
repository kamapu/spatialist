#' @name erodil_raster
#' @rdname erodil_raster
#' 
#' @title Raster erosion and dilation
#' 
#' @description 
#' This function apply [morphology()] to raster objects.
#' 
#' @param raster An object of class [RasterLayer-class] containing NA values.
#'     Note that as in the function [morphology()], erosion and dilation is
#'     applied to non-NA values.
#' @param erosion A logical value indicating whether the data will be eroded or
#'     not.
#' @param dilation A logical value indicating whether the data will be dilated
#'     or not.
#' @param erosion_first A logical value indicating whether erosion should be
#'     carried out before dilation or vice versa.
#' @param nt Number of times to be processed (passed to [morphology()]).
#' @param ... Further arguments passed to [raster()].
#' 
#' @return 
#' A [RasterLayer-class] object with value 1 for the processed features and NA
#' for the background.
#' 
#' @author Jan Blöthe and Miguel Alvarez (\email{kamapu78@@gmail.com}).
#' 
#' @examples 
#' ## Load installed rasterLayer
#' require(raster)
#' r <- raster(file.path(path.package("spatialist"), "binras.tif"))
#' 
#' ## Make only erosion or only dilation
#' plot(stack(list(original=r,
#'          eroded=erodil_raster(r, dilation=FALSE),
#' 			dilated=erodil_raster(r, erosion=FALSE))))
#' 
#' ## By default erosion will be done before dilation
#' plot(stack(list(original=r,
#' 			eroded_first=erodil_raster(r),
#' 			dilated_first=erodil_raster(r, erosion_first=FALSE),
#' 			both=erodil_raster(erodil_raster(r, erosion_first=FALSE)))))
#' 
#' @exportMethod erodil_raster
#' 
setGeneric("erodil_raster",
		function(raster, ...)
			standardGeneric("erodil_raster")
)

#' @rdname erodil_raster
#' @aliases erodil_raster,RasterLayer-method
#' 
setMethod("erodil_raster", signature(raster="RasterLayer"),
		function(raster, erosion=TRUE, dilation=TRUE, erosion_first=TRUE, nt=1,
				...) {
			#convert raster to "SpatialPixelsDataFrame"
			tmp_SPDF <- as(raster, "SpatialPixelsDataFrame")
			# Apply morphology
			if(erosion_first) {
				if(erosion)
					tmp_SPDF <- morphology(tmp_SPDF, operation="erode", nt=nt)
				if(dilation)
					tmp_SPDF <- morphology(tmp_SPDF, operation="dilate", nt=nt)
			} else {
				if(dilation)
					tmp_SPDF <- morphology(tmp_SPDF, operation="dilate", nt=nt)
				if(erosion)
					tmp_SPDF <- morphology(tmp_SPDF, operation="erode", nt=nt)
			}
			#convert back to rasterLayer
			return(raster(tmp_SPDF, layer=1, values=TRUE, ...))
		})
