#' @name erodil_raster
#' @rdname erodil_raster
#' 
#' @title Raster erosion and dilation
#' 
#' @description 
#' This function apply [morphology()] to raster objects.
#' 
#' @param raster An object of class [RasterLayer-class] containing binary values
#'     (0 or 1).
#' @param erosion A logical value indicating whether the data will be eroded or
#'     not.
#' @param dilation A logical value indicating whether the data will be dilated
#'     or not.
#' @param nt Number of times to be processed (passed to [morphology()]).
#' @param ... Further arguments passed to [raster()].
#' 
#' @return A [RasterLayer-class] object.
#' 
#' @author Jan Blöthe and Miguel Alvarez (\email{kamapu78@@gmail.com}).
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
		function(raster, erosion=TRUE, dilation=TRUE, nt=1, ...) {
			# set xy resolution to equal size
			old_res <- res(raster)
			res(raster) <- rep(round(mean(old_res)), 2)
			#convert raster to "SpatialPixelsDataFrame"
			tmp_SPDF <- as(raster, "SpatialPixelsDataFrame")
			#apply morphological erosion filter
			if(erosion)
				tmp_SPDF <- morphology(tmp_SPDF, operation="erode", nt=nt)
			#apply morphological dilation filter
			if(dilation)
				tmp_SPDF <- morphology(tmp_SPDF, operation="dilate", nt=nt)
			#convert to raster data set again
			raster <- raster(tmp_SPDF, layer=1, values=TRUE, ...)
			# Return old resolution
			res(raster) <- old_res
			return(raster)
		})
