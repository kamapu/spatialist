#' @name bbox_sp
#' @rdname bbox_sp
#' 
#' @title Bounding boxes as spatial objects
#' 
#' @description 
#' Bounding boxes of raster (class `Raster*`) objects will be converted either
#' to [SpatialPointsDataFrame-class] or [SpatialPointsDataFrame-class].
#' 
#' This function extract the corners including the bounding box of the input
#' raster or extension and writes either four spatial points or a rectangle as
#' polygon.
#' 
#' @param raster An object of class `Raster*` or [Extent-class].
#' @param geom Character value indicating the geometry of output (either point
#'     or polygon).
#' @param proj4 Character vector indicating the `proj4string` (required only
#'     for inputs of class [Extent-class]).
#' @param ... Further arguments passed among methods (not yet used).
#' 
#' @return Either a [SpatialPointsDataFrame-class] or a
#' [SpatialPolygonsDataFrame-class] object.
#' 
#' @author Lukas Trübenbach and Miguel Alvarez (\email{kamapu78@@gmail.com}).
#' 
#' @examples
#' require(raster)
#' 
#' r <- raster(system.file("external/test.grd", package="raster"))
#' plot(r)
#' plot(bbox_sp(r, "polygon"), border="darkgreen", lwd=3, add=TRUE)
#' plot(bbox_sp(r, "point"), col="red", pch=16, cex=3, add=TRUE)
#' 
#' @exportMethod bbox_sp
#' 
setGeneric("bbox_sp",
		function(raster, ...)
			standardGeneric("bbox_sp")
)

#' @rdname bbox_sp
#' @aliases bbox_sp,Extent-method
#' 
setMethod("bbox_sp", signature(raster="Extent"),
		function(raster, proj4="", geom="point", ...) {
			geom <- pmatch(geom, c("point", "polygon"))
			if(!geom %in% c(1,2))
				stop("Invalid value for argument 'geom'.")
			# Extension as points
			if(geom == 1) {
				raster <- data.frame(
						fid=1:4,
						longitude=c(raster@xmin, raster@xmin, raster@xmax,
								raster@xmax),
						latitude=c(raster@ymin, raster@ymax, raster@ymax,
								raster@ymin))
				coordinates(raster) <- ~ longitude + latitude
				proj4string(raster) <- CRS(proj4)
			}
			# rasterension as polygon
			if(geom == 2) {
				raster <- Polygons(list(Polygon(cbind(c(raster@xmin, raster@xmin,
														raster@xmax, raster@xmax),
												c(raster@ymin, raster@ymax,
														raster@ymax, raster@ymin)))),
						ID="bbox")
				raster <- SpatialPolygons(list(raster), proj4string=CRS(proj4))
				raster <- SpatialPolygonsDataFrame(raster, data.frame(fid=1),
						match.ID=FALSE)
			}
			return(raster)
		}
)

#' @rdname bbox_sp
#' @aliases bbox_sp,RasterLayer-method
#' 
setMethod("bbox_sp", signature(raster="RasterLayer"),
		function(raster, geom="point", ...) {
			return(bbox_sp(extent(raster), proj4string(raster), geom, ...))
		}
)

#' @rdname bbox_sp
#' @aliases bbox_sp,RasterStack-method
#' 
setMethod("bbox_sp", signature(raster="RasterStack"),
		function(raster, geom="point", ...) {
			return(bbox_sp(extent(raster), proj4string(raster), geom, ...))
		}
)

#' @rdname bbox_sp
#' @aliases bbox_sp,RasterBrick-method
#' 
setMethod("bbox_sp", signature(raster="RasterBrick"),
		function(raster, geom="point", ...) {
			return(bbox_sp(extent(raster), proj4string(raster), geom, ...))
		}
)
