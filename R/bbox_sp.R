# TODO:   Extracting bounding boxes for raster layers
# 
# Author: Miguel Alvarez
################################################################################

# Generic function
setGeneric("bbox_sp",
		function(raster, ...)
			standardGeneric("bbox_sp")
)

# Method for class Extent
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

# Method for class RasterLayer etc.
setMethod("bbox_sp", signature(raster="RasterLayer"),
		function(raster, geom="point", ...) {
			return(bbox_sp(extent(raster), proj4string(raster), geom, ...))
		}
)

setMethod("bbox_sp", signature(raster="RasterStack"),
		function(raster, geom="point", ...) {
			return(bbox_sp(extent(raster), proj4string(raster), geom, ...))
		}
)

setMethod("bbox_sp", signature(raster="RasterBrick"),
		function(raster, geom="point", ...) {
			return(bbox_sp(extent(raster), proj4string(raster), geom, ...))
		}
)
