# TODO:   Extracting bounding boxes for raster layers
# 
# Author: Miguel Alvarez
################################################################################

bbox_sp <- function(raster, geom="point") {
	geom <- pmatch(geom, c("point", "polygon"))
	if(!geom %in% c(1,2))
		stop("Invalid value for argument 'geom'.")
	ext <- extent(raster)
	# Extension as points
	if(geom == 1) {
		ext <- data.frame(fid=1:4, longitude=c(ext@xmin, ext@xmin, ext@xmax,
						ext@xmax), latitude=c(ext@ymin, ext@ymax, ext@ymax,
						ext@ymin))
		coordinates(ext) <- ~ longitude + latitude
		proj4string(ext) <- CRS(proj4string(raster))
	}
	# Extension as polygon
	if(geom == 2) {
		ext <- Polygons(list(Polygon(cbind(c(ext@xmin, ext@xmin, ext@xmax,
												ext@xmax), c(ext@ymin, ext@ymax,
												ext@ymax, ext@ymin)))),
				ID="bbox")
		ext <- SpatialPolygons(list(ext), proj4string=CRS(proj4string(raster)))
		ext <- SpatialPolygonsDataFrame(ext, data.frame(fid=1), match.ID=FALSE)
	}
	return(ext)
}
