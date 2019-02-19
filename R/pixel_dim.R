# TODO:   Estimate dimensions of pixels
# 
# Author: Miguel Alvarez
################################################################################

# Generic function
setGeneric("pixel_dim",
		function(raster, ...)
			standardGeneric("pixel_dim")
)

# Hidden function
px_dim <- function(raster, projected=FALSE) {
	ext <- bbox_sp(raster)
	if(!projected) {
		ext <- spTransform(ext, CRS("+init=epsg:4326"))
		centr <- coordinates(ext)
		centr <- c(mean(centr[,1]), mean(centr[,2]))
		# UTM conversion
		zones <- cbind(c(1:60), seq(-177, 177, length.out=60))
		zone <- zones[order(abs(zones[,2] - centr[1]))[1],1]
		if(centr[2] >= 0) hem <- "north" else hem <- "south"
		ext <- spTransform(ext, CRS(paste0("+proj=utm +zone=", zone, " +",
								hem," +datum=WGS84")))
	}
	# euclidean distance
	ext <- coordinates(ext)
	OUT <- c(mean_height=mean(c(dist(ext[c(2,1),]), dist(ext[c(4,3),]))),
			mean_width=mean(c(dist(ext[c(1,4),]), dist(ext[c(2,3),]))))
	OUT <- OUT/dim(raster)[1:2]
	return(OUT)
}

# Define method for RasterLayer etc.
setMethod("pixel_dim", signature(raster="RasterLayer"),
		function(raster, projected=FALSE, ...) px_dim(raster, projected)
)

setMethod("pixel_dim", signature(raster="RasterLayer"),
		function(raster, projected=FALSE, ...) px_dim(raster, projected)
)

setMethod("pixel_dim", signature(raster="RasterLayer"),
		function(raster, projected=FALSE, ...) px_dim(raster, projected)
)
