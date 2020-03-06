#' @name pixel_dim
#' 
#' @title Estimating metric dimensions of pixels in raster object
#' 
#' @description 
#' Pixel dimension in raster objects (class `Raster*`) in projected space.
#' 
#' For non projected rasters, the bounding boxes will be reprojected to UTM
#' coordinates for the calculation of height (latitude) and width (longitude).
#' For projected rasters, the calculation will be directly done in the units of
#' the respective spacial reference system.
#' 
#' @param raster An object of class `Raster*`.
#' @param projected Logical value indicating whether the coordinates of raster
#'     input are projected or not.
#' @param ... Further arguments passed among methods (Not yet used).
#' 
#' @return A named vector with the estimated height (latitude) and width
#' (longitude) of pixels either in meters for non-projected rasters or in the
#' own spacial units for projected ones.
#' 
#' @author Lukas Trübenbach and Miguel Alvarez (\email{kamapu78@@gmail.com}).
#' 
#' @examples
#' require(raster)
#' 
#' r <- raster(system.file("external/test.grd", package="raster"))
#' pixel_dim(r)
#' 
#' ## More appropriate for projected coordinates
#' pixel_dim(r, projected=TRUE)
#' 
#' @export pixel_dim
#' 
pixel_dim <- function(raster, projected=FALSE) {
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
