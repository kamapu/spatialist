\name{pixel_dim}
\docType{methods}
\alias{pixel_dim}
\alias{pixel_dim,RasterLayer-method}
\alias{pixel_dim,RasterStack-method}
\alias{pixel_dim,RasterBrick-method}

\title{Estimating metric dimensions of pixels in raster object}
\description{
Pixel dimension in raster objects (class \code{Raster*}) in projected space.
}
\usage{
\S4method{pixel_dim}{RasterLayer}(raster, projected=FALSE, ...)

\S4method{pixel_dim}{RasterStack}(raster, projected=FALSE, ...)

\S4method{pixel_dim}{RasterBrick}(raster, projected=FALSE, ...)
}
\arguments{
\item{raster}{An object of class \code{Raster*}.}
\item{projected}{Logical value indicating whether the coordinates of raster
	input are projected or not.}
\item{...}{Further arguments passed among methods (Not yet used).}
}
\details{
For non projected rasters, the bounding boxes will be reprojected to UTM
coordinates for the calculation of height (latitude) and width (longitude).
For projected rasters, the calculation will be directly done in the units of the
respective spacial reference system.
}
\value{
A named vector with the estimated height (latitude) and width (longitude) of
pixels either in meters for non-projected rasters or in the own spacial units
for projected ones.
}
\author{
Lukas Trübenbach and Miguel Alvarez (\email{kamapu78@gmail.com}).
}
\examples{
library(spatialist)
library(raster)

r <- raster(system.file("external/test.grd", package="raster"))
pixel_dim(r)

## More appropriate for projected coordinates
pixel_dim(r, projected=TRUE)
}
