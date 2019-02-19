\name{bbox_sp}
\docType{methods}
\alias{bbox_sp}
\alias{bbox_sp,Extent-method}
\alias{bbox_sp,RasterLayer-method}
\alias{bbox_sp,RasterStack-method}
\alias{bbox_sp,RasterBrick-method}

\title{Bounding boxes as spatial objects}
\description{
Bounding boxes of raster (class \code{Raster*}) objects will be converted either
to \code{\linkS4class{SpatialPointsDataFrame}} or
\code{\linkS4class{SpatialPointsDataFrame}}.
}
\usage{
\S4method{bbox_sp}{Extent}(raster, proj4="", geom="point", ...)

\S4method{bbox_sp}{RasterLayer}(raster, geom="point", ...)

\S4method{bbox_sp}{RasterStack}(raster, geom="point", ...)

\S4method{bbox_sp}{RasterBrick}(raster, geom="point", ...)
}
\arguments{
\item{raster}{An object of class \code{Raster*} or \code{Extent}.}
\item{geom}{Character value indicating the geometry of output (either point or
	polygon).}
\item{proj4}{Character vector indicating the \code{proj4string} (required only for
	inputs of class \code{Extent}).}
\item{...}{Further arguments passed among methods (not yet used).}
}
\details{
This function extract the corners including the bounding box of the input raster
or extension and writes either four spatial points or a rectangle as polygon.
}
\value{
Either a \code{\linkS4class{SpatialPointsDataFrame}} or a
\code{\linkS4class{SpatialPolygonsDataFrame}}.
}
\author{
Lukas Trübenbach and Miguel Alvarez (\email{kamapu78@gmail.com}).
}
\examples{
library(spatialist)
library(raster)

r <- raster(system.file("external/test.grd", package="raster"))
plot(r)
plot(bbox_sp(r, "polygon"), border="darkgreen", lwd=3, add=TRUE)
plot(bbox_sp(r, "point"), col="red", pch=16, cex=3, add=TRUE)
}
