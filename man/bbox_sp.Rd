\name{bbox_sp}
\alias{bbox_sp}

\title{Bounding boxes as spatial objects}
\description{
Bounding boxes of raster (class \code{Raster*}) objects will be exported as
spatial objects.
}
\usage{
bbox_sp(raster, geom="point")
}
\arguments{
\item{raster}{An object of class \code{Raster*}.}
\item{geom}{Character value indicating the geometry of output (either point or
polygon).}
}
\details{
Some description will follow.
}
\value{
Bounding box either as \code{SpatialPointsDataFrame} or as \code{SpatialPolygonsDataFrame}.
}
\author{
Lukas Truebenbach and Miguel Alvarez (\email{kamapu78@gmail.com}).
}
\examples{
library(spatialist)
library(raster)

r <- raster(system.file("external/test.grd", package="raster"))
plot(r)
plot(bbox_sp(r, "polygon"), border="darkgreen", lwd=3, add=TRUE)
plot(bbox_sp(r, "point"), col="red", pch=16, cex=3, add=TRUE)
}
