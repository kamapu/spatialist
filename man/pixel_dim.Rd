\name{pixel_dim}
\alias{pixel_dim}

\title{Estimating metric dimensions of pixels in raster object}
\description{
Pixel dimension in raster objects (class \code{Raster*}) measured in meters.
}
\usage{
pixel_dim(raster)
}
\arguments{
\item{raster}{An object of class \code{Raster*}.}
}
\details{
Some description will follow.
}
\value{
A named vector with the estimated length and width of pixels in meters.
}
\author{
Lukas Truebenbach and Miguel Alvarez (\email{kamapu78@gmail.com}).
}
\examples{
library(spatialist)
library(raster)

r <- raster(system.file("external/test.grd", package="raster"))
pixel_dim(r)
}
