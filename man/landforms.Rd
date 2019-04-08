\name{landforms}
\docType{methods}
\alias{landforms}
\alias{landforms,RasterLayer,RasterLayer-method}

\title{Classification of landforms}
\description{
Add description.
}
\usage{
\S4method{landforms}{RasterLayer,RasterLayer}(small, large, mean_s, mean_l, sd_s, sd_l, ...)
}
\arguments{
\item{small,large}{Raster layers with topographic position index at small and
	large scale, respectively.}
\item{mean_s,mean_l}{Reference mean value for small and large scale,
	respectively.}
\item{sd_s,sd_l}{Reference standard deviation for small and large scale,
	respectively.}
\item{...}{Further arguments passed to \code{\link{writeRaster}}.}
}
\details{
Details.
}
\value{
A raster.
}
\author{
Miguel Alvarez (\email{kamapu78@gmail.com}).
}
\references{
Link to Weiss (2001)
}
\examples{
## Add Example
}
