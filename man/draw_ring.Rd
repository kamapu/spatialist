\name{draw_ring}
\alias{draw_ring}

\title{Ring matrix as moving window}
\description{
When circles and rings are required as moving windows in function 
\code{\link[raster]{focal}}.
}
\usage{
draw_ring(inner, outer, squared=FALSE)
}
\arguments{
\item{inner}{Inner annulus in pixel (cell) number.}
\item{outer}{Outer annulus in pixel (cell) number.}
\item{squared}{Logical value, whether the ring should be squared or round.}
}
\details{
This function was written to produce rings to be used as moving windows in the
function \code{\link[raster]{focal}}.
Such rings are required for instance to calculate the topographic position index
(TPI) at variable scales according to Weiss
(2001, \url{http://www.jennessent.com/downloads/tpi-poster-tnc_18x22.pdf}).
Values of inner and outer annulus have to be provided as integers and will
correspond to the respective radial length in pixel (cell) number.
}
\value{
A square matrix of dimensions \code{outer*2 + 1} containing values 0 and 1 (1
for cells inside of the ring).
Remember that the function \code{\link[raster]{focal}} will calculate by default
the sum of the cells inside the ring when provided a matrix as window.
Thus you may change the values 1 by their relative weight in order to get the
mean value as output (i.e. \code{ring <- ring/sum(ring)}).
}
\author{
Miguel Alvarez (\email{kamapu78@gmail.com}).
}
\seealso{
\code{\link[raster]{focal}}.
}
\examples{
library(spatialist)

## Two different rings
A <- draw_ring(3, 6)
B <- draw_ring(7, 10)

A
B

## For circles, set inner=0
C <- draw_ring(0,100)

## For squared rings, set squared=TRUE
D <- draw_ring(0, 50, squared=TRUE)

## Now see them in plot
library(raster)

par(mfrow=c(2,2))
plot(as.raster(A))
plot(as.raster(B))
plot(as.raster(C))
plot(as.raster(D))
}
