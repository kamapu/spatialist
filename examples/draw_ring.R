## Two different rings
A <- draw_ring(3, 6)
B <- draw_ring(7, 10)

A
B

## For circles, set inner=0
C <- draw_ring(0, 100)

## For squared rings, set squared=TRUE
D <- draw_ring(0, 50, squared = TRUE)

## Now see them in plot
library(terra)

par(mfrow = c(2, 2))
plot(rast(A))
plot(rast(B))
plot(rast(C))
plot(rast(D))
