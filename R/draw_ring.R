# TODO:   Get a ring matrix for function raster::focal
# 
# Author: Miguel Alvarez
################################################################################

draw_ring <- function(inner, outer, squared=FALSE) {
	if(inner > outer - 1)
		stop("'outer' should be higher than 'inner'")
	r_ind <- matrix(rep(1:(outer*2 + 1), outer*2 + 1), nrow=outer*2 + 1,
			ncol=outer*2 + 1)
	c_ind <- matrix(rep(1:(outer*2 + 1), outer*2 + 1), nrow=outer*2 + 1,
			ncol=outer*2 + 1, byrow=TRUE)
	if(squared) {
		r_dist <- abs(r_ind - outer - 1) > inner
		c_dist <- abs(c_ind - outer - 1) > inner
		win <- r_dist | c_dist
	} else {
		Dist <- sqrt(((outer + 1) - r_ind)^2 + ((outer + 1) - c_ind)^2)
		win <- Dist <= outer & Dist >= inner
	}
	return(matrix(as.numeric(win), ncol=ncol(win), nrow=nrow(win)))
}
