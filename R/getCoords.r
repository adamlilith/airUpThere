#' Get coordinates
#' 
#' Get coordinates from an object of class \code{data.frame}, \code{matrix}, \code{SpatialPoints}, \code{SpatialPointsDataFrame}, or \code{SpatVector}.
#' @param x Object of class \code{data.frame}, \coed{SpatialPoints}, \code{SpatialPointsDataFrame}, or \code{SpatVector}.
#' @param index
getCoords <- function(x, index=NULL, longLat=NULL) {

	if (any(c('SpatialPointsDataFrame') %in% class(x))) {
		if (is.null(index)) index <- 1:nrow(x)
		locs <- sp::coordinates(x)
	} else if (any(c('SpatialPoints') %in% class(x))) {
		if (is.null(index)) index <- seq_along(x)
		locs <- sp::coordinates(x)
	} else if (any(c('data.frame', 'matrix') %in% class(x))) {
		if (is.null(index)) index <- 1:nrow(x)
		locs <- x[ , longLat]
	} else if ('SpatVector' %in% class(x)) {
		if (is.null(index)) index <- seq_along(x)
		locs <- terra::coords(x)
	}
	
	locs <- locs[index, , drop=FALSE]
	locs
	
}
