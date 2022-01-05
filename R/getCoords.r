#' Get coordinates
#' 
#' Get coordinates from an object of class \code{data.frame}, \code{matrix}, \code{SpatialPoints}, \code{SpatialPointsDataFrame}, or \code{SpatVector}.
#' @param x Object of class \code{data.frame}, \coed{SpatialPoints}, \code{SpatialPointsDataFrame}, or \code{SpatVector}.
#' @param index Either \code{NULL} (default) or an index of which coordinates to get. If \code{NULL}, all coordinates are obtained.
#' @param longLat Either \code{NULL} (default) or a two-element character vector with the names of the fields in a matrix or data frame with longitude and latitude (in that order).
#' @export
getCoords <- function(x, index=NULL, longLat=NULL) {

	if (inherits(x, 'SpatialPointsDataFrame')) {
		if (is.null(index)) index <- 1:nrow(x)
		locs <- sp::coordinates(x)
	} else if (inherits(x, 'SpatialPoints')) {
		if (is.null(index)) index <- seq_along(x)
		locs <- sp::coordinates(x)
	} else if (inherits(x, 'matrix')| inherits(x, 'data.frame')) {
		if (is.null(index)) index <- 1:nrow(x)
		locs <- x[ , longLat]
	} else if (inherits(x, 'SpatVector')) {
		if (is.null(index)) index <- seq_along(x)
		locs <- terra::coords(x)
	}
	
	locs <- locs[index, , drop=FALSE]
	locs
	
}
