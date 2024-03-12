#' Obtain metadata on extraction from PRISM
#'
#' This function obtains metdata (ID, cell number, weights, coordinates) for a planned extraction from PRISM. It is necessary for cases where the locations to extract from are lines or polygons, and can be useful if they are points.
#'
#' @param x SpatVector
#' @param template NULL or SpatRaster
#' @param dots Dots (after processing)
#' @param dates NULL or vector of dates. Used for `prExtractRelative` functions
#' @param startDates, endDates NULL or vector of dates. Used for `prExtractAbsolute` functions
#' @noRd
.prMeta <- function(x, template, dots, dates = NULL, startDates = NULL, endDates = NULL) {

	isPoints <- terra::geomtype(x) == 'points'

	# lines/polygons
	if (!isPoints) {

		thisDots <- dots
		thisDots$ID <- TRUE
		thisDots$weights <- TRUE
		thisDots$cells <- TRUE
		thisDots$xy <- TRUE
		if (!inherits(template, 'SpatRaster')) template <- terra::rast(template)
		thisArgs <- list(x = template, y = x)
		thisDots <- c(thisArgs, thisDots)
		meta <- do.call(terra::extract, thisDots)

		meta <- meta[ , c('ID', 'weight', 'cell', 'x', 'y'), drop = FALSE]

	} else if (isPoints & !is.null(template)) {
	
		thisDots <- dots
		thisDots$ID <- TRUE
		thisDots$cells <- TRUE
		thisDots$xy <- TRUE
		if (!inherits(template, 'SpatRaster')) template <- terra::rast(template)
		thisArgs <- list(x = template, y = x)
		thisDots <- c(thisArgs, thisDots)
		meta <- do.call(terra::extract, thisDots)

		meta <- meta[ , c('ID', 'cell', 'x', 'y'), drop = FALSE]
	
	} else {
	
		nrowOut <- nrow(x)
	
		meta <- data.frame(
			ID = seq_len(nrowOut),
			weight = rep(1, nrowOut),
			cell = rep(NA, nrowOut),
			x = rep(NA, nrowOut),
			y = rep(NA, nrowOut)
		)

	}
	
	if (!is.null(dates)) meta$date <- dates[meta$ID]
	if (!is.null(startDates)) meta$startDate <- startDates[meta$ID]
	if (!is.null(endDates)) meta$endDate <- endDates[meta$ID]
	meta

}
