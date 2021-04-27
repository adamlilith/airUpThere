#' Get dates from a vector of a data frame-like object
#' @param x Data frame, SPDF, of object of class \code{SpatVector}
#' @param date Object of class{Date} or character vector with one or three elements
#' @return Vector of \code{Date} objects
#' @keywords internal
prGetDates <- function(x, date) {
	
	if (class(date) == 'Date') {
		dates <- if (length(date) == 1L) {
			rep(date, nrow(x))
		} else if (length(date) != nrow(x)) {
			stop('Argument "date" must have either a single value or one value per record.')
		}
	} else if (length(date) == 3 & 'character' %in% class(date)) {
		dates <- lubridate::make_date(
			year=x[ , date[1], drop=TRUE],
			month=x[ , date[2], drop=TRUE],
			day=x[ , date[3]]
		)
	} else if (length(date) == 1 & 'character' %in% class(date)) {
		if (class(x[ , date]) != 'Date') {
			dates <- lubridate::ymd(x[ , date, drop=TRUE])
		} else {
			dates <- x[ , date, drop=TRUE]
		}
	} else {
		stop('Argument "date" must be an object of class "Date", or a character vector with one or three elements.')
	}
	
	dates
	
}

#' Convert PRISM resolution to name used in file names
#'
#' @param res Either 30 (30 arcsec) or 800 (30 arcsec), or 1 (1 arcmin) or 4 (1 arcmin)
#' @return Character
#' @keywords internal
prGetRes <- function(res) {

	res <- as.character(res)
	out <- if (res == '30' | res == '800') {
		'30s'
	} else if (res == '1' | res == '4') {
		'4km'
	}
	out

}
