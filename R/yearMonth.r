#' Get "YYYY-MO" (year-month) from a date.
#' 
#' This function returns the "YYYY-MM" format of a date (i.e., dropping the "day").
#' @param x Object(s) of class \code{Date}, or of class \code{character}.
#' @return A character vector.
#' @examples
#' getYearMonth(c('2015-10-24', '2017-02-26', '2021-04'))
#' @export
getYearMonth <- function(x) {

	x <- as.character(x)
	out <- paste0(substr(x, 1, 4), '-', substr(x, 6, 7))
	out
	
}
