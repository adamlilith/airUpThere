#' Get "YYYY-MO" (year-month) from a date.
#' 
#' This function returns the "YYYY-MM" format of a date (i.e., dropping the "day").
#' @param x Object(s) of class \code{Date}, or of class \code{character}.
#' @return A character vector.
#' @examples
#' formatYYYYMM(c('2015-10-24', '2017-02-26', '2021-04'))
#' @export
formatYYYYMM <- function(x) {

	y <- getYMD(x, 'y')
	m <- getYMD(x, 'm')
	out <- paste0(y, '-', ifelse(m < 10, '0', ''), m)
	out
	
}
