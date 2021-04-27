#' Generate a sequence of years and months from a starting and ending date.
#' 
#' This function generates a sequence of years and months for a given start and end date. Days are dropped. For example, if the input is \code{2015-10-24} and \code{2016-02-26}, the output would be \code{2015-10}, \code{2015-11}, \code{2015-12}, \code{2016-01}, and \code{2015-02}.
#' @param start,end Start and end dates. Objects of class \code{Date}, or of class \code{character} in either YYYY-MM-DD or YYYY-MO format (e.g., \code{2015-01-01} or \code{2015-01}; note the need for leading zeros for months <10).
#' @return A character vector.
#' @examples
#' seqMonths('2015-10-24', '2017-02-26')
#' seqMonths('2015-10', '2017-02')
#' @export
seqMonths <- function(start, end) {

	if (length(start) != 1L | length(end) != 1L) stop('More than one value supplied to "start" and/or "end".')

	start <- as.character(start)
	end <- as.character(end)

	yr1 <- getYMD(start, 'y')
	yr2 <- getYMD(end, 'y')
	
	mo1 <- getYMD(start, 'm')
	mo2 <- getYMD(end, 'm')

	if (yr2 < yr1 | (yr1 == yr2 & mo2 < mo1)) stop('Ending date occurs before starting date.')
	if (mo1 < 1 | mo1 > 12 | mo2 < 1 | mo2 > 12) stop('Invalid months.')

	yrs <- mos <- integer()

	# this year
	if (yr1 == yr2) {
		yrs <- rep(yr1, mo2 - mo1 + 1)
		mos <- mo1:mo2
	} else {
		yrs <- rep(yr1, 12 - mo1 + 1)
		mos <- mo1:12
	}
	
	# full intervening years
	if (yr2 > yr1 + 1) {
		n <- yr2 - yr1 - 1
		int <- yr2 - yr1 - 1
		yrs <- c(yrs, rep(yr1 + 1:int, each=12 * n))
		mos <- c(mos, rep(1:12, n))
	}
	
	# tail end of last year
	if (yr2 > yr1) {
		yrs <- c(yrs, rep(yr2, mo2))
		mos <- c(mos, 1:mo2)
	}

	out <- paste0(yrs, '-', ifelse(mos < 10, '0', ''), mos)
	out
	
}
