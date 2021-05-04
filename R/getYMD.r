#' Year, month, or day from a date
#'
#' Get year, month, or day as an integer from a date without having to convert it to a \code{Date}.
#' @param d Date(s) in YYYY, YYYY-MM, or YYYY-MM-DD format.
#' @param get Any of \code{'year'}, \code{'month'}, or \code{'day'}.
#' @return Integer
#' @examples
#' getYMD('2021-04-27', 'year')
#' getYMD('2021-04-27', 'month')
#' getYMD('2021-04-27', 'day')
#' @export
getYMD <- compiler::cmpfun(function(d, get) {

	d <- as.character(d)
	
	choices <- c('year', 'month', 'day')
	get <- pmatch(get, choices)
	
	if (is.na(get)) {
		stop('Specify whether you want the year, month, or day.')
	} else {
	
		get <- choices[get]

		out <- if (get == 'year') {
			as.integer(substr(d, 1, 4))
		} else if (get == 'month') {
			as.integer(substr(d, 6, 7))
		} else if (get == 'day') {
			as.integer(substr(d, 9, 10))
		}
		
	}

	out
	
})
