#' Compare dates in YYYY, YYYY-MM, or YYYY-MM-DD formats
#'
#' Compare dates in YYYY-MM format
#' @param d1,d2 Dates in YYYY, YYYY-MM, YYYY-MM-DD.
#' @param op Any of \code{'>'}, \code{'>='}, \code{'=='}, \code{'<'}, \code{'<='}, or \code{'!='}.
#' @return Logical.
#' @examples
#' compareDates('2021-04-27', '2021-05-27', '>')
#' compareDates('2021-05-27', '2021-04-27', '>')
#'
#' compareDates('2021-05-27', '2021-05-27', '==')
#' compareDates('2021-04-27', '2021-05-27', '!=')
#'
#' '2021-05-27' %>d% '2021-04-27'
#' @export
compareDates <- compiler::cmpfun(function(d1, d2, op) {

	# convert to date object
	if ('character' %in% class(d1)) {
		nchar(d1)
		d1 <- if (n == 4) {
			lubridate::ymd(paste0(d1, '-01-01'))
		} else if (n == 7) {
			lubridate::ymd(paste0(d1, '-01'))
		} else if (n == 10) {
			lubridate::ymd(d1)
		} else {
			stop('Argument "d1" does not appear to be a valid date.')
		}
	}

	if ('character' %in% class(d2)) {
		n <- nchar(d2)
		d2 <- if (n == 4) {
			lubridate::ymd(paste0(d2, '-01-01'))
		} else if (n == 7) {
			lubridate::ymd(paste0(d2, '-01'))
		} else if (n == 10) {
			lubridate::ymd(d2)
		} else {
			stop('Argument "d2" does not appear to be a valid date.')
		}

	}

	out <- do.call(op, list(d1, d2))
	out
	
})

#' @describeIn compareDates Compare dates of YYYY, YYYY-MM, or YYYY-MM-DD format using >
#' @export
`%>d%` <- compiler::cmpfun(function(d1, d2) {

	out <- do.call('>', list(d1, d2))
	out

})

#' @describeIn compareDates Compare dates of YYYY, YYYY-MM, or YYYY-MM-DD format using >=
#' @export
`%>=d%` <- compiler::cmpfun(function(d1, d2) {

	out <- do.call('>=', list(d1, d2))
	out

})

#' @describeIn compareDates Compare dates of YYYY, YYYY-MM, or YYYY-MM-DD format using ==
#' @export
`%==d%` <- compiler::cmpfun(function(d1, d2) {

	out <- do.call('==', list(d1, d2))
	out

})

#' @describeIn compareDates Compare dates of YYYY, YYYY-MM, or YYYY-MM-DD format using <
#' @export
`%<d%` <- compiler::cmpfun(function(d1, d2) {

	out <- do.call('<', list(d1, d2))
	out

})

#' @describeIn compareDates Compare dates of YYYY, YYYY-MM, or YYYY-MM-DD format using <=
#' @export
`%<=d%` <- compiler::cmpfun(function(d1, d2) {

	out <- do.call('<=', list(d1, d2))
	out

})

#' @describeIn compareDates Compare dates of YYYY, YYYY-MM, or YYYY-MM-DD format using !=
#' @export
`%!=d%` <- compiler::cmpfun(function(d1, d2) {

	out <- do.call('!=', list(d1, d2))
	out

})
