#' Difference in months between dates in YYYY-MM format
#'
#' This function allows you to calculate the difference between two dates in YYYY-MM format.
#'
#' @param d1,d2 Two dates (characters) expressed in YYYY-MM format.
#' @return Integer indicating number of months difference between the two dates.
#' @examples
#'
#' monthDiff('2020-10', '2020-05')
#' monthDiff('2020-10', '2019-10')
#' monthDiff('2020-10', '2017-12')
#' 
#' @export
monthDiff <- function(d1, d2) {

	y1 <- getYMD(d1, 'y')
	y2 <- getYMD(d2, 'y')

	m1 <- 12 * y1 + getYMD(d1, 'm')
	m2 <- 12 * y2 + getYMD(d2, 'm')

	m1 - m2

}
