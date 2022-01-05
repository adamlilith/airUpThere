#' Convert CHELSA "future" period shorthand to full range, or vice versa.
#' 
#' Convert CHELSA "future" period shorthand (e.g., \code{2071}) to full range (e.g., \code{2071-2100}), or vice versa. The function will try to guess whether you want to go from short to long or vice versa.
#' @param cmip Number the Coupled Model Intercomparison Project (CMIP) future ratsers to download. Valid values are either \code{5} or \code{6}.
#' @param period Time period. Looks either like \code{2041} or \code{2041-2070}. Short/long versions should not be mixed.
#' @return Character or integer.
#' @examples
#'
#' chConvertPeriod(5, 2041)
#' chConvertPeriod(5, c(2041, 2061))
#' chConvertPeriod(5, '2061-2080')
#'
#' chConvertPeriod(6, 1981)
#' chConvertPeriod(6, c(1981, 2071))
#' chConvertPeriod(6, '2011-2040')

chConvertPeriod <- function(cmip, period) {

	periods <- chPeriod(cmip)
	shortToLong <- !(grepl(period[1], pattern='-'))
	
	out <- if (shortToLong) {
		periods$long[periods$short %in% period]
	} else {
		periods$short[periods$long %in% period]
	}
	
	out

}
