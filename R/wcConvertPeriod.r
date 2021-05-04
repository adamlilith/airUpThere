#' Name of future time period in WorldClim URL and file names
#'
#' Return the name of a future time WorldClim time period in the format used in URLs and file names.
#' @param ver WorldClim version: 1.4 or 2.1.
#' @param period Year indicating time period.
#' @param type Either of \code{future} or \code{decadal}. Only needed for WC 2.1.
#' @return Character
#' @examples
#' wcConvertPeriod(1.4, 2050)
#' wcConvertPeriod(1.4, 2070)
#' wcConvertPeriod(2.1, 2030)
#' wcConvertPeriod(2.1, 2050)
#' wcConvertPeriod(2.1, 2070)
#' wcConvertPeriod(2.1, 2090)
#' @export
wcConvertPeriod <- function(ver, period, type = NULL) {

	yearCode <- if (ver == 1.4) {
		if (period == 2050) {
			'50'
		} else if (period == 2070) {
			'70'
		} else {
			stop('This is not a valid time period for CMIP5.')
		}
	} else if (ver == 2.1) {
		if (type == 'future') {
			if (period == 2030) {
				'2021-2040'
			} else if (period == 2050) {
				'2041-2060'
			} else if (period == 2070) {
				'2061-2080'
			} else if (period == 2090) {
				'2081-2100'
			} else {
				stop('This is not a valid time period for CMIP6.')
			}
		} else if (type == 'decadal') {
			if (period == 1960) {
				'1960-1969'
			} else if (period == 1970) {
				'1970-1979'
			} else if (period == 1980) {
				'1980-1989'
			} else if (period == 1990) {
				'1990-1999'
			} else if (period == 2000) {
				'2000-2009'
			} else if (period == 2010) {
				'2010-2018'
			} else {
				stop('This is not a valid time period for decadal rasters.')
			}
		}
	} else {
		stop('This is an invalid WorldClim period.')
	}
	
	yearCode
	
}

