#' Name of future time period in WorldClim URL and file names
#'
#' Return the name of a future time WorldClim time period in the format used in URLs and file names.
#' @param ver WorldClim version: 1.4 or 2.1.
#' @param period Year indicating time period.
#' @param type Either of \code{future} or \code{decadal}. Only needed for WC 2.1.
#' @param standardToFile If \code{TRUE} (default), then convert from "standard" to "file/URL" version. If \code{FALSE}, then from "file/URL" version to "standard."
#' @return Character
#' @examples
#' wcConvertPeriod(1.4, 2050)
#' wcConvertPeriod(1.4, 2070)
#' wcConvertPeriod(2.1, 2030)
#' wcConvertPeriod(2.1, 2050)
#' wcConvertPeriod(2.1, 2070)
#' wcConvertPeriod(2.1, 2090)
#' @export
wcConvertPeriod <- function(ver, period, type = NULL, standardToFile = TRUE) {

	if (ver < 2) {
		out <- if (standardToFile) {
			wcPeriod$wcPeriod$cmip5$short[which(wcPeriod$cmip5$short == period)]
		} else {
			wcPeriod$wcPeriod$cmip5$long[which(wcPeriod$cmip5$short == period)]
		}
	} else if (ver >= 2 & ver < 3) {
		if (type == 'future') {
			out <- if (standardToFile) {
				wcPeriod$wcPeriod$cmip6$long[which(wcPeriod$cmip6$short == period)]
			} else {
				wcPeriod$wcPeriod$cmip6$short[which(wcPeriod$cmip6$short == period)]
			}
		} else if (type == 'decadal') {
			out <- if (standardToFile) {
				wcPeriod$wcPeriod$decadal$short[which(wcPeriod$decadal$short == period)]
			} else {
				wcPeriod$wcPeriod$decadal$long[which(wcPeriod$decadal$short == period)]
			}
		}
	}

	out
	
}
