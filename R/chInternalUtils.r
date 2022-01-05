#' Check ESM for CHELSA
#' @param cmip CMIP (5 or 6)
#' @param esm Name of the ESM. Case-sensitive!
#' @return \code{TRUE} (invisible) or an error will be called.
#' @keywords internal
chCheckEsm_internal <- function(cmip, esm) {

	valids <- chEsm(cmip)
	ok <- esm %in% valids
	
	if (!ok) {
	
		cat(paste0('These ESMs are not valid for CHELSA CMIP ', cmip, ':\n'))
		cat(paste(esm[!ok], collapse=' '), '\n')
		stop('See chEsm() for valid ESM names.')
	
	}

	invisible(TRUE)
	
}

#' Are periods in short or long format?
#' @param period
#' @return \code{TRUE} (invisible) or an error will be called.
#' @keywords internal
chIsPeriodLong_internal <- function(period) {

	long <- grepl(period, pattern='-')
	all(long)
	
}
