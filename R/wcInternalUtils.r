#' Check if this scenario exists for WorldClim future version
#' @param cmip 5 or 6
#' @param scenario Valid values for CMIP5 (RCPs) are one or more of 26, 45, 60, and/or 85.  Valid values for SSPs (CMIP6) are one or more of 126, 245, 370, and/or 585.
#' @return Logical
#' @keywords internal
wcCheckGhg_internal <- function(ver, scenario) {

	# data('ghgs', envir=environment())
	if (ver < 2) {
		scenario <- scenario / 10
		if (!scenario %in% ghgs$cmip5$rcp) {
			stop('This is not a valid scenario for CMIP5.')
		}
	} else if (ver >= 2 & ver < 3) {
		if (!scenario %in% ghgs$cmip6$ssp) {
			stop('This is not a valid scenario for CMIP6.')
		}
	} else {
		stop('This is an invalid version number for WorldClim.')
	}
	
	invisible(TRUE)
	
}

#' Check WorldClim version number
#' @param ver WorldCli version number (e.g., 1.4, 2.1, etc.)
#' @return Logical
#' @keywords internal
wcCheckVer_internal <- function(ver) {
	
	if (!(ver %in% airUrls$ver[airUrls$src == 'wc'])) {
		stop('This is an invalid version number for WorldClim.')
	}
	invisible(TRUE)
}


#' Check ESM for WorldClim
#' @param ver Version number (e.g., 1.4, 2.1)
#' @param esm Character, either short (2-letter) or long ESM names (case will not matter). Types cannot be mixed (i.e., all names must be 2-characters in length or long names).
#' @return \code{TRUE} (invisible) or an error will be called.
#' @keywords internal
wcCheckEsm_internal <- function(ver, esm) {

	esm <- toupper(esm)
	
	if (ver < 2) {
		if (all(nchar(esm) == 2)) {
			ins <- esm %in% toupper(wcEsm$cmip5$short)
			if (!all(ins)) {
				cat(paste0('These ESMs are invalid for WorldClim ', ver, ': ', paste(esm[!ins], sep=' ')), '\n')
			}
		} else {
			ins <- esm %in% toupper(wcEsm$cmip5$long)
			if (!all(ins)) {
				cat(paste0('These ESMs are invalid for WorldClim ', ver, ': ', paste(esm[!ins], sep=' ')), '\n')
			}
		}
	} else if (ver >= 2 & ver < 3) {
		if (all(nchar(esm) == 2)) {
			ins <- esm %in% toupper(wcEsm$cmip6$short)
			if (!all(ins)) {
				cat(paste0('These ESMs are invalid for WorldClim ', ver, ': ', paste(esm[!ins], sep=' ')), '\n')
			}
		} else {
			ins <- esm %in% toupper(wcEsm$cmip6$long)
			if (!all(ins)) {
				cat(paste0('These ESMs are invalid for WorldClim ', ver, ': ', paste(esm[!ins], sep=' ')), '\n')
			}
		}
	}
	
	invisible(TRUE)

}

#' Check and get ESM for WorldClim
#' @param ver 1.4 or 2.1
#' @param esm Character
#' @return Character
#' @keywords internal
wcConvertEsm_internal <- function(ver, esm) {

	esm <- toupper(esm)
	
	if (ver < 2) {
		esmFile <- if (all(nchar(esm) == 2)) {
			wcEsm$cmip5$short[toupper(wcEsm$cmip5$short) %in% esm]
		} else {
			wcEsm$cmip5$short[toupper(wcEsm$cmip5$long) %in% esm]
		}
		esmFile <- tolower(esmFile)
	} else if (ver >= 2 & ver < 3) {
		esmFile <- if (all(nchar(esm) == 2)) {
			wcEsm$cmip6$long[toupper(wcEsm$cmip6$short) %in% esm]
		} else {
			wcEsm$cmip6$long[toupper(wcEsm$cmip6$long) %in% esm]
		}
		# esmFile <- toupper(esmFile)
	}
	
	esmFile
		
}


#'Check name of future time period for WorldClim
#'
#' @param ver WorldClim version
#' @param period Year indicating time period (in "standard" format, like '2050' or '1960')
#' @param type Either of \code{future} or \code{decadal}. Only needed for WC 2.1.
#' @return Character
#' @examples
#' wcCheckPeriod_internal(1.4, 2050)
#' wcCheckPeriod_internal(1.4, 2070)
#' wcCheckPeriod_internal(2.1, 2050, 'future')
#' wcCheckPeriod_internal(2.1, 2111, 'future')
#' wcCheckPeriod_internal(2.1, 1960, 'decadal')
#' @export
wcCheckPeriod_internal <- function(ver, period, type = NULL) {

	if (ver < 2) {
		ins <- period %in% wcPeriod$cmip5$long
		if (!(all(ins))) stop('Invalid period(s) for WorldClim ', ver, ': ', paste(period[!ins], sep=' '))
	} else if (ver >= 2 & ver < 3) {
		if (type == 'future') {
			ins <- period %in% wcPeriod$cmip6$short
			if (!(all(ins))) stop('Invalid period(s) for WorldClim ', ver, ' ', type, ': ', paste(period[!ins], sep=' '))
		} else if (type == 'decadal') {
			ins <- period %in% wcPeriod$decadal$short
			if (!(all(ins))) stop('Invalid period(s) for WorldClim ', ver, ' ', type, ': ', paste(period[!ins], sep=' '))
		}
	}

	invisible(TRUE)
	
}

#' Check to see if variables are valid WC variables
#'
#' Check to see if variables are valid WC variables
#' @param ver WC version
#' @param vars Variables in standard format
#' @param period "historical", "future", "decadal" Partial matching OK.
#' @return \code{TRUE} or fails
#' @keywords internal
wcCheckVars_internal <- function(ver, vars, period) {

	# period
	choices <- c('historical', 'future', 'decadal')
	this <- pmatch(period, choices)
	period <- choices[this]

	valids <- wcVars(ver=ver, period=period, standard=TRUE)
	oks <- vars %in% valids
	if (all(oks)) {
		invisible(TRUE)
	} else {
		bads <- vars[!oks]
		stop('These are invalid WorldClim ', ver, ' ', period, ' variables: ', paste(bads, sep=' '))
	}
}
