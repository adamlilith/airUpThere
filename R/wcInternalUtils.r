#' Check if this scenario exists for WorldClim future version
#' @param cmip 5 or 6
#' @param scenario Valid values for CMIP5 (RCPs) are one or more of 26, 45, 60, and/or 85.  Valid values for SSPs (CMIP6) are one or more of 126, 245, 370, and/or 585.
#' @return Logical
#' @keywords internal
wcCheckGhg_internal <- function(ver, scenario) {

	data(ghgNames, envir=environment())
	if (ver == 1.4) {
		if (!scenario %in% ghg$cmip5) {
			stop('This is not a valid scenario for CMIP5.')
		}
	} else if (ver == 2.1) {
		if (!scenario %in% ghg$cmip6) {
			stop('This is not a valid scenario for CMIP6.')
		}
	} else {
		stop('This is an invalid version number for WorldClim.')
	}
	
	invisible(TRUE)
	
}

#' Check WorldClim version number
#' @param ver 1.4 or 2.1
#' @return Logical
#' @keywords internal
wcCheckVer_internal <- function(ver) {
	
	if (!(ver %in% c(1.4, 2.1))) {
		stop('This is an invalid version number for WorldClim.')
	}
	invisible(TRUE)
}

#' Check and get ESM for WorldClim
#' @param ver 1.4 or 2.1
#' @param esm Character
#' @return Character
#' @keywords internal
wcGetEsm_internal <- function(ver, esm) {

	data('wcEsm', envir = environment())
	
	theseWcEsm <- wcEsm(ver)
	
	if (ver == 1.4) {
		
		theseWcEsm$long <- tolower(theseWcEsm$long)
		theseWcEsm$short <- tolower(theseWcEsm$short)
		
		esm <- tolower(esm)
		if (nchar(esm) == 2) {
			if (!(esm %in% theseWcEsm)) {
				stop('This is an invalid ESM for WorldClim 1.4.')
			} else {
				out <- theseWcEsm$short[esm == theseWcEsm$short]
			}
		} else if (nchar(esm) > 2) {
			out <- theseWcEsm$short[esm == theseWcEsm$long]
		} else {
			stop('This is an invalid ESM for WorldClim 1.4.')
		}
		
		out <- tolower(out)
		
	} else if (ver == 2.1) {
	
		esm <- toupper(esm)
		if (nchar(esm) == 2) {
			if (!(esm %in% theseWcEsm$short)) {
				stop('This is an invalid ESM for WorldClim 2.1.')
			} else {
				out <- theseWcEsm$long[esm == theseWcEsm$short]
			}
		} else if (nchar(esm) > 2) {
			if (!(esm %in% theseWcEsm$long)) {
				stop('This is an invalid ESM for WorldClim 2.1.')
			} else {
				out <- theseWcEsm$long[esm == theseWcEsm$long]
			}
		} else {
			stop('This is an invalid ESM for WorldClim 2.1.')
		}
	} else {
		stop('This is an invalid version number for WorldClim.')
	}
	
	out
		
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
