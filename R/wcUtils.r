#' Check if this scenario exists for WorldClim future version
#' @param cmip 5 or 6
#' @param scenario Valid values for CMIP5 (RCPs) are one or more of 26, 45, 60, and/or 85.  Valid values for SSPs (CMIP6) are one or more of 126, 245, 370, and/or 585.
#' @return Logical
#' @keywords internal
wcCheckGhg_internal <- function(ver, scenario) {

	if (ver == 1.4) {
		if (!scenario %in% c(26, 45, 60, 85)) {
			stop('This is not a valid scenario for CMIP5.')
		}
	} else if (ver == 2.1) {
		if (!scenario %in% c(126, 245, 370, 585)) {
			stop('This is not a valid scenario for CMIP6.')
		}
	} else {
		stop('This is an invalid version number for WorldClim.')
	}
	
	TRUE
	
}

#' Check WorldClim version number
#' @param ver 1.4 or 2.1
#' @return Logical
#' @keywords internal
wcCheckVer_internal <- function(ver) {
	
	if (!(ver %in% c(1.4, 2.1))) {
		stop('This is an invalid version number for WorldClim.')
	}
	TRUE
}

#' Check and get ESM for WorldClim
#' @param ver 1.4 or 2.1
#' @param esm Character
#' @return Character
#' @keywords internal
wcGetEsm_internal <- function(ver, esm) {

	data('wcEsm', envir = environment())
	
	if (ver == 1.4) {
		if (nchar(esm) == 2) {
			esm <- tolower(esm)
			if (!(esm %in% tolower(wcEsm$cmip5$shortNames))) {
				stop('This is an invalid ESM for WorldClim 1.4.')
			}
		} else if (nchar(esm) > 2) {
			esm <- wcEsm$cmip5$shortNames[toupper(esm) == wcEsm$cmip5$longNames]
			esm <- tolower(esm)
			if (!(esm %in% tolower(wcEsm$cmip5$shortNames))) {
				stop('This is an invalid ESM for WorldClim 1.4.')
			}
		} else {
			stop('This is an invalid ESM for WorldClim 1.4.')
		}
	} else if (ver == 2.1) {
		if (nchar(esm) == 2) {
			esm <- tolower(esm)
			if (!(esm %in% tolower(wcEsm$cmip6$shortNames))) {
				stop('This is an invalid ESM for WorldClim 2.1.')
			}
		} else if (nchar(esm) > 2) {
			esm <- wcEsm$cmip6$shortNames[toupper(esm) == wcEsm$cmip6$longNames]
			esm <- tolower(esm)
			if (!(esm %in% tolower(wcEsm$cmip6$shortNames))) {
				stop('This is an invalid ESM for WorldClim 2.1.')
			}
		} else {
			stop('This is an invalid ESM for WorldClim 2.1.')
		}
	} else {
		stop('This is an invalid version number for WorldClim.')
	}
	
	esm
		
}

#' Nice name for GHG WorldClim scenarios
#' 
#' Create a nice name for a WorldClim future climate scenario.
#' @param ver Version number: 1.4 or 2.1
#' @param ghg Greenhouse gas emissions scenario. This is the RCP or SSP number (without a decimal point, for RCPs).
#' @return Character
#' @examples
#' wcNiceGhg(1.4, 45)
#' wcNiceGhg(1.4, 85)
#' wcNiceGhg(2.1, 126)
#' wcNiceGhg(2.1, 370)
#' @export
wcNiceGhg <- function(ver, ghg) {

	if (ver == 1.4) {
		ghg <- as.character(ghg)
		ghg <- paste0(substr(ghg, 1, 1), '.', substr(ghg, 2, 2))
	}
	ghgNice <- if (ver == 1.4) {
		paste('RCP', ghg)
	} else if (ver == 2.1) {
		paste('SSP', ghg)
	} else {
		stop('This is an invalid version number for WorldClim.')
	}
	
	ghgNice
	
}

#' Name of resolution in WorldClim URL and file names
#' 
#' Return the name of a resolution as used in WorldClim URLs and file names.
#' @param ver 1.4 or 2.1
#' @param res 10, 5, 2.5, or 30
#' @param period For WC 1.4, this is either \code{'historical'} or \coed{'future'}. For WC 2.1 this is \code{NULL}.
#' @return Character
#' @examples
#' wcGetRes(1.4, 2.5, 'historical')
#' wcGetRes(1.4, 2.5, 'future')
#' wcGetRes(2.1, 2.5)
#' wcGetRes(2.1, 2.5)
#' @export
wcGetRes <- function(ver, res, period = NULL) {

	resUnit <- if (ver == 1.4) {
		if (period == 'historical') {
			if (res == 10) {
				'10m'
			} else if (res == 5) {
				'5m'
			} else if (res == 2.5) {
				'2-5m'
			} else if (res == 30) {
				'30s'
			} else {
				stop('This is not a valid resolution for WorldClim 1.4.')
			}
		} else if (period == 'future') {
			if (res == 10) {
				'10m'
			} else if (res == 5) {
				'5m'
			} else if (res == 2.5) {
				'2_5m'
			} else if (res == 30) {
				'30s'
			} else {
				stop('This is not a valid resolution for WorldClim 1.4.')
			}
		}

	} else if (ver == 2.1) {
		if (res == 10) {
			'10m'
		} else if (res == 5) {
			'5m'
		} else if (res == 2.5) {
			'2.5m'
		} else if (res == 30) {
			'30s'
		} else {
			stop('This is not a valid resolution for WorldClim 1.4.')
		}
	} else {
		stop('This is an invalid version number for WorldClim.')
	}
	
	resUnit
	
}

#' Name of future time period in WorldClim URL and file names
#'
#' Return the name of a future time WorldClim time period in the format used in URLs and file names.
#' @param ver WorldClim version: 1.4 or 2.1.
#' @param period Year indicating time period.
#' @param type Either of \code{future} or \code{decadal}. Only needed for WC 2.1.
#' @return Character
#' @examples
#' wcGetPeriod(1.4, 2050)
#' wcGetPeriod(1.4, 2070)
#' wcGetPeriod(2.1, 2030)
#' wcGetPeriod(2.1, 2050)
#' wcGetPeriod(2.1, 2070)
#' wcGetPeriod(2.1, 2090)
#' @export
wcGetPeriod <- function(ver, period, type = NULL) {

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
		stop('This is an invalid verion.')
	}
	
	yearCode
	
}

