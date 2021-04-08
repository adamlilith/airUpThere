#' Check if this scenario exists for WorldClim future version
#' @param cmip 5 or 6
#' @param scenario Valid values for CMIP5 (RCPs) are one or more of 26, 45, 60, and/or 85.  Valid values for SSPs (CMIP6) are one or more of 126, 245, 370, and/or 585.
#' @return Logical
#' @keywords internal
wcCheckGhg <- function(ver, scenario) {

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

# wcVar
	# if (ver == 1.4) {
		# if (time == 'historical') {
			# if (!(var %in% c('tmin', 'tmax', 'tmean', 'bio'))) {
				# if (var == 'prec') {
					# 'ppt'
				# } else {
					# stop('Valid variable names for WorldClim 1.4 historical climate include "tmin", "tmax", "tmean", "prec", and "bio".')
				# }
			# }
		# } else if (time == 'future') {
			# if (var == 'tmin') {
				# 'tn'
			# } else if (var == 'tx'
		
			# if (!(var %in% c('tn', 'tx', 'pr', 'bi'))) {
				# stop('Valid variable names for WorldClim 1.4 future climate include "tn", "tx", "pr", and "bi".')
			# }
		# }
	# } else if (ver == 2.1) {
		# if (time == 'historical') {
			# if (!(var %in% c('tmin', 'tmax', 'tmean', 'prec', 'srad', 'wind', 'vapr', 'bio', 'elev'))) {
				# stop('Valid variable names for WorldClim 2.1 historical climate include "tmin", "tmax", "tmean", "prec", "srad", "wind", "vapr", "bio", and "elev".')
			# }
		# } else if (time == 'future') {
			# if (!(var %in% c('tmin', 'tmax', 'prec', 'bioc'))) {
				# stop('Valid variable names for WorldClim 2.1 future climate include "tmin", "tmax", "prec", and "bioc".')
			# }
		# }
	# } else {
		# stop('This is an invalid version number for WorldClim.')
	# }
	
	# TRUE
# }

#' Check WorldClim version number
#' @param ver 1.4 or 2.1
#' @return Logical
#' @keywords internal
wcCheckVer <- function(ver) {
	
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
wcGetEsm <- function(ver, esm) {

	data(wcEsm)
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
		if (!(esm %in% wcEsm$cmip6$longNames)) {
			stop('This is an invalid ESM for WorldClim 2.1.')
		}
	} else {
		stop('This is an invalid version number for WorldClim.')
	}
	
	esm
		
}

#' Get name of WorldClim variable
#'
#' Given a "standard" name of a WorldClim climate variable, returns the name of variable as used in files and paths, or given the "file/path" name of a variable, returns the "standard" name. For example, in WorldClim 1.4, minimum temperature is called "tmin" in historical rasters and "tn" in future rasters. These are the "file" names. The standard name for minimum temperature (in this package) is "tmin". Standard names are:
#' \itemize{
#' 	\item \code{tmin}: minimum temperature (available for all)
#' 	\item \code{tmax}: maximum temperature (available for all)
#' 	\item \code{tmean}: mean temperature (available for WC 1.4 and 2.1 historical)
#' 	\item \code{ppt}: precipitation (available for all)
#' 	\item \code{bio}: BIOCLIM variables (available for all)
#' 	\item \code{srad}: solar radiation (available for WC 2.1 historical)
#' 	\item \code{wind}: average wind speed (available for WC 2.1 historical)
#' 	\item \code{vapr}: vapor pressure deficit (available for WC 2.1 historical)
#' 	\item \code{elev}: elevation (available for WC 2.1 historical)
#' }
#' A crosswalk table between standard and file names of variables can be obtained from \code{data(wcVars)}.
#' @param ver Version of WorldClim: 1.4 or 2.1
#' @param var Name of variable in standard or file format.
#' @param time \code{'historical'} or \code{'future'}.
#' @param standardToFile If \code{TRUE}, then convert the file format of the variable name to standard format. If \code{FALSE}, the convert the standard format to the file format.
#' @return Character
#' @examples
#'
#' wcConvertVar(1.4, 'ppt', 'historical', TRUE)
#' wcConvertVar(1.4, 'prec', 'historical', FALSE)
#'
#' wcConvertVar(1.4, 'ppt', 'future', TRUE)
#' wcConvertVar(1.4, 'pr', 'future', FALSE)
#'
#' wcConvertVar(2.1, 'ppt', 'historical', TRUE)
#' wcConvertVar(2.1, 'prec', 'historical', FALSE)
#'
#' wcConvertVar(2.1, 'ppt', 'future', TRUE)
#' wcConvertVar(2.1, 'prec', 'future', FALSE)
#' 
#' @export
wcConvertVar <- function(ver, var, time, standardToFile) {
	
	data(wcVars)
	if (standardToFile) {
		newVar <- wcVars[wcVars$ver==ver & wcVars$time==time, names(wcVars)==var]
	} else {
		fileNames <- unlist(wcVars[wcVars$ver == ver & wcVars$time==time, , drop=TRUE])
		newVar <- names(fileNames)[which(fileNames == var)]
	}

	newVar
	
}

#' Nice name for GHG WorldClim scenario
#' @param ver 1.4 or 2.1
#' @param ghg 
#' @return Character
#' @keyrords internal
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
#' @param ver 1.4 or 2.1
#' @param res 10, 5, 2.5, or 30
#' @param time 'historical' or 'future'
#' @return Character
#' @keywords internal
wcGetRes <- function(ver, res, time) {

	resUnit <- if (ver == 1.4) {
		if (time == 'historical') {
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
		} else if (time == 'future') {
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

#' Name of period in WorldClim URL and file names
#' @param cmip 5 or 6
#' @return Character
#' @keywords internal
wcGetYear <- function(ver, year) {

	yearCode <- if (ver == 1.4) {
		if (year == 2050) {
			'50'
		} else if (year == 2070) {
			'70'
		} else {
			stop('This is not a valid year for CMIP5.')
		}
	} else if (ver == 2.1) {
		if (year == 2030) {
			'2021-2040'
		} else if (year == 2050) {
			'2041-2060'
		} else if (year == 2070) {
			'2061-2080'
		} else if (year == 2090) {
			'2081-2100'
		} else {
			stop('This is not a valid year for CMIP6.')
		}
	} else {
		stop('This is an invalid CMIP.')
	}
	
	yearCode
	
}

