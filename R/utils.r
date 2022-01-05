#' Fill in wildcards in file/URL names
#'
#' This function replaces wildcards in generic file/URL names with the proper values. It first gathers any named wildcards (e.g., "varFile", "esmFile"), then for each one that occurs in a file/URL, name, replaces it with the value of the named object. The function basically translates generic names in \code{airUrls} to useful file names or URLs.
#' @param x Character (usually a URL or file name)
#' @param ... Named arguments of values to fill in. Must be named any of: \code{ver}, \code{varFile}, \code{resFile}, \code{periodFile}, \code{ghgFile}, \code{esmFile}.
#' @return Character.
#' @example
#' 
#' f1 <- function() {
#' 	
#' 	varFile <- 'abcdefg'
#' 	
#' 	x <- '<<varFile>>_example'
#' 	fillPattern_internal(x)
#' 	
#' }
#' 
#' out <- f1()
#' 
#' @keywords internal
fillPattern_internal <- function(x) {

	# construct list of file/URL parameters that exist in calling function/environment
	wildcardsToReplace <- c(
		'ver',
		'varFile',
		'resFile',
		'periodFile',
		'ghgFile',
		'esmFile'
	)
	
	wilds <- list()
	for (i in seq_along(wildcardsToReplace)) {

		v <- wildcardsToReplace[i]
		if (exists(v, envir=parent.frame())) {
			val <- get(v, envir=parent.frame())
			wilds <- c(wilds, DUMMY=val)
			names(wilds)[length(wilds)] <- v
		}
		
	}
	
	# replace wildcards
	if (length(wilds) > 0) {
		for (i in seq_along(wilds)) {
		
			wildName <- names(wilds)[i]
			wild <- wilds[[i]]
		
			if (grepl(x, pattern=wildName)) {
				x <- gsub(x, pattern=paste0('<<', wildName, '>>'), replacement=wild)
			}
		}
	}
	
	x
}

#' Get URL and file pattern name for a data source
#'
#' This function gets the appropriate URL and file name pattern for a given climate data source. Typically it is called within an \pkg{airUpThere} function.
#'
#' @param what Either \code{rastUrl}, \code{rastFilePattern}, \code{timestampFileName}, or \code{timestampFilePattern}.
#' @param src Name or abbreviation for climate data source
#' @param ver Version number, name, or \code{NULL}.
#' @param period Character or year indicating time period from which data is to be downloaded, or \code{NULL}.
#' @param elevation \code{TRUE} or \code{FALSE}.
#'
#' @return URL or file name pattern
#' @examples
getFileOrURL_internal <- function(
	what,
	src,
	ver = NULL,
	period = NULL,
	elevation = FALSE
) {

	# type conversions
	src <- tolower(src)
	if (!is.null(ver)) ver <- tolower(as.character(ver))
	if (!is.null(period)) period <- tolower(as.character(period))
	what <- tolower(what)
	
	# source column
	sourceCol <- if (any(src %in% airUrls$src)) {
		airUrls$src
	} else if (any(src %in% airUrls$source)) {
		airUrls$source
	} else {
		stop('This is an invalid name/abbreviation for a climate data source. See "airUrls" for valid values.')
	}
	
	# which row?
	this <- if (is.null(ver) & is.null(period)) {
		which(src == sourceCol & elevation == airUrls$elevation)
	} else if (!is.null(ver) & is.null(period)) {
		which(src == sourceCol & ver == airUrls$ver & elevation == airUrls$elevation)
	} else if (is.null(ver) & !is.null(period)) {
		which(src == sourceCol & period == airUrls$period & elevation == airUrls$elevation)
	} else if (!is.null(ver) & !is.null(period)) {
		which(src == sourceCol & ver == airUrls$ver & period == airUrls$period & elevation == airUrls$elevation)
	} else {
		stop('Invalid value(s) for arguments "ver" and/or "period".')
	}

	# get the name
	out <- if (what == tolower('rastUrl')) {
		airUrls$rastUrl[this]
	} else if (what == tolower('timestampURL')) {
		airUrls$timestampUrl[this]
	} else if (what == tolower('rastFilePattern')) {
		airUrls$rastFilePattern[this]
	} else if (what == tolower('timestampFilePattern')) {
		airUrls$timestampFilePattern[this]
	} else {
		stop('Argument "what" must be "rastUrl", "timestampUrl", "rastFilePattern", or "timestampFilePattern".')
	}
	
	out

}

