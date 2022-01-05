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

	src <- tolower(src)
	if (!is.null(ver)) ver <- tolower(as.character(ver))
	if (!is.null(period)) period <- tolower(as.character(period))
	what <- tolower(what)
	
	sourceCol <- if (any(src %in% airUrls$src)) {
		airUrls$src
	} else if (any(src %in% airUrls$source)) {
		airUrls$source
	} else {
		stop('This is an invalid name/abbreviation for a climate data source. See "airUrls" for valid values.')
	}
	
	
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
