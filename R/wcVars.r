#' Get Names of WorldClim variables
#'
#' Get standard or file/URL versions of WorldClim variables.
#' @param ver WorldClim version number: 1.4 or 2.1.
#' @param period Any of \code{'historical'}, \code{'future'}, or \code{'decadal'}  Partial matching is supported.
#' @param standard If \code{TRUE} (default), the "standard" names are returned. If \code{FALSE}, the file/URL names are returned.
#' @param defs If \code{TRUE}, return general definitions of each variable, too.
#' @details You can see a full list of all variables using \code{data(airVars); airVars}.
#' @return A character vector.
#' @seealso airVars
#' @examples
#' wcVars(1.4, 'historical')
#' wcVars(1.4, 'historical', FALSE)
#' wcVars(1.4, 'historical', defs=TRUE)
#'
#' wcVars(1.4, 'future')
#' wcVars(1.4, 'future', FALSE)
#' 
#' wcVars(2.1, 'historical')
#' wcVars(2.1, 'historical', FALSE)
#' wcVars(2.1, 'future')
#' wcVars(2.1, 'future', FALSE)
#' 
#' wcVars(2.1, 'decadal')
#' 
#' @export
wcVars <- function(ver, period, standard=TRUE, defs=FALSE) {

	wcCheckVer_internal(ver)

	if (!exists('airVars', inherits=TRUE)) data('airVars', envir=.GlobalEnv)
	
	# period
	choices <- c('historical', 'future', 'decadal')
	this <- pmatch(period, choices)
	period <- choices[this]

	colName <- paste0('worldclim_', ver, '_', period)
	index <- which(!is.na(airVars[ , colName]))
	
	out <- if (standard) {
		airVars$standard[index]
	} else {
		airVars[index, colName]
	}
	
	if (defs) out <- data.frame(var=out, definition=airVars$definition[index])
	out
	
}

