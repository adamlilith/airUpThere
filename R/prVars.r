#' Get Names of PRISM variables
#'
#' Get standard or file/URL versions of PRISM variables.
#' @param ver Version of PRISM. Either of \code{'public'} or \code{'subscription'}. Partial matching is used.
#' @param standard If \code{TRUE} (default), the "standard" names are returned. If \code{FALSE}, the file/URL names are returned.
#' @param defs If \code{TRUE}, return general definitions of each variable, too.
#' @details You can see a full list of all variables using \code{data(airVars); airVars}.
#' @return A character vector.
#' @seealso airVars
#' @examples
#' prVars('p')
#' prVars('s')
#' prVars('s', defs=TRUE)
#' @export

prVars <- function(ver, standard=TRUE, defs=FALSE) {

	if (!exists('airVars', inherits=TRUE)) data('airVars', envir=.GlobalEnv)
	
	# period
	choices <- c('public', 'subscription')
	this <- pmatch(ver, choices)
	ver <- choices[this]

	colName <- paste0('prism_', ver)
	index <- which(!is.na(airVars[ , colName]))
	
	out <- if (standard) {
		airVars$standard[index]
	} else {
		airVars[index, colName]
	}
	
	if (defs) out <- data.frame(var=out, definition=airVars$definition[index])
	out
	
}
