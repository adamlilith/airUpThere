#' Get Names of PRISM variables
#'
#' Get standard or file/URL versions of PRISM variables.
#' @param ver Version of PRISM. Either of \code{'public'} or \code{'subscription'}. Partial matching is used.
#' @param standard If \code{TRUE} (default), the "standard" names are returned. If \code{FALSE}, the file/URL names are returned.
#' @param defs If \code{TRUE}, return general definitions of each variable, too.
#' @details You can see a full list of all variables using \code{data(varNames); varNames}.
#' @return A character vector.
#' @seealso varNames
#' @examples
#' prVars('p')
#' prVars('s')
#' prVars('s', defs=TRUE)
#' @export

prVars <- function(ver, standard=TRUE, defs=FALSE) {

	data('varNames', envir = environment())
	
	# period
	choices <- c('public', 'subscription')
	this <- pmatch(ver, choices)
	ver <- choices[this]

	colName <- paste0('prism_', ver)
	index <- which(!is.na(varNames[ , colName]))
	
	out <- if (standard) {
		varNames$standard[index]
	} else {
		varNames[index, colName]
	}
	
	if (defs) out <- data.frame(var=out, definition=varNames$definition[index])
	out
	
}
