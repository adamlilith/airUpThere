#' Get Names of TerraClimate variables
#'
#' Get standard or file/URL versions of TerraClimate variables.
#' @param standard If \code{TRUE} (default), the "standard" names are returned. If \code{FALSE}, the file/URL names are returned.
#' @param defs If \code{TRUE}, return general definitions of each variable, too.
#' @details You can see a full list of all variables using \code{data(varNames); varNames}.
#' @return A character vector.
#' @seealso varNames
#' @examples
#' tcVars(TRUE)
#' tcVars(TRUE, defs=TRUE)
#' tcVars(FALSE)
#' @export

tcVars <- function(standard=TRUE, defs=FALSE) {

	data('varNames', envir = environment())
	
	colName <- 'terraclimate'
	index <- which(!is.na(varNames[ , colName]))
	
	out <- if (standard) {
		varNames$standard[index]
	} else {
		varNames[index, colName]
	}
	
	if (defs) out <- data.frame(var=out, definition=varNames$definition[index])
	out
	
}
