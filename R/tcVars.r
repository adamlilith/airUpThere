#' Get Names of TerraClimate variables
#'
#' Get standard or file/URL versions of TerraClimate variables.
#' @param standard If \code{TRUE} (default), the "standard" names are returned. If \code{FALSE}, the file/URL names are returned.
#' @param defs If \code{TRUE}, return general definitions of each variable, too.
#' @details You can see a full list of all variables using \code{data(airVars); airVars}.
#' @return A character vector.
#' @seealso airVars
#' @examples
#' tcVars(TRUE)
#' tcVars(TRUE, defs=TRUE)
#' tcVars(FALSE)
#' @export

tcVars <- function(standard=TRUE, defs=FALSE) {

	if (!exists('airVars', inherits=TRUE)) data('airVars', envir=.GlobalEnv)
	
	colName <- 'terraclimate'
	index <- which(!is.na(airVars[ , colName]))
	
	out <- if (standard) {
		airVars$standard[index]
	} else {
		airVars[index, colName]
	}
	
	if (defs) out <- data.frame(var=out, definition=airVars$definition[index])
	out
	
}
