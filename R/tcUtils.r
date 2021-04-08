#' Get name of TerraClimate variable
#'
#' Given a "standard" name of a TerraClimate climate variable, returns the name of variable as used in files and paths, or given the "file/path" name of a variable, returns the "standard" name. For example, in terraClimate, mean monthly wind speed called "ws". These are the "file" names. The standard name for wind speed (in this package) is "wind". Standard names are:
#' \itemize{
#' 	\item \code{elev}: elevation ("file" name: \code{elev})
#' 	\item \code{tmin}: minimum temperature
#' 	\item \code{tmax}: maximum temperature
#' 	\item \code{ppt}: accumulated precipitation
#' 	\item \code{swe}: snow water equivalent
#' 	\item \code{aet}: actual evapotranspiration
#' 	\item \code{pet}: reference (potential) evapotranspiration (ET0)
#' 	\item \code{def}: climate water deficit
#' 	\item \code{PDSI}: Palmer Drought Severity Index
#' 	\item \code{soil}: extractable soil moisture
#' 	\item \code{srad}: downward shortwave flux at the surface (solar radiation)
#' 	\item \code{wind}: average wind speed (file name: \code{ws})
#' 	\item \code{q}: cumulative streamflow
#' 	\item \code{vap}: vapor pressure
#' 	\item \code{vpd}: vapor pressure deficit
#' }
#' A crosswalk table between standard and file names of variables can be obtained from \code{data(tcVars)}.
#' @param standardToFile If \code{TRUE}, then convert the file format of the variable name to standard format. If \code{FALSE}, the convert the standard format to the file format.
#' @return Character
#' @examples
#'
#' tcConvertVar('tmin', TRUE) # same as input
#' tcConvertVar('ws', FALSE)
#' tcConvertVar('wind', TRUE) # same as input
#' 
#' @export
tcConvertVar <- function(var, standardToFile) {
	
	data(tcVars)
	if (standardToFile) {
		newVar <- tcVars$file[tcVars$standard == var]
	} else {
		newVar <- tcVars$standard[tcVars$file == var]
	}

	newVar
	
}
