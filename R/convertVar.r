#' Convert between "standard" and "file/URL" names of variables
#'
#' This function converts "standard" and "file/URL" names of variables. Being able to do this assists with analyses that use the same variables from different data sets. Names can even vary within a data source. For example, WorldClim 2.1 uses "bio" to refer to BIOCLIM variables in files representing historical climate, but "bioc" in files representing future climate. This function provides a crosswalk between the source/version names used in filenames and URLs and a "standard" name. You can obtain a table of standard and file-name versions of variables using \code{data(airVars)}.
#'
#' @param src Name of the source of the climate data. Case is ignored. Valid names include:
#' \itemize{
#'		\item \code{CHELSA}: CHELSA
#'		\item \code{PRISM}: PRISM
#'		\item \code{tc}: TerraClimate
#'		\item \code{wc}: WorldClim
#' }
#' @param vars Name(s) of the variable to convert in "standard" or "file/URL" format. See \code{data(airVars)} for valid values.
#' @param ver Version number of the climate source. Valid values depend on the data source:
#' \itemize{
#'		\item CHELSA: \code{1.0}
#'		\item PRISM: \code{NULL} (no version required)
#'		\item TerraClimate: \code{NULL} (no version required)
#'		\item WorldClim: \code{1.4} or \code{2.1}
#' }
#' @param period Time period of the data source. Valid values depend on the data source, and values are case-insensitive:
#' \itemize{
#'		\item CHELSA: \code{TraCE21K} paleoclimate (0 to 21 Kybp)
#'		\item PRISM: \code{NULL} (no period period required)
#'		\item TerraClimate: \code{NULL} (no period period required)
#'		\item WorldClim: \code{historical} or \code{future} (partial matching is supported)
#' }
#' @param standardToFile If \code{TRUE}, then convert the file format of the variable name to standard format. If \code{FALSE}, the convert the standard format to the file format.
#'
#' @return Character.
#' @examples
#'
#' 
#' # CHELSA
#' convertVar('chelsa', vars=c('tmin', 'tmax', 'ppt'), ver=1.0,
#' period='trace21k', standardToFile=TRUE)
#' convertVar('chelsa', vars=c('tasmin', 'tasmax', 'pr'), ver=1.0,
#' period='trace21k', standardToFile=FALSE)
#' 
#' # PRISM: file and standard names are the same
#' convertVar('prism', vars=c('tmin', 'tmax', 'ppt'), standardToFile=TRUE)
#' convertVar('prism', vars=c('tmin', 'tmax', 'ppt'), standardToFile=FALSE)
#' 
#' # TerraClimate
#' convertVar('tc', vars=c('tmin', 'tmax', 'ppt', 'awd', 'soilWater'),
#' 	standardToFile=TRUE)
#' 
#' convertVar('tc', vars=c('tmin', 'tmax', 'ppt', 'def', 'soil'),
#' 	standardToFile=FALSE)
#' 
#' # WorldClim: file and standard names vary by version and period period
#' convertVar('wc', vars=c('tmin', 'tmax', 'ppt', 'bio'), ver=1.4,
#' 	period='historical', standardToFile=TRUE)
#' convertVar('wc', vars=c('tmin', 'tmax', 'ppt', 'bio'), ver=1.4,
#' 	period='future', standardToFile=TRUE)
#' 
#' convertVar('wc', vars=c('tmin', 'tmax', 'prec', 'bio'), ver=1.4,
#' 	period='historical', standardToFile=FALSE)
#' convertVar('wc', vars=c('tn', 'tx', 'pr', 'bi'), ver=1.4,
#' 	period='future', standardToFile=FALSE)
#' 
#' convertVar('wc', vars=c('tmin', 'tmax', 'ppt', 'bio'), ver=2.1,
#' 	period='historical', standardToFile=TRUE)
#' convertVar('wc', vars=c('tmin', 'tmax', 'ppt', 'bio'), ver=2.1,
#' 	period='future', standardToFile=TRUE)
#' 
#' convertVar('wc', vars=c('tmin', 'tmax', 'prec', 'bio'), ver=2.1,
#' 	period='historical', standardToFile=FALSE)
#' convertVar('wc', vars=c('tmin', 'tmax', 'prec', 'bioc'), ver=2.1,
#' 	period='future', standardToFile=FALSE)
#' 
#' @export

convertVar <- function(
	src,
	vars,
	ver = NULL,
	period = NULL,
	standardToFile = TRUE
) {

	src <- tolower(src)

	# standardize source name
	src <- if (any(c('worldclim', 'wc') %in% src)) {
		'worldclim'
	} else if (any(c('terraclimate', 'tc') %in% src)) {
		'terraclimate'
	} else if (any(c('ch', 'chelsa') %in% src)) {
		'chelsa'
	} else if (any('prism' %in% src)) {
		'prism'
	}
	
	if (is.na(src)) stop('Invalid data source.')

	# source-specific periods
	if (src == 'worldlcim') {
		times <- c('historical', 'future')
		period <- times[pmatch(period, c('historical', 'future'), NA)]
	}

	# column name for airVars
	fileUrlName <- if (!is.null(ver) & !is.null(period)) {
		paste(src, ver, period, sep='_')
	} else if (!is.null(ver) & is.null(period)) {
		paste(src, ver, sep='_')
	} else if (is.null(ver) & !is.null(period)) {
		paste(src, period, sep='_')
	} else {
		src
	}

	# data('airVars', envir = environment())
	out <- if (standardToFile) {
		airData$airVars[match(vars, airData$airVars$standard), fileUrlName]
	} else if (!standardToFile) {
		airData$airVars$standard[match(vars, airData$airVars[ , fileUrlName])]
	} else {
		NA
	}

	out

}
