#' @name prismCompare
#' @title Compare version of available rasters to rasters already downloaded

#' @description This function uses 

#' The basic input is data frame or data frame-like object, with each row representing a location and having a particular date. The function also needs to be pointed toward a folder with PRISM data. The folder with PRISM data must be structured as:
#' \itemize{
#'		\item Base folder (e.g., \code{'C:/PRISM/an81'} or \code{'C:/PRISM/lt81'}), which contains:
#'		\item A folder named either \code{daily} or \code{monthly}, which contains:
#'		\item One folder per variable (e.g., \code{tmin}, \code{tmax}, \code{vpdmax}), each of which contain:
#'		\item One folder per year (e.g., \code{1981}, code{1982}, etc.), each of which contains any of:
#'		\itemize{
#'			\item One raster per day named like \code{prism_tdmean_us_30s_19810101.bil}, \code{prism_tdmean_us_30s_19810102.bil}, etc.
#'			\item One raster per month named like \code{prism_tdmean_us_30s_198101.bil}, \code{prism_tdmean_us_30s_198101.bil}, etc.
#'			\item One raster representing the yearly value named like \code{prism_tdmean_us_30s_1981.bil}.
#' 		}
#' }
#' \cr

#' The function can extract values corresponding to the day/month/year of each record, plus (optionally) a user-specified window of time prior to the day/month/year of each record. For example, you could use this to extract daily climate data for a site collected on April 22, 2014, and all days prior up to 10 years (April 23, 2004). This function is really a fancy wrapper for \code{\link[terra]{extract}}, but it does the tough job of getting the directory structures right, pulling all needed rasters, and efficiently grouping records to speed extraction. \cr\cr

#' The function does not assume that data for all PRISM years are available, but it does assume that all relevant rasters for a particular year are available within each yearly folder. If rasters preceding a date only partially cover the window, then values for the part covered will be extracted. For example if you try to extract annual values for a window spanning 2010 to 2020 but only have available rasters for 1981 to 2018, then values for 2010 to 2018 will be extracted. Values that cannot be extracted are represented by \code{NA} in the output.

#' @param prismDir Character. Path of the base directory in which the rasters are stored. It must have a structure with subfolders as described above.
#' @param rastVars Name of variable(s) to extract. Valid values are:
#' \itemize{
#'		\item	\code{ppt}: Precipitation.
#'		\item	\code{tmax}: Maximum temperature.
#'		\item	\code{tmin}: Minimum temperature.
#'		\item	\code{tmean}: Mean temperature.
#'		\item	\code{tdmean}: Dew-point temperature.
#'		\item	\code{vpdmax}: Maximum vapor-pressure.
#'		\item	\code{vpdmin}: Minimum vapor-pressure.
#' }
#' @param rastSuffix Character. The "suffix" at the end of each weather raster. PRISM rasters are usually shipped in 'BIL' format, so normally this should be \code{bil}. However, any other suffix corresponding to a raster type that can be opened by the \code{\link[terra]{rast}} function can be used if the rasters have been converted to another format.
#' @param x Data frame.
#' @param dateField Either:
#' \itemize{
#'		\item 	Name of column in \code{x} with date of each record. Values must be of in YYYY-MM-DD (year-month-day of month) format \emph{or} already of class \code{\link{Date}}. See \code{\link[lubridate]{ymd}} or related functions for more help.
#'		\item   Names of columns with year, month, and day of month of each record (in that order). Months must be numeric (i.e., 10, not "October").
#' }
#' @param longLat Character vector with two elements. Names of columns in \code{x} with longitude and latitude (in that order). Coordinates will be assumed to be in the same coordinate reference system as the PRISM rasters.
#' @param windowYears,windowMonths,windowDays Integers >= 0. Extract data for this many years, months, and/or days before the day of each observation, \emph{plus} the day of observation. Note:
#' \itemize{
#'		\item For daily data, only \code{windowYears} and \code{windowDays} are used. Note that the difference between using, say, \code{windowYears = 1} and \code{windowDays = 365} is that the former can accommodate leap days whereas the latter just extracts the 365 days prior (which may be less than a full calendar year if the timespan encompasses a leap day).
#'		\item For monthly data, only \code{windowYears} and \code{windowMonths} are used.
#'		\item For annual data, only \code{windowYears} is used.
#' }
#' To get only data for the day/month/year of each record, set all of the respective \code{window} arguments to 0 (default).
#' @param verbose Logical. If \code{TRUE} (default), show progress.
#' @return Matrix with one row per row in \code{x}. \code{NA} values represent days/months/years that did not fall within the specified window or for which rasters were unavailable. 
#' @examples
#' \dontrun{

#' }

prismDir <- 'D:/ecology/Climate/PRISM/Acquired 2020/an81'

prismCompare <- function(
	prismDir,
	date,
	period = 'monthly',
	vars = 'tmean',
	service = 'http://services.nacse.org/prism/data/subscription/800m/releaseDate/',
	verbose = TRUE
) {

	if (class(date) != 'Date') dates <- lubridate::ymd(date)

	for (thisVar in vars) {

		yr <- lubridate::year(date)
		mo <- lubridate::month(date)
		mo <- sprintf('%02.0f', mo)
		yrmo <- paste0(yr, mo)
		
		### get version from PRISM
		path <- paste(service, thisVar, yrmo, sep='/')
		
		
		
		### get version from raster
		infoPath <- paste0(prismDir, thisVar, 'monthly', year, 'prism_ppt_us_30s_', yrmo, '.info.txt')
		
			
		
	} # next variable

}