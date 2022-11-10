#' Create a raster stack from PRISM rasters

#' This function creates a raster stack from select PRISM rasters. The stack consists of one or more variables across one or more time periods.
#' @param prDir Character. Path of the base directory in which the rasters are stored.
#' @param vars Name of variable(s) to stack. Valid values are:
#' \itemize{
#'		\item	\code{ppt}: Precipitation.
#'		\item	\code{tmax}: Maximum temperature.
#'		\item	\code{tmin}: Minimum temperature.
#'		\item	\code{tmean}: Mean temperature.
#'		\item	\code{tdmean}: Dew-point temperature.
#'		\item	\code{vpdmax}: Maximum vapor-pressure.
#'		\item	\code{vpdmin}: Minimum vapor-pressure.
#' }
#' @param dates This specifies either specific dates or a range of dates across which to stack rasters. Either way, dates are specified as objects of class \code{Date} or as characters naming dates in YYYY-MM-DD format.
#' \itemize{
#'		\item	If \code{span} is \code{FALSE} (default), one or more dates can be specified, and the rasters matching the these dates will be stacked.
#'		\itemize{
#'			\item If \code{by} is \code{day}, then daily rasters will be obtained.
#'			\item If \code{by} is \code{month}, then monthly rasters will be obtained (i.e., the "day" portion of the dates is ignored).
#'			\item If \code{y} is \code{year}, the annual rasters will be obtained (the "day" and "month" portion of the dates will be ignored).
#'		}
#'		\item	If \code{span} is \code{TRUE}, then two dates must be specified, and rasters will represent the period bracketed by these dates. For example, \code{dates = c('2014-04-22', '2015-04-22')} will return all daily, monthly, or annual rasters including and spanning those dates.
#'		\itemize{
#'			\item If \code{by} is \code{day}, then all days across the period \code{dates[1]} and \code{dates[2]}, inclusive, will be stacked.
#'			\item If \code{by} is \code{month}, then all months starting in the month of \code{dates[1]} and extending to the month of \code{dates[2]} will be stacked.
#'			\item If \code{by} is \code{year}, then all years starting in the year of \code{dates[1]} and extending to the year of \code{dates[2]} will be stacked.
#'		}
#' }
#' @param span If \code{FALSE} (default), then use the exact dates in \code{dates}. If \code{TRUE}, then extract a time series.
#' @param res Resolution of the rasters. Valid values are either \code{30} or \code{800} (i.e., 30-arcsecond or the "800-m" resolution version of PRISM), \emph{or} code{1} or \code{4} (i.e., the 1-arcminute or "4-km" resolution version of PRISM).
#' @param rastSuffix Character. The "suffix" at the end of each weather raster. PRISM rasters are usually shipped in 'BIL' format, so normally this should be \code{bil}. However, any other suffix corresponding to a raster type that can be opened by the \code{\link[terra]{rast}} function can be used if the rasters have been converted to another format.
#' @param annualDir Name of the highest-level folder in which annual rasters are stored. If you use the \code{prDownloadAnnual} function to download PRISM rasters, this will be \code{'annual'} (default). However, if you are extracting values from a purchased version of PRISM (i.e., that the PRISM staff sent you on a hard drive), the annual rasters are typically stored in the folder called \code{'monthly'}. This is not needed when stacking daily or monthly rasters.
#'
#' @return Object of class \code{rast}.
#' @examples
#' \dontrun{
#' prDir <- 'F:/ecology/Climate/PRISM/acquired_2020/an81'
#' 
#' # specific days
#' y <- prStack(
#'  	prDir = prDir,
#'  	vars = c('tmin', 'tmax'),
#' 		dates = c('2015-01-01', '2015-01-05'),
#' 		by = 'day',
#' 		res = 30,
#'  	rastSuffix = 'tif'
 #' )
#' 
#' # series of days
#' y <- prStack(
#'  	prDir = prDir,
#'  	vars = c('tmin', 'tmax'),
#' 		dates = c('2015-01-01', '2015-01-05'),
#' 		by = 'day',
#' 		span = TRUE,
#' 		res = 30,
#'  	rastSuffix = 'tif'
#' )
#' 
#' prDir <- 'F:/ecology/Climate/PRISM/acquired_2020/lt81'
#' # specific months
#' y <- prStack(
#'  	prDir = prDir,
#'  	vars = c('tmin', 'tmax'),
#' 		dates = c('2015-01-01', '2015-05-31'),
#' 		by = 'month',
#' 		res = 30,
#'  	rastSuffix = 'tif'
#' )
#' 
#' # series of months
#' y <- prStack(
#'  	prDir = prDir,
#'  	vars = c('tmin', 'tmax'),
#' 		dates = c('2015-01-01', '2015-05-31'),
#' 		by = 'month',
#' 		span = TRUE,
#' 		res = 30,
#'  	rastSuffix = 'tif'
#' )
#'  
#' # specific years
#' y <- prStack(
#'  	prDir = prDir,
#'  	vars = c('tmin', 'tmax'),
#' 		dates = c('2015-01-01', '2018-05-31'),
#' 		by = 'year',
#'  	rastSuffix = 'tif',
#' 		res = 30,
#' 		annualDir = 'monthly'
#' )
#' 
#' # series of months
#' y <- prStack(
#'  	prDir = prDir,
#'  	vars = c('tmin', 'tmax'),
#' 		dates = c('2015-01-01', '2018-05-31'),
#' 		by = 'year',
#' 		span = TRUE,
#'  	rastSuffix = 'tif',
#' 		res = 30,
#' 		annualDir = 'monthly'
#' )
#' 
#' }
#' @export

prStack <- function(
	prDir,
	vars,
	dates,
	by = 'day',
	span = FALSE,
	res = 4,
	rastSuffix = 'bil',
	annualDir = 'annual'
) {

	# get raster dates
	if (by == 'day') {
		subDir <- 'daily'
		if (!span) {
			rastGrid <- expand.grid(date=dates, var=vars)
		} else {
			dateSpan <- seq(as.Date(dates[1]), as.Date(dates[2]), by='1 days')
			rastGrid <- expand.grid(date=dateSpan, var=vars)
		}
		rastGrid$rastDate <- gsub(rastGrid$date, pattern='-', replacement='')
	} else if (by == 'month') {
		subDir <- 'monthly'
		if (!span) {
			rastGrid <- expand.grid(date=formatYYYYMM(dates), var=vars)
		} else {
			dateSpan <- seqMonths(dates[1], dates[2])
			rastGrid <- expand.grid(date=dateSpan, var=vars)
		}
		rastGrid$rastDate <- paste0(substr(rastGrid$date, 1, 4), substr(rastGrid$date, 6, 7))
	} else if (by == 'year') {
		subDir <- annualDir
		if (!span) {
			rastGrid <- expand.grid(date=substr(dates, 1, 4), var=vars)
		} else {
			yr1 <- as.integer(substr(dates[1], 1, 4))
			yr2 <- as.integer(substr(dates[2], 1, 4))
			dateSpan <- yr1:yr2
			rastGrid <- expand.grid(date=dateSpan, var=vars)
		}
		rastGrid$rastDate <- substr(rastGrid$date, 1, 4)
	} else {
		stop('Argument "by" must be "day", "month", or "year", and argument "span" must be "TRUE" or "FALSE".')
	}

	rastGrid$year <- substr(rastGrid$rastDate, 1, 4)
	rastGrid$rastName <- paste0('prism_', rastGrid$var, '_us_', prGetRes(res), '_', rastGrid$rastDate)
	
	rastsNeeded <- paste0(prDir, '/', rastGrid$var, '/', subDir, '/', rastGrid$year, '/', rastGrid$rastName, '.', rastSuffix)
	rasts <- terra::rast(rastsNeeded)
	rasts

}
