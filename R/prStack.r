#' Create a raster stack from PRISM rasters
#'
#' This function creates a raster stack from select PRISM rasters. The stack consists of one or more variables across one or more time periods.
#'
#' @param prDir Character. Path of the base directory in which the rasters are stored.
#'
#' @param vars Name of variable(s) to stack. Valid values are:
#' * `ppt`: Precipitation.
#' * `tmax`: Maximum temperature.
#' * `tmin`: Minimum temperature.
#' * `tmean`: Mean temperature.
#' * `tdmean`: Dew-point temperature.
#' * `vpdmax`: Maximum vapor-pressure.
#' * `vpdmin`: Minimum vapor-pressure.
#'
#' @param dates This specifies either specific dates or a range of dates across which to stack rasters. Either way, dates are specified as objects of class `Date` or as characters naming dates in YYYY-MM-DD format.
#' * If `span` is `FALSE` (default), one or more dates can be specified, and the rasters matching the these dates will be stacked.
#'      * If `by` is `day`, then daily rasters will be obtained.
#'      * If `by` is `month`, then monthly rasters will be obtained (i.e., the "day" portion of the dates is ignored).
#'      * If `by` is `year`, the annual rasters will be obtained (the "day" and "month" portion of the dates will be ignored).
#'		}
#' * If `span` is `TRUE`, then two dates must be specified, and rasters will represent the period bracketed by these dates. For example, `dates = c('2014-04-22', '2015-04-22')` will return all daily, monthly, or annual rasters including and spanning those dates.
#'      * If `by` is `day`, then all days across the period `dates[1]` and `dates[2]`, inclusive, will be stacked.
#'      * If `by` is `month`, then all months starting in the month of `dates[1]` and extending to the month of `dates[2]` will be stacked.
#'      * If `by` is `year`, then all years starting in the year of `dates[1]` and extending to the year of `dates[2]` will be stacked.
#'
#' @param span If `FALSE` (default), then use the exact dates in `dates`. If `TRUE`, then extract a time series.
#'
#' @param res Resolution of the rasters. Valid values are either `30` or `800` (i.e., 30-arcsecond or the "800-m" resolution version of PRISM), *or* `1` or `4` (i.e., the 1-arcminute or "4-km" resolution version of PRISM).
#'
#' @param rastSuffix Character. The "suffix" at the end of each weather raster. PRISM rasters are usually shipped in 'BIL' format, so normally this should be `bil`. However, any other suffix corresponding to a raster type that can be opened by the [terra::rast()] function can be used if the rasters have been converted to another format.
#'
#' @param annualDir Name of the highest-level folder in which annual rasters are stored. If you use the `prDownloadAnnual()` function to download PRISM rasters, this will be `'annual'` (default). However, if you are extracting values from a purchased version of PRISM (i.e., that the PRISM staff sent you on a hard drive), the annual rasters are typically stored in the folder called `'monthly'`. This is not needed when stacking daily or monthly rasters.
#'
#' @return Object of class `rast`.
#'
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
