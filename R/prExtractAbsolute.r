#' @name prExtractAbsolute
#' @rdname prExtractAbsolute
#' @title Extract values from PRISM across specified time periods

#' @description These functions extract values from interpolated weather rasters from the Parameter Regression on Independent Slopes (PRISM) data product across a range of dates. For example, it could extract all values from 2014-04-22 to 2014-04-29. If you wish to extract values across a set date range (e.g., up to 10 days prior to a set of dates, then see \code{\link{prExtractWindow}}. If you wish to extract values across a specified date range (e.g., from 2014-04-22 to 2014-04-29, then see \code{\link{prExtractRelative}}. Extractions are done at points (versus polygons or lines, for example). \cr\cr

#' The basic input is an object of class \code{spatVector}, \code{SpatialPoints}, \code{SpatialPointsDataFrame}, \code{data.frame}, or \code{matrix}, with each row representing a point location. The function also needs to be pointed toward a folder with PRISM data. The folder with PRISM data must be structured as:
#' \itemize{
#'		\item Base folder (e.g., \code{'C:/PRISM/an81'} or \code{'C:/PRISM/lt81'}), which contains:
#'		\item A folder named either \code{daily}, \code{monthly}, \code{annual}, which contains:
#'		\item One folder per variable (e.g., \code{tmin}, \code{tmax}, \code{vpdmax}), each of which contain:
#'		\item One folder per year (e.g., \code{1981}, code{1982}, etc.), each of which rasters like:
#'		\itemize{
#'			\item One raster per day named like \code{prism_tmin_us_30s_19810101.bil}, \code{prism_tdmean_us_30s_19810102.bil}, etc.
#'			\item One raster per month named like \code{prism_tmin_us_30s_198101.bil}, \code{prism_tdmean_us_30s_198101.bil}, etc.
#'			\item One raster representing the annual value named like \code{prism_tmin_us_30s_1981.bil}.
#' 		}
#' }
#' \cr

#' The function can extract values corresponding to the day/month/year of each record, plus (optionally) a user-specified window of time prior to the day/month/year of each record. For example, you could use this to extract daily climate data for a site collected on April 22, 2014, and all days prior up to 10 years (April 23, 2004). This function is really a fancy wrapper for \code{\link[terra]{extract}}, but it does the tough job of getting the directory structures right, pulling all needed rasters, and efficiently grouping records to speed extraction. \cr\cr

#' The function does not assume that data for all PRISM years are available, but it does assume that all relevant rasters for a particular year are available within each yearly folder. If rasters preceding a date only partially cover the window, then values for the part covered will be extracted. For example if you try to extract annual values for a window spanning 2010 to 2020 but only have available rasters for 1981 to 2018, then values for 2010 to 2018 will be extracted. Values that cannot be extracted are represented by \code{NA} in the output.

#' @param x "Points" object of class \code{SpatVector}, \code{SpatialPoints}, \code{SpatialPointsDataFrame}, \code{data.frame}, or \code{matrix}.
#' @param startDate,endDate These def the beginning/ending date across which values are to be extracted. Either:
#' \itemize{
#'		\item 	Object of class \code{Date}. You can name a single date, in which case this date will be used for all records, or a vector of dates, one per point in \code{x}.
#'		\item 	Name of column in \code{x} with date of each record. Values must be of in YYYY-MM-DD (year-month-day of month) format \emph{or} already of class \code{\link{Date}}. See \code{\link[lubridate]{ymd}} or related functions for more help.
#'		\item   Names of columns in \code{x} with year, month, and day of month of each record (in that order). Months must be numeric (i.e., 10, not "October").
#' }
#' @param prDir Character. Path of the base directory in which the rasters are stored. It must have a structure with subfolders as described above.
#' @param vars Name of variable(s) to extract. Valid values are:
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
#' @param annualDir Name of the highest-level folder in which annual rasters are stored. If you use the \code{prDownloadAnnual} function to download PRISM rasters, this will be \code{'annual'} (default). However, if you are extracting values from a purchased version of PRISM (i.e., that the PRISM staff sent you on a hard drive), the annual rasters are typically stored in the folder called \code{'monthly'}.
#' @param longLat Character vector with two elements. If \code{x} is an object of class \code{data.frame}, this is the name of the columns in \code{x} with longitude and latitude (in that order). Coordinates will be assumed to be in the same coordinate reference system as the PRISM rasters. This argument is ignored if \code{x} is not a data frame.
#' @param windowYears,windowMonths,windowDays Integers >= 0. Extract data for this many years, months, and/or days before the day of each observation, \emph{plus} the day of observation. Note:
#' \itemize{
#'		\item For daily data, only \code{windowYears} and \code{windowDays} are used. Note that the difference between using, say, \code{windowYears = 1} and \code{windowDays = 365} is that the former can accommodate leap days whereas the latter just extracts the 365 days prior (which may be less than a full calendar year if the timespan encompasses a leap day).
#'		\item For monthly data, only \code{windowYears} and \code{windowMonths} are used.
#'		\item For annual data, only \code{windowYears} is used.
#' }
#' To get only data for the day/month/year of each record, set all of the respective \code{window} arguments to 0 (default).
#' @param verbose Logical. If \code{TRUE} (default), show progress.
#' @param ... Argument(s) to pass to \code{\link[terra]{extract}}.
#'
#' @return Matrix with one row per row in \code{x}. \code{NA} values represent days/months/years that did not fall within the specified window or for which rasters were unavailable. 
#' @examples
#' \dontrun{
# daily
#' x <- data.frame(
#' 	long=rep(-97.66, 3),
#' 	lat=rep(38.37, 3),
#'  	startDate=c('2014-04-22', '2014-04-25', '2014-04-22'),
#' 	endDate=c('2014-04-22', '2014-04-29', '2014-05-22')
#' )
#' 
#' prDir <- 'F:/ecology/Climate/PRISM/acquired_2020/an81'
#' y <- prExtractAbsoluteDaily(
#' 	x,
#' 	startDate = 'startDate',
#' 	endDate = 'endDate',
#' 	longLat = c('long', 'lat'),
#' 	prDir = prDir,
#' 	vars = 'tmin',
#' 	rastSuffix = 'tif',
#' 	verbose = TRUE
#' )
#' 
#' # monthly
#' x <- data.frame(
#' 	long=rep(-97.66, 3),
#' 	lat=rep(38.37, 3),
#'  	startDate=c('2014-04-22', '2014-06-01', '2014-05-22'),
#' 	endDate=c('2014-04-22', '2014-08-01', '2015-05-22')
#' )
#' 
#' prDir <- 'F:/ecology/Climate/PRISM/acquired_2020/lt81'
#' y <- prExtractAbsoluteMonthly(
#' 	x,
#' 	startDate = 'startDate',
#' 	endDate = 'endDate',
#' 	longLat = c('long', 'lat'),
#' 	prDir = prDir,
#'  vars = 'tmin',
#'  rastSuffix = 'tif',
#' 	verbose = TRUE
#' )
#' 
#' # annual
#' x <- data.frame(
#' 	long=rep(-97.66, 3),
#' 	lat=rep(38.37, 3),
#'  	startDate=c('2016-01-01', '2016-06-01', '2018-01-01'),
#' 	endDate=c('2016-12-31', '2018-08-01', '2120-01-01')
#' )
#' 
#' prDir <- 'F:/ecology/Climate/PRISM/acquired_2020/lt81'
#' y <- prExtractAbsoluteAnnual(
#' 	x,
#' 	startDate = 'startDate',
#' 	endDate = 'endDate',
#'   	longLat = ll,
#'   	prDir = prDir,
#'  	vars = 'tmin',
#'  	rastSuffix = 'tif',
#' 		annualDir = 'monthly',
#'  	verbose = TRUE
#' )
#' 
#' }
NULL

#' @describeIn prExtractAbsolute Extract daily values from PRISM across a date range
#' @export
prExtractAbsoluteDaily <- function(
	x,
	startDate,
	endDate,
	prDir,
	vars,
	rastSuffix = 'bil',
	longLat = NULL,
	verbose = TRUE,
	...
) {

	### get dates
	startDates <- prGetDates(x, startDate)
	endDates <- prGetDates(x, endDate)
	
	startEndDates <- data.frame(startDates=startDates, endDates=endDates)
	startEndDates <- unique(startEndDates)

	# sort by end date then start date
	dateOrder <- order(endDates, startDates)
	startEndDates <- startEndDates[dateOrder, ]

	### by VARIABLE
	for (thisVar in vars) {
	
		if (!dir.exists(paste0(prDir, '/', thisVar))) {
			warning(paste0('There is no directory for ', thisVar, '.'))
		} else {
		
			### get available years
			yearsAvail <- list.dirs(paste0(prDir, '/', thisVar, '/daily'), full.names=FALSE, recursive=FALSE)
			yearsAvail <- as.integer(yearsAvail)

			earliestRasterDate <- lubridate::make_date(min(yearsAvail), '01', '01')
			latestRasterDate <- lubridate::make_date(max(yearsAvail), '12', '31')

			### range and number of dates needed
			earliestStartDate <- min(startDates)
			latestEndDate <- max(endDates)
	
			firstDate <- max(earliestRasterDate, earliestStartDate)
			lastDate <- min(latestRasterDate, latestEndDate)
			
			numDates <- lastDate - firstDate + 1
			numDates <- as.numeric(numDates)
			
			datesNeeded <- seq(firstDate, lastDate, by='1 days')
			thisOut <- matrix(NA, nrow=nrow(x), ncol=numDates)
			colnames(thisOut) <- paste0(thisVar, '_', datesNeeded)

			### extract by date
			for (countDate in 1:nrow(startEndDates)) {
			
				thisStartDate <- startEndDates$startDate[countDate]
				thisEndDate <- startEndDates$endDate[countDate]
				if (verbose) cat(thisVar, thisStartDate, 'through', thisEndDate, '\n')); flush.console()
				
				if (thisStartDate < earliestRasterDate) thisStartDate <- earliestRasterDate
				if (thisEndDate > latestRasterDate) thisEndDate <- latestRasterDate
			
				# dates needed
				datesNeeded <- seq(thisStartDate, thisEndDate, by='1 days')
		
				yearsNeeded <- lubridate::year(datesNeeded)
				monthsNeeded <- lubridate::month(datesNeeded)
				daysNeeded <- lubridate::day(datesNeeded)
		
				rastDates <- paste0(yearsNeeded, sprintf('%02.0f', monthsNeeded), sprintf('%02.0f', daysNeeded)) 
		
				rastsNeeded <- paste0(prDir, '/', thisVar, '/daily/', yearsNeeded, '/prism_', thisVar, '_us_30s_', rastDates, '.', rastSuffix)

				# get rasters
				rasts <- terra::rast(rastsNeeded)
			
				# extract to records with this date
				index <- which(startDates == thisStartDate & endDates == thisEndDate)
				locs <- getCoords(x=x, index=index, longLat=longLat)
				ext <- terra::extract(rasts, locs, ...)
				# ext <- terra::extract(rasts, locs)
				ext <- as.matrix(ext)

				# ext <- ext[ , 2:ncol(ext), drop=FALSE]

				# remember
				cols <- paste0(thisVar, '_', datesNeeded)
				thisOut[index, cols] <- ext
						
			} # next date

			out <- if (exists('out', inherits=FALSE)) {
				cbind(out, thisOut)
			} else {
				thisOut
			}
			
		} # directory for variable exists
		
	} # next variable
	
	out

}

#' @describeIn prExtractAbsolute Extract monthly values from PRISM across a date range
#' @export
prExtractAbsoluteMonthly <- function(
	x,
	startDate,
	endDate,
	prDir,
	vars,
	rastSuffix = 'bil',
	longLat = NULL,
	verbose = TRUE,
	...
) {

	### get dates
	startDates <- prGetDates(x, startDate)
	endDates <- prGetDates(x, endDate)
	
	startEndDates <- data.frame(startDates=startDates, endDates=endDates)

	dateOrder <- order(endDates, startDates)
	startEndDates <- startEndDates[dateOrder, ]

	startYear <- as.numeric(substr(startEndDates$startDate, 1, 4))
	startMonth <- as.numeric(substr(startEndDates$startDate, 6, 7))
	endYear <- as.numeric(substr(startEndDates$endDate, 1, 4))
	endMonth <- as.numeric(substr(startEndDates$endDate, 6, 7))
	
	startEndDates$startYearMonth <- paste0(startYear, '-', ifelse(startMonth < 10, '0', ''), startMonth)
	startEndDates$endYearMonth <- paste0(endYear, '-', ifelse(endMonth < 10, '0', ''), endMonth)
	
	dups <- duplicated(startEndDates[ , c('startYearMonth', 'endYearMonth')])
	startEndDates <- startEndDates[!dups, ]

	### by VARIABLE
	for (thisVar in vars) {
	
		if (!dir.exists(paste0(prDir, '/', thisVar))) {
			warning(paste0('There is no directory for ', thisVar, '.'))
		} else {
		
			### get available years
			yearsAvail <- list.dirs(paste0(prDir, '/', thisVar, '/monthly'), full.names=FALSE, recursive=FALSE)
			yearsAvail <- as.integer(yearsAvail)

			earliestRasterDate <- lubridate::make_date(min(yearsAvail), '01', '01')
			latestRasterDate <- lubridate::make_date(max(yearsAvail), '12', '31')

			### range and number of dates needed
			earliestStartDate <- min(startDates)
			latestEndDate <- max(endDates)
	
			firstDate <- max(earliestRasterDate, earliestStartDate)
			lastDate <- min(latestRasterDate, latestEndDate)
			
			datesNeeded <- seqMonths(firstDate, lastDate)
			
			numDates <- length(datesNeeded)

			thisOut <- matrix(NA, nrow=nrow(x), ncol=numDates)
			colnames(thisOut) <- paste0(thisVar, '_', datesNeeded)

			### extract by date
			for (countDate in 1:nrow(startEndDates)) {
			
				thisStartDate <- startEndDates$startDate[countDate]
				thisEndDate <- startEndDates$endDate[countDate]

				thisStartYearMonth <- startEndDates$startYearMonth[countDate]
				thisEndYearMonth <- startEndDates$endYearMonth[countDate]

				if (verbose) cat(thisVar, thisStartYearMonth, 'through', thisEndYearMonth, '\n'); flush.console()
				
				if (thisStartDate < earliestRasterDate) thisStartDate <- earliestRasterDate
				if (thisEndDate > latestRasterDate) thisEndDate <- latestRasterDate
			
				# dates needed
				yearsMonthsNeeded <- seqMonths(thisStartDate, thisEndDate)
				yearsNeeded <- substr(yearsMonthsNeeded, 1, 4)
				rastYearsMonthsNeeded <- paste0(substr(yearsMonthsNeeded, 1, 4), substr(yearsMonthsNeeded, 6, 7))
		
				rastsNeeded <- paste0(prDir, '/', thisVar, '/monthly/', yearsNeeded, '/prism_', thisVar, '_us_30s_', rastYearsMonthsNeeded, '.', rastSuffix)

				# get rasters
				rasts <- terra::rast(rastsNeeded)
			
				# extract to records with this date
				index <- which(yearMonth(startDates) == yearMonth(thisStartDate) & yearMonth(endDates) == yearMonth(thisEndDate))
				locs <- getCoords(x=x, index=index, longLat=longLat)
				ext <- terra::extract(rasts, locs, ...)
				# ext <- terra::extract(rasts, locs)
				ext <- as.matrix(ext)

				# ext <- ext[ , 2:ncol(ext), drop=FALSE]

				# remember
				cols <- paste0(thisVar, '_', yearsMonthsNeeded)
				thisOut[index, cols] <- ext
						
			} # next date

			out <- if (exists('out', inherits=FALSE)) {
				cbind(out, thisOut)
			} else {
				thisOut
			}
			
		} # directory for variable exists
		
	} # next variable
	
	out

}

#' @describeIn prExtractAbsolute Extract annual values from PRISM across a date range
#' @export
prExtractAbsoluteAnnual <- function(
	x,
	startDate,
	endDate,
	prDir,
	vars,
	rastSuffix = 'bil',
	longLat = NULL,
	annualDir = 'annual',
	verbose = TRUE,
	...
) {

	### get dates
	startDates <- prGetDates(x, startDate)
	endDates <- prGetDates(x, endDate)
	
	startYears <- substr(startDates, 1, 4)
	endYears <- substr(endDates, 1, 4)
	
	startYears <- as.integer(startYears)
	endYears <- as.integer(endYears)
	
	startEndDates <- data.frame(startDates=startYears, endDates=endYears)

	dateOrder <- order(endDates, startDates)
	startEndDates <- startEndDates[dateOrder, ]

	dups <- duplicated(startEndDates)
	startEndDates <- startEndDates[!dups, ]

	### by VARIABLE
	for (thisVar in vars) {
	
		if (!dir.exists(paste0(prDir, '/', thisVar))) {
			warning(paste0('There is no directory for ', thisVar, '.'))
		} else {
		
			### get available years
			yearsAvail <- list.dirs(paste0(prDir, '/', thisVar, '/', annualDir), full.names=FALSE, recursive=FALSE)
			yearsAvail <- as.integer(yearsAvail)

			earliestRasterDate <- min(yearsAvail)
			latestRasterDate <- max(yearsAvail)
			
			### range and number of dates needed
			earliestStartDate <- min(startYears)
			latestEndDate <- max(endYears)
	
			firstDate <- max(earliestRasterDate, earliestStartDate)
			lastDate <- min(latestRasterDate, latestEndDate)
			
			thisStartYears <- pmax(startYears, firstDate)
			thisEndYears <- pmin(endYears, lastDate)

			datesNeeded <- firstDate:lastDate
			
			numDates <- length(datesNeeded)

			thisOut <- matrix(NA, nrow=nrow(x), ncol=numDates)
			colnames(thisOut) <- paste0(thisVar, '_', datesNeeded)

			### extract by date
			for (countDate in 1:nrow(startEndDates)) {
			
				thisStartDate <- startEndDates$startDate[countDate]
				thisEndDate <- startEndDates$endDate[countDate]

				if (verbose) cat(thisVar, thisStartDate, 'through', thisEndDate, '\n'); flush.console()
				
				if (thisStartDate < earliestRasterDate) thisStartDate <- earliestRasterDate
				if (thisEndDate > latestRasterDate) thisEndDate <- latestRasterDate
			
				# dates needed
				yearsNeeded <- thisStartDate:thisEndDate
		
				rastsNeeded <- paste0(prDir, '/', thisVar, '/', annualDir, '/', yearsNeeded, '/prism_', thisVar, '_us_30s_', yearsNeeded, '.', rastSuffix)

				# get rasters
				rasts <- terra::rast(rastsNeeded)
			
				# extract to records with this date
				index <- which(thisStartYears == thisStartDate & thisEndYears == thisEndDate)
				locs <- getCoords(x=x, index=index, longLat=longLat)
				ext <- terra::extract(rasts, locs, ...)
				# ext <- terra::extract(rasts, locs)
				ext <- as.matrix(ext)

				# ext <- ext[ , 2:ncol(ext), drop=FALSE]

				# remember
				cols <- paste0(thisVar, '_', yearsNeeded)
				thisOut[index, cols] <- ext
						
			} # next date

			out <- if (exists('out', inherits=FALSE)) {
				cbind(out, thisOut)
			} else {
				thisOut
			}
			
		} # directory for variable exists
		
	} # next variable
	
	out

}
