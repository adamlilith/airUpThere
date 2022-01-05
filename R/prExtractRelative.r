#' @name prismExtractRelative
#' @rdname prismExtractRelative
#' @title Extract values from PRISM across a time period relative to specific dates

#' @description These functions extract values from interpolated weather rasters from the Parameter Regression on Independent Slopes (PRISM) data product across a window of time relative to a given date. For example, it could extract all values starting 10 days prior to a given date, with the date varying by point. If you wish to extract values across a specified date range (e.g., from 2014-04-22 to 2014-04-29, then see \code{\link{prExtractAbsolute}}.  Extractions are done at points (versus polygons or lines, for example). \cr\cr

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
#' @param date Either:
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
#' @param res Resolution of the rasters. Valid values are either \code{30} or \code{800} (these are treated as the same: 30-arcsecond also known as the "800-m" resolution version of PRISM), \emph{or} code{1} or \code{4} (these are also treated as the same: the 1-arcminute also known as the "4-km" resolution version of PRISM).
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
#'  x <- data.frame(
#'  	long=rep(-97.66, 5),
#'  	lat=rep(38.37, 5),
#'  	date=c('2015-12-31', '1981-01-05', '2020-12-01',
#'  	'2019-01-05', '1895-05-01')
#' )
#'  
#'  
#' # NA values are unavailable
#' prDir <- 'F:/ecology/Climate/PRISM/acquired_2020/an81'
#' y <- prExtractRelativeDaily(
#'  	x,
#'  	date = 'date',
#'  	longLat = c('long', 'lat'),
#'  	prDir = prDir,
#'  	vars = 'tmin',
#' 		res = 30,
#'  	rastSuffix = 'tif',
#'  	windowYears = 0,
#'  	windowDays = 7,
#'  	verbose = TRUE
#' )
#'  
#' # NA values are unavailable
#' prDir <- 'F:/ecology/Climate/PRISM/acquired_2020/lt81'
#' y <- prExtractRelativeMonthly(
#'  	x,
#'  	date = 'date',
#'  	longLat = c('long', 'lat'),
#'  	prDir = prDir,
#'  	vars = 'tmin',
#' 		res = 30,
#'  	rastSuffix = 'tif',
#'  	windowYears = 0,
#'  	windowMonths = 5,
#'  	verbose = TRUE
#' )
#'  
#' # NA values are unavailable
#' prDir <- 'F:/ecology/Climate/PRISM/acquired_2020/lt81'
#' y <- prExtractRelativeAnnual(
#'  	x,
#'  	date = 'date',
#' 		longLat = c('long', 'lat'),
#' 		prDir = prDir,
#' 		vars = 'tmin',
#' 		res = 30,
#' 		rastSuffix = 'tif',
#' 		annualDir = 'monthly',
#' 		windowYears = 3,
#' 		verbose = TRUE
#' )
#' 
#' }
NULL

#' @describeIn prismExtractRelative Extract daily values from PRISM across a temporal window
#' @export
prExtractRelativeDaily <- function(
	prDir,
	x,
	vars,
	date,
	res = 4,
	rastSuffix = 'bil',
	longLat = NULL,
	windowYears = 0,
	windowDays = 0,
	verbose = TRUE,
	...
) {

	### get dates
	dates <- prGetDates(x, date)
	
	recordDates <- dates
	uniqueDates <- unique(sort(uniqueDates))

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

			### create data frame to store extracted values
			daysNeeded <- windowDays + ceiling(365.25 * windowYears)
			thisOut <- matrix(NA, nrow=nrow(x), ncol=daysNeeded + 1)
			colnames(thisOut) <- paste0(thisVar, '_', daysNeeded:0, 'daysPrior')

			### extract by date
			for (countDate in seq_along(uniqueDates)) {
			
				thisDate <- uniqueDates[countDate]
				if (verbose) print(paste0(thisVar, ' ', thisDate)); flush.console()
			
				# get date of window start/end
				windowStartDate <- thisDate
				lubridate::year(windowStartDate) <- lubridate::year(windowStartDate) - windowYears
				if (windowYears > 0) lubridate::day(windowStartDate) <- lubridate::day(windowStartDate) + 1
				lubridate::day(windowStartDate) <- lubridate::day(windowStartDate) - windowDays
				windowStartDate <- max(windowStartDate, earliestRasterDate)
			
				windowEndDate <- min(thisDate, latestRasterDate)
			
				# date falls within PRISM data set range
				if (thisDate >= earliestRasterDate & windowStartDate <= latestRasterDate) {
			
					# get rasters
					rasts <- prStack(prDir=prDir, vars=thisVar, dates=c(windowStartDate, windowEndDate), by='day', span=TRUE, res=res, rastSuffix=rastSuffix)
				
					# extract to records with this date
					index <- which(recordDates == thisDate)
					locs <- getCoords(x=x, index=index, longLat=longLat)
					ext <- terra::extract(rasts, locs, ...)
					ext$ID <- NULL
					# ext <- terra::extract(rasts, locs)
					ext <- as.matrix(ext)

					# ext <- ext[ , 2:ncol(ext), drop=FALSE]

					# remember
					lastCol <- ncol(thisOut) - as.integer(thisDate - windowEndDate)
					firstCol <- lastCol - ncol(ext) + 1
					thisOut[index, firstCol:lastCol] <- ext
				
				} # if record date falls within years available in PRISM
			
			} # next date

			out <- if (exists('out', inherits=FALSE)) {
				cbind(out, thisOut)
			} else {
				thisOut
			}
			
		} # directory for variable exists
		
	} # next variable
	
	while (all(is.na(out[ , 1]))) out <- out[ , 2:ncol(out), drop=FALSE]
	out

}

#' @describeIn prismExtractRelative Extract monthly values from PRISM across a temporal window
#' @export
prExtractRelativeMonthly <- function(
	prDir,
	x,
	vars,
	date,
	res = 4,
	rastSuffix = 'bil',
	longLat = NULL,
	windowYears = 0,
	windowMonths = 0,
	verbose = TRUE,
	...
) {

	### get dates
	dates <- prGetDates(x, date)
	
	recordDates <- formatYYYYMM(dates)
	uniqueDates <- unique(sort(recordDates))
	
	# if window months > 12 then allocate to years
	if (windowMonths > 12) {
		windowYears <- windowYears + floor(windowMonths / 12)
		windowMonths <- windowMonths %% 12
	}

	### by VARIABLE
	for (thisVar in vars) {
	
		if (!dir.exists(paste0(prDir, '/', thisVar))) {
			warning(paste0('There is no directory for ', thisVar, '.'))
		} else {
		
			### get available years
			yearsAvail <- list.dirs(paste0(prDir, '/', thisVar, '/monthly'), full.names=FALSE, recursive=FALSE)
			yearsAvail <- as.integer(yearsAvail)

			earliestRasterDate <- paste0(min(yearsAvail), '-01')
			latestRasterDate <- paste0(max(yearsAvail), '-12')

			### create data frame to store extracted values
			monthsNeeded <- 12 * windowYears + windowMonths
			thisOut <- matrix(NA, nrow=nrow(x), ncol=monthsNeeded + 1)
			colnames(thisOut) <- paste0(thisVar, '_', monthsNeeded:0, 'monthsPrior')

			### extract by date
			for (countDate in seq_along(uniqueDates)) {
			
				thisDate <- uniqueDates[countDate]
				if (verbose) cat(paste0(thisVar, ' ', thisDate, '\n')); flush.console()
			
				# get date of window start
				windowStartDate <- thisDate
				windowStartDate <- c(getYMD(windowStartDate, 'y'), getYMD(windowStartDate, 'm'))
				
				windowStartDate[1] <- windowStartDate[1] - windowYears
				if (windowStartDate[2] - windowMonths < 1) {
					windowStartDate[1] <- windowStartDate[1] - 1
					windowStartDate[2] <- 12 - (windowMonths - windowStartDate[2])
				} else {
					windowStartDate[2] <- windowStartDate[2] - windowMonths
				}
				
				windowStartDate <- paste0(windowStartDate[1], '-', ifelse(windowStartDate[2] < 10, '0', ''), windowStartDate[2])
				# desiredWindowStartDate <- windowStartDate
				if (windowStartDate < earliestRasterDate) windowStartDate <- earliestRasterDate
				
				# get date of window end
				windowEndDate <- thisDate
				# desiredWindowEndDate <- windowStartDate
				if (windowEndDate > latestRasterDate) windowEndDate <- latestRasterDate

				if (windowStartDate <= latestRasterDate & windowEndDate >= earliestRasterDate) {

					# get rasters
					rasts <- prStack(prDir=prDir, vars=thisVar, dates=c(windowStartDate, windowEndDate), by='month', span=TRUE, res=res, rastSuffix=rastSuffix)
				
					# extract to records with this date
					index <- which(recordDates == thisDate)
					locs <- getCoords(x=x, index=index, longLat=longLat)
					ext <- terra::extract(rasts, locs, ...)
					ext$ID <- NULL
					# ext <- terra::extract(rasts, locs)
					ext <- as.matrix(ext)

					# ext <- ext[ , 2:ncol(ext), drop=FALSE]

					# remember
					lastCol <- ncol(thisOut) - monthDiff(thisDate, windowEndDate)
					firstCol <- lastCol - ncol(ext) + 1
					thisOut[index, firstCol:lastCol] <- ext
					
				}
				
			} # next date

			out <- if (exists('out', inherits=FALSE)) {
				cbind(out, thisOut)
			} else {
				thisOut
			}
			
		} # directory for variable exists
		
	} # next variable
	
	while (all(is.na(out[ , 1]))) out <- out[ , 2:ncol(out), drop=FALSE]
	out

}

#' @describeIn prismExtractRelative Extract annual values from PRISM across a temporal window
#' @export
prExtractRelativeAnnual <- function(
	prDir,
	x,
	vars,
	date,
	res = 4,
	rastSuffix = 'bil',
	annualDir = 'annual',
	longLat = NULL,
	windowYears = 0,
	verbose = TRUE,
	...
) {

	### get dates
	dates <- prGetDates(x, date)

	# get record years
	recordYears <- lubridate::year(dates)
	uniqueYears <- sort(unique(recordYears))

	### by VARIABLE
	for (thisVar in vars) {
	
		if (!dir.exists(paste0(prDir, '/', thisVar))) {
			warning(paste0('There is no directory for ', thisVar, '.'))
		} else {
		
			### get available years
			yearsAvail <- list.dirs(paste0(prDir, '/', thisVar, '/', annualDir), full.names=FALSE, recursive=FALSE)
			yearsAvail <- as.integer(yearsAvail)
			
			earliestRasterYear <- min(yearsAvail)
			latestRasterYear <- max(yearsAvail)

			### create matrix to store extracted values
			thisOut <- matrix(NA, nrow=nrow(x), ncol=windowYears + 1)
			colnames(thisOut) <- paste0(thisVar, '_', windowYears:0, 'yearsPrior')

			### extract by date
			for (countYear in seq_along(uniqueYears)) {
			
				year <- uniqueYears[countYear]
				if (verbose) cat(' ', thisVar, as.integer(year), '\n'); flush.console()
			
				# get date of window start
				startYear <- year - windowYears
				
				# date falls within PRISM data set range
				if (year >= earliestRasterYear & startYear <= latestRasterYear) {
			
					# get years of extraction
					windowStartYear <- max(earliestRasterYear, startYear)
					windowEndYear <- min(latestRasterYear, year)
					
					# get rasters
					rasts <- prStack(prDir=prDir, vars=thisVar, dates=c(windowStartYear, windowEndYear), by='year', span=TRUE, res=res, rastSuffix=rastSuffix, annualDir=annualDir)
				
					# extract to records with this date
					index <- which(recordYears == year)
					locs <- getCoords(x=x, index=index, longLat=longLat)
					ext <- terra::extract(rasts, locs, ...)
					# ext <- terra::extract(rasts, locs)
					ext$ID <- NULL					
					ext <- as.matrix(ext)

					# ext <- ext[ , 2:ncol(ext), drop=FALSE]

					# remember
					deltaYears <- if (year < latestRasterYear) {
						0
					} else {
						year - latestRasterYear
					}
					
					lastCol <- ncol(thisOut) - deltaYears
					firstCol <- lastCol - ncol(ext) + 1
					thisOut[index, firstCol:lastCol] <- ext
			
				} # if record date falls within years available in PRISM
			
			} # next date

			out <- if (exists('out', inherits=FALSE)) {
				cbind(out, thisOut)
			} else {
				thisOut
			}
	
		} # directory for variable exists
	
	} # next variable

	while (all(is.na(out[ , 1]))) out <- out[ , 2:ncol(out), drop=FALSE]
	out

}
