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
#' @param startDate,endDate These define the beginning/ending date across which values are to be extracted. Either:
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
#'  startDate=c('2014-04-22', '2014-04-25', '2014-04-22'),
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
#'	res = 30,
#' 	rastSuffix = 'tif',
#' 	verbose = TRUE
#' )
#' 
#' # monthly
#' x <- data.frame(
#' 	long=rep(-97.66, 3),
#' 	lat=rep(38.37, 3),
#'  startDate=c('2014-04-22', '2014-06-01', '2014-05-22'),
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
#'	res = 30,
#'  rastSuffix = 'tif',
#' 	verbose = TRUE
#' )
#' 
#' # annual
#' x <- data.frame(
#' 	long=rep(-97.66, 3),
#' 	lat=rep(38.37, 3),
#'  startDate=c('2016-01-01', '2016-06-01', '2017-01-01'),
#' 	endDate=c('2016-12-31', '2018-08-01', '2120-01-01')
#' )
#' 
#' prDir <- 'F:/ecology/Climate/PRISM/acquired_2020/lt81'
#' y <- prExtractAbsoluteAnnual(
#' 	x,
#' 	startDate = 'startDate',
#' 	endDate = 'endDate',
#'  longLat = c('long', 'lat'),
#'  prDir = prDir,
#'  vars = 'tmin',
#'	res = 30,
#'  rastSuffix = 'tif',
#' 	annualDir = 'monthly',
#'  verbose = TRUE
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
	res = 4,
	rastSuffix = 'bil',
	longLat = NULL,
	verbose = TRUE,
	...
) {

	### get unique dates ranges
	startDates <- prGetDates(x, startDate)
	endDates <- prGetDates(x, endDate)
	
	startEndDates <- data.frame(startDates=startDates, endDates=endDates)
	startEndDates <- startEndDates[!duplicated(startEndDates), ]

	# sort by end date then start date
	dateOrder <- order(startEndDates$endDates, startEndDates$startDates)
	startEndDates <- startEndDates[dateOrder, ]
	
	startEndDates$startDate <- lubridate::ymd(startEndDates$startDate)
	startEndDates$endDate <- lubridate::ymd(startEndDates$endDate)
	
	firstStartDate <- startEndDates$startDate[1]
	lastEndDate <- startEndDates$endDate[nrow(startEndDates)]

	### by VARIABLE
	for (thisVar in vars) {
	
		if (!dir.exists(paste0(prDir, '/', thisVar))) {
			warning(paste0('There is no directory for ', thisVar, '.'))
		} else {
		
			### get available dates
			yearsAvail <- list.dirs(paste0(prDir, '/', thisVar, '/daily'), full.names=FALSE, recursive=FALSE)
			yearsAvail <- as.integer(yearsAvail)

			earliestRasterDate <- lubridate::ymd(paste0(yearsAvail[1], '-01-01'))
			latestRasterDate <- lubridate::ymd(paste0(tail(yearsAvail, 1), '-12-31'))
			
			# get first date of extraction
			firstExtractDate <- if (earliestRasterDate %<d% firstStartDate) {
				firstStartDate
			} else {
				earliestRasterDate
			}
			
			# get last date of extraction
			lastExtractDate <- if (latestRasterDate %>d% lastEndDate) {
				lastEndDate
			} else {
				latestRasterDate
			}

			# create temporary data object to store extraction
			datesNeeded <- seq(firstExtractDate, lastExtractDate, by='1 day')
			numNeeded <- length(datesNeeded)
			
			thisOut <- matrix(NA, nrow=nrow(x), ncol=numNeeded)
			colnames(thisOut) <- paste0(thisVar, '_', datesNeeded)

			### by date range
			for (countDate in 1:nrow(startEndDates)) {
			
				thisStartDate <- startEndDates$startDate[countDate]
				thisEndDate <- startEndDates$endDate[countDate]
				
				if (verbose) cat(paste0(thisVar, ' ', thisStartDate, ' through ', thisEndDate, '\n')); flush.console()

				# if desired date range is at least partially within available date range
				if (!(thisStartDate %>d% latestRasterDate | thisEndDate %<d% thisStartDate)) {

					# get start/end dates of extraction (determined by available rasters)
					thisStartExtractDate <- thisStartDate
					thisEndExtractDate <- thisEndDate
					
					if (thisStartExtractDate %<d% earliestRasterDate) thisStartExtractDate <- earliestRasterDate
					if (thisEndExtractDate %>d% latestRasterDate) thisEndExtractDate <- latestRasterDate
				
					# rasters
					rasts <- prStack(prDir=prDir, vars=thisVar, dates=c(thisStartExtractDate, thisEndExtractDate), by='day', span=TRUE, res=res, rastSuffix=rastSuffix)
					
					# extract to records with this date
					index <- which(startDates == thisStartDate & endDates == thisEndDate)
					locs <- getCoords(x=x, index=index, longLat=longLat)
					ext <- terra::extract(rasts, locs, ...)
					# ext <- terra::extract(rasts, locs)
					ext <- as.matrix(ext)
					
					# remember
					datesExtracted <- seq(thisStartExtractDate, thisEndExtractDate, by='1 day')
					cols <- paste0(thisVar, '_', datesExtracted)
					thisOut[index, cols] <- ext
					
				} # if desired date range is at least partially within available date range
				
			} # next date range
			
			out <- if (exists('out', inherits=FALSE)) {
				cbind(out, thisOut)
			} else {
				thisOut
			}

		} # if variable exists
		
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
	res = 4,
	rastSuffix = 'bil',
	longLat = NULL,
	verbose = TRUE,
	...
) {

	### get unique dates ranges
	startDates <- prGetDates(x, startDate)
	endDates <- prGetDates(x, endDate)
	
	startDates <- formatYYYYMM(startDates)
	endDates <- formatYYYYMM(endDates)
	
	startEndDates <- data.frame(startDates=startDates, endDates=endDates)
	startEndDates <- startEndDates[!duplicated(startEndDates), ]

	# sort by end date then start date
	dateOrder <- order(startEndDates$endDates, startEndDates$startDates)
	startEndDates <- startEndDates[dateOrder, ]
	
	firstStartDate <- startEndDates$startDate[1]
	lastEndDate <- startEndDates$endDate[nrow(startEndDates)]
	
	### by VARIABLE
	for (thisVar in vars) {
	
		if (!dir.exists(paste0(prDir, '/', thisVar))) {
			warning(paste0('There is no directory for ', thisVar, '.'))
		} else {
		
			### get available dates
			yearsAvail <- list.dirs(paste0(prDir, '/', thisVar, '/monthly'), full.names=FALSE, recursive=FALSE)
			yearsAvail <- as.integer(yearsAvail)

			earliestRasterDate <- paste0(yearsAvail[1], '-01')
			latestRasterDate <- paste0(tail(yearsAvail, 1), '-12')
			
			# get first date of extraction
			firstExtractDate <- if (earliestRasterDate %<d% firstStartDate) {
				firstStartDate
			} else {
				earliestRasterDate
			}
			
			# get last date of extraction
			lastExtractDate <- if (latestRasterDate %>d% lastEndDate) {
				lastEndDate
			} else {
				latestRasterDate
			}
			
			# create temporary data object to store extraction
			datesNeeded <- seqMonths(firstExtractDate, lastExtractDate)
			numNeeded <- length(datesNeeded)
			
			thisOut <- matrix(NA, nrow=nrow(x), ncol=numNeeded)
			colnames(thisOut) <- paste0(thisVar, '_', datesNeeded)

			### by date range
			for (countDate in 1:nrow(startEndDates)) {
			
				thisStartDate <- startEndDates$startDate[countDate]
				thisEndDate <- startEndDates$endDate[countDate]
				
				if (verbose) cat(paste0(thisVar, ' ', thisStartDate, ' through ', thisEndDate, '\n')); flush.console()

				# if desired date range is at least partially within available date range
				if (!(thisStartDate %>d% latestRasterDate | thisEndDate %<d% thisStartDate)) {

					# get start/end dates of extraction (determined by available rasters)
					thisStartExtractDate <- thisStartDate
					thisEndExtractDate <- thisEndDate
					
					if (thisStartExtractDate %<d% earliestRasterDate) thisStartExtractDate <- earliestRasterDate
					if (thisEndExtractDate %>d% latestRasterDate) thisEndExtractDate <- latestRasterDate
				
					# rasters
					rasts <- prStack(prDir=prDir, vars=thisVar, dates=c(thisStartExtractDate, thisEndExtractDate), by='month', span=TRUE, res=res, rastSuffix=rastSuffix)
					
					# extract to records with this date
					index <- which(startDates == thisStartDate & endDates == thisEndDate)
					locs <- getCoords(x=x, index=index, longLat=longLat)
					ext <- terra::extract(rasts, locs, ...)
					# ext <- terra::extract(rasts, locs)
					ext <- as.matrix(ext)
					
					# remember
					datesExtracted <- seqMonths(thisStartExtractDate, thisEndExtractDate)
					cols <- paste0(thisVar, '_', datesExtracted)
					thisOut[index, cols] <- ext
					
				} # if desired date range is at least partially within available date range
				
			} # next date range
			
			out <- if (exists('out', inherits=FALSE)) {
				cbind(out, thisOut)
			} else {
				thisOut
			}
				
		} # if variable exists
		
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
	res = 4,
	rastSuffix = 'bil',
	longLat = NULL,
	annualDir = 'annual',
	verbose = TRUE,
	...
) {

	### get unique dates ranges
	startDates <- prGetDates(x, startDate)
	endDates <- prGetDates(x, endDate)
	
	startDates <- getYMD(startDates, 'y')
	endDates <- getYMD(endDates, 'y')
	
	startEndDates <- data.frame(startDates=startDates, endDates=endDates)
	startEndDates <- startEndDates[!duplicated(startEndDates), ]

	# sort by end date then start date
	dateOrder <- order(startEndDates$endDates, startEndDates$startDates)
	startEndDates <- startEndDates[dateOrder, ]
	
	firstStartDate <- startEndDates$startDate[1]
	lastEndDate <- startEndDates$endDate[nrow(startEndDates)]
	
	### by VARIABLE
	for (thisVar in vars) {
	
		if (!dir.exists(paste0(prDir, '/', thisVar))) {
			warning(paste0('There is no directory for ', thisVar, '.'))
		} else {
		
			### get available dates
			yearsAvail <- list.dirs(paste0(prDir, '/', thisVar, '/', annualDir), full.names=FALSE, recursive=FALSE)
			yearsAvail <- as.integer(yearsAvail)

			earliestRasterDate <- yearsAvail[1]
			latestRasterDate <- tail(yearsAvail, 1)
			
			# get first date of extraction
			firstExtractDate <- if (earliestRasterDate %<d% firstStartDate) {
				firstStartDate
			} else {
				earliestRasterDate
			}
			
			# get last date of extraction
			lastExtractDate <- if (latestRasterDate %>d% lastEndDate) {
				lastEndDate
			} else {
				latestRasterDate
			}
			
			# create temporary data object to store extraction
			datesNeeded <- firstExtractDate:lastExtractDate
			numNeeded <- length(datesNeeded)
			
			thisOut <- matrix(NA, nrow=nrow(x), ncol=numNeeded)
			colnames(thisOut) <- paste0(thisVar, '_', datesNeeded)

			### by date range
			for (countDate in 1:nrow(startEndDates)) {
			
				thisStartDate <- startEndDates$startDate[countDate]
				thisEndDate <- startEndDates$endDate[countDate]
				
				if (verbose) cat(paste0(thisVar, ' ', thisStartDate, ' through ', thisEndDate, '\n')); flush.console()

				# if desired date range is at least partially within available date range
				if (!(thisStartDate %>d% latestRasterDate | thisEndDate %<d% thisStartDate)) {

					# get start/end dates of extraction (determined by available rasters)
					thisStartExtractDate <- thisStartDate
					thisEndExtractDate <- thisEndDate
					
					if (thisStartExtractDate %<d% earliestRasterDate) thisStartExtractDate <- earliestRasterDate
					if (thisEndExtractDate %>d% latestRasterDate) thisEndExtractDate <- latestRasterDate
				
					# rasters
					rasts <- prStack(prDir=prDir, vars=thisVar, dates=c(thisStartExtractDate, thisEndExtractDate), by='year', span=TRUE, res=res, rastSuffix=rastSuffix, annualDir=annualDir)
					
					# extract to records with this date
					index <- which(startDates == thisStartDate & endDates == thisEndDate)
					locs <- getCoords(x=x, index=index, longLat=longLat)
					ext <- terra::extract(rasts, locs, ...)
					# ext <- terra::extract(rasts, locs)
					ext <- as.matrix(ext)
					
					# remember
					datesExtracted <- thisStartExtractDate:thisEndExtractDate
					cols <- paste0(thisVar, '_', datesExtracted)
					thisOut[index, cols] <- ext
					
				} # if desired date range is at least partially within available date range
				
			} # next date range
			
			out <- if (exists('out', inherits=FALSE)) {
				cbind(out, thisOut)
			} else {
				thisOut
			}
				
		} # if variable exists
		
	} # next variable
	
	out

}
