#' @name prismExtractRelative
#' @rdname prismExtractRelative
#' @title Extract values from PRISM across a time period relative to specific dates
#'
#' @description These functions extract values at point, line, or polygon locations from interpolated weather rasters from the Parameter Regression on Independent Slopes (PRISM) data product across a window of time relative to a given date. For example, it could extract all values starting 10 days prior to a given date, with the date varying by point, line, or polygon. If you wish to extract values across a specified date range that can vary by point, line, or plane, then see [prExtractAbsolute()].
#'
#' The basic input is a `SpatVector` representing points, lines, or polygons. The function also needs to be pointed toward a folder with PRISM data. The folder with PRISM data must be structured as:
#' * Base folder (e.g., `'C:/PRISM/an81'` or `'C:/PRISM/lt81'`), which contains:
#' * A folder named either `daily`, `monthly`, `annual`, which contains:
#' * One folder per variable (e.g., `tmin`, `tmax`, `vpdmax`), each of which contain:
#' * One folder per year (e.g., `1981`, `1982`, etc.), each of which rasters like:
#'       * One raster per day named like `prism_tmin_us_30s_19810101.bil`, `prism_tdmean_us_30s_19810102.bil`, etc.
#'       * One raster per month named like `prism_tmin_us_30s_198101.bil`, `prism_tdmean_us_30s_198101.bil`, etc.
#'       * One raster representing the annual value named like `prism_tmin_us_30s_1981.bil`.
#'
#' The function can extract values corresponding to the day/month/year of each record, plus a user-specified window of time prior to the day/month/year of each record. For example, you could use this to extract daily climate data for a site collected on April 22, 2014, and all days prior up to 10 years (April 23, 2004). This function is really a fancy wrapper for [terra::extract()], but it does the tough job of getting the folder structures right, pulling all needed rasters, and efficiently grouping records to speed extraction.
#'
#' The function does not assume that data for all PRISM years are available, but it does assume that all relevant rasters for a particular year are available within each yearly folder. If rasters preceding a date only partially cover the window, then values for the part covered will be extracted. For example if you try to extract annual values for a window spanning 2010 to 2020 but only have available rasters for 1981 to 2018, then values for 2010 to 2018 will be extracted. Values that cannot be extracted are represented by `NA` in the output.
#'
#' @param x A `SpatVector`.
#' @param date Either:
#' * Object of class `Date`. You can name a single date, in which case this date will be used for all records, or a vector of dates, one per point in `x`.
#' * Name of column in `x` with date of each record. Values must be of in YYYY-MM-DD (year-month-day of month) format *or* already of class `Date`. See [lubridate::ymd()] or related functions for more help.
#' * Names of columns in `x` with year, month, and day of month of each record (in that order). Months must be numeric (i.e., 10, not "October").
#'
#' @param prDir Character. Path of the base folder in which the rasters are stored. It must have a structure with subfolders as described above.
#'
#' @param vars Name of variable(s) to extract. Valid values are:
#' * `ppt`: Precipitation.
#' * `tmax`: Maximum temperature.
#' * `tmin`: Minimum temperature.
#' * `tmean`: Mean temperature.
#' * `tdmean`: Dew-point temperature.
#' * `vpdmax`: Maximum vapor-pressure.
#' * `vpdmin`: Minimum vapor-pressure.
#'
#' @param res Resolution of the rasters. Valid values are either `30` or `800` (these are treated as the same: 30-arcsecond also known as the "800-m" resolution version of PRISM), *or* `1` or `4` (these are also treated as the same: the 1-arcminute also known as the "4-km" resolution version of PRISM).
#'
#' @param rastSuffix Character. The "suffix" at the end of each weather raster. PRISM rasters are usually shipped in 'BIL' format, so normally this should be `bil`. However, any other suffix corresponding to a raster type that can be opened by the [terra::rast()] function can be used if the rasters have been converted to another format.
#'
#' @param annualDir Name of the highest-level folder in which annual rasters are stored. If you use the [prDownloadAnnual()] function to download PRISM rasters, this will be `'annual'` (default). However, if you are extracting values from a purchased version of PRISM (i.e., that the PRISM staff sent you on a hard drive), the annual rasters are typically stored in the folder called `'monthly'`.
#'
#' @param windowYears,windowMonths,windowDays Integers >= 0. Extract data for this many years, months, and/or days before the day of each observation, *plus* the day of observation. Note:
#' * For daily data, only `windowYears` and `windowDays` are used. Note that the difference between using, say, `windowYears = 1` and `windowDays = 365` is that the former can accommodate leap days whereas the latter just extracts the 365 days prior (which may be less than a full calendar year if the time span encompasses a leap day).
#' * For monthly data, only `windowYears` and `windowMonths` are used.
#' * For annual data, only `windowYears` is used.
#'
#' To get only data for the day/month/year of each record, set all of the respective `window` arguments to 0 (default).
#'
#' @param template Either `NULL` (default), or `SpatRaster` or path and filename of a PRISM raster. This argument is used to set the dimensions of the output data frame. If `x` is a "points" `SpatVector`, then `template` can be `NULL`, but cell number and coordinate cannot be returned (i.e., setting `cells = TRUE` and `xy = TRUE` in the `...` part of the function call). If `x` is a "lines" or "polygons" vector, then this *must* be supplied so that the function can ascertain how many rows will be in the output table.
#'
#' @param verbose Logical. If `TRUE` (default), show progress.
#'
#' @param ... Argument(s) to pass to [terra::extract()].
#'
#' @returns Matrix. `NA` values represent days/months/years that did not fall within the specified window or for which rasters were unavailable.
#'
#' @example ./man/examples/ex_prExtractRelative.r
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
	windowYears = 0,
	windowDays = 0,
	template = NULL,
	verbose = TRUE,
	...
) {

	### get dates
	dates <- prGetDates(x, date) # DO NOT SORT THIS!!!
	
	recordDates <- dates
	uniqueDates <- unique(sort(dates))

	### type and errors
	isPoints <- terra::geomtype(x) == 'points'
	if (!isPoints & is.null(template)) stop('To extract values to a line or polygon vector, you need to supply a template raster.')
	
	### arguments for terra::extract()	
	dots <- list(...)
	idTRUE <- if (any(names(dots) == 'ID')) { dots$ID } else { FALSE }
	weightsTRUE <- if (any(names(dots) == 'weights') & !isPoints) { dots$weights } else { FALSE }
	cellsTRUE <- if (any(names(dots) == 'cells')) { dots$cells } else { FALSE }
	xyTRUE <- if (any(names(dots) == 'xy')) { dots$xy } else { FALSE }
	dots$ID <- FALSE
	dots$weights <- TRUE # needs to be TRUE to get fractional cells!
	dots$cells <- FALSE
	dots$xy <- FALSE
	
	### metadata (ID, cell number, weights, coordinates) on extraction from PRISM
	meta <- .prMeta(x = x, template = template, dots = dots, dates = dates)
	nrowOut <- nrow(meta)
	
	### by VARIABLE
	for (thisVar in vars) {
	
		if (!dir.exists(paste0(prDir, '/', thisVar))) {
			warning(paste0('There is no folder for ', thisVar, '.'))
		} else {
		
			### get available years
			yearsAvail <- list.dirs(paste0(prDir, '/', thisVar, '/daily'), full.names=FALSE, recursive=FALSE)
			yearsAvail <- as.integer(yearsAvail)

			earliestRasterDate <- lubridate::make_date(min(yearsAvail), '01', '01')
			latestRasterDate <- lubridate::make_date(max(yearsAvail), '12', '31')

			### create data frame to store extracted values
			daysNeeded <- windowDays + ceiling(365.25 * windowYears)
			
			thisOut <- matrix(NA, nrow = nrowOut, ncol = daysNeeded + 1)
			colnames(thisOut) <- paste0(thisVar, '_', daysNeeded:0, 'daysPrior')

			### extract by date
			for (countDate in seq_along(uniqueDates)) {
			
				thisDate <- uniqueDates[countDate]
				if (verbose) omnibus::say(thisVar, ' ', thisDate)
			
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
					locs <- x[index]
					args <- list(x = rasts, y = locs)
					args <- c(args, dots)
					ext <- do.call(terra::extract, args = args)
					if (!isPoints) ext$weight <- NULL
					ext <- as.matrix(ext)

					# remember
					lastCol <- ncol(thisOut) - as.integer(thisDate - windowEndDate)
					firstCol <- lastCol - ncol(ext) + 1
					
					if (isPoints) {
						rowIndex <- index
					} else {
						rowIndex <- which(meta$date == thisDate)
					}
					
					thisOut[rowIndex, firstCol:lastCol] <- ext
				
				} # if record date falls within years available in PRISM
			
			} # next date

			out <- if (exists('out', inherits=FALSE)) {
				cbind(out, thisOut)
			} else {
				thisOut
			}
			
		} # folder for variable exists
		
	} # next variable
	
	# remove unwanted columns
	while (all(is.na(out[ , 1]))) out <- out[ , 2:ncol(out), drop=FALSE]

	meta$date <- NULL
	
	if (any(c(!idTRUE, !weightsTRUE, !cellsTRUE, !xyTRUE))) {
	
		bads <- c('ID', 'weight', 'cell', 'x', 'y')[c(!idTRUE, !weightsTRUE, !cellsTRUE, !xyTRUE, !xyTRUE)]
		keeps <- which(omnibus::notIn(colnames(meta), bads))
		meta <- meta[ , keeps, drop = FALSE]
		
	
	}
	if (nrow(meta) > 0) out <- cbind(meta, out)
	
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
	windowYears = 0,
	windowMonths = 0,
	template = NULL,
	verbose = TRUE,
	...
) {

	### get dates
	dates <- prGetDates(x, date)
	
	recordDates <- formatYYYYMM(dates)
	uniqueDates <- unique(sort(recordDates))
	
	### type and errors
	isPoints <- terra::geomtype(x) == 'points'
	if (!isPoints & is.null(template)) stop('To extract values to a line or polygon vector, you need to supply a template raster.')

	### arguments for terra::extract()	
	dots <- list(...)
	idTRUE <- if (any(names(dots) == 'ID')) { dots$ID } else { FALSE }
	weightsTRUE <- if (any(names(dots) == 'weights') & !isPoints) { dots$weights } else { FALSE }
	cellsTRUE <- if (any(names(dots) == 'cells')) { dots$cells } else { FALSE }
	xyTRUE <- if (any(names(dots) == 'xy')) { dots$xy } else { FALSE }
	dots$ID <- FALSE
	dots$weights <- TRUE # needs to be TRUE to get fractional cells!
	dots$cells <- FALSE
	dots$xy <- FALSE
	
	### metadata (ID, cell number, weights, coordinates) on extraction from PRISM
	meta <- .prMeta(x = x, template = template, dots = dots, dates = dates)
	nrowOut <- nrow(meta)

	# if window months > 12 then allocate to years
	if (windowMonths > 12) {
		windowYears <- windowYears + floor(windowMonths / 12)
		windowMonths <- windowMonths %% 12
	}

	### by VARIABLE
	for (thisVar in vars) {
	
		if (!dir.exists(paste0(prDir, '/', thisVar))) {
			warning(paste0('There is no folder for ', thisVar, '.'))
		} else {
		
			### get available years
			yearsAvail <- list.dirs(paste0(prDir, '/', thisVar, '/monthly'), full.names=FALSE, recursive=FALSE)
			yearsAvail <- as.integer(yearsAvail)

			earliestRasterDate <- paste0(min(yearsAvail), '-01')
			latestRasterDate <- paste0(max(yearsAvail), '-12')

			### create data frame to store extracted values
			monthsNeeded <- 12 * windowYears + windowMonths
			thisOut <- matrix(NA, nrow=nrowOut, ncol=monthsNeeded + 1)
			colnames(thisOut) <- paste0(thisVar, '_', monthsNeeded:0, 'monthsPrior')

			### extract by date
			for (countDate in seq_along(uniqueDates)) {
			
				thisDate <- uniqueDates[countDate]
				if (verbose) omnibus::say(thisVar, ' ', thisDate)
			
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
					locs <- x[index]
					args <- list(x = rasts, y = locs)
					args <- c(args, dots)
					ext <- do.call(terra::extract, args = args)
					if (!isPoints) ext$weight <- NULL
					ext <- as.matrix(ext)

					# remember
					if (isPoints) {
						rowIndex <- index
					} else {
						rowIndex <- which(substr(meta$date, 1, 7) == thisDate)
					}

					lastCol <- ncol(thisOut) - monthDiff(thisDate, windowEndDate)
					firstCol <- lastCol - ncol(ext) + 1
					thisOut[rowIndex, firstCol:lastCol] <- ext
					
				}
				
			} # next date

			out <- if (exists('out', inherits=FALSE)) {
				cbind(out, thisOut)
			} else {
				thisOut
			}
			
		} # folder for variable exists
		
	} # next variable
	
	# remove unwanted columns
	while (all(is.na(out[ , 1]))) out <- out[ , 2:ncol(out), drop=FALSE]

	meta$date <- NULL
	
	if (any(c(!idTRUE, !weightsTRUE, !cellsTRUE, !xyTRUE))) {
	
		bads <- c('ID', 'weight', 'cell', 'x', 'y')[c(!idTRUE, !weightsTRUE, !cellsTRUE, !xyTRUE, !xyTRUE)]
		keeps <- which(omnibus::notIn(colnames(meta), bads))
		meta <- meta[ , keeps, drop = FALSE]
		
	
	}
	if (nrow(meta) > 0) out <- cbind(meta, out)
	
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
	windowYears = 0,
	verbose = TRUE,
	...
) {

	### get dates
	dates <- prGetDates(x, date)

	# get record years
	recordYears <- lubridate::year(dates)
	uniqueYears <- sort(unique(recordYears))

	### type and errors
	isPoints <- terra::geomtype(x) == 'points'
	if (!isPoints & is.null(template)) stop('To extract values to a line or polygon vector, you need to supply a template raster.')
	
	### arguments for terra::extract()	
	dots <- list(...)
	idTRUE <- if (any(names(dots) == 'ID')) { dots$ID } else { FALSE }
	weightsTRUE <- if (any(names(dots) == 'weights') & !isPoints) { dots$weights } else { FALSE }
	cellsTRUE <- if (any(names(dots) == 'cells')) { dots$cells } else { FALSE }
	xyTRUE <- if (any(names(dots) == 'xy')) { dots$xy } else { FALSE }
	dots$ID <- FALSE
	dots$weights <- TRUE # needs to be TRUE to get fractional cells!
	dots$cells <- FALSE
	dots$xy <- FALSE
	
	### metadata (ID, cell number, weights, coordinates) on extraction from PRISM
	meta <- .prMeta(x = x, template = template, dots = dots, dates = dates)
	nrowOut <- nrow(meta)

	### by VARIABLE
	for (thisVar in vars) {
	
		if (!dir.exists(paste0(prDir, '/', thisVar))) {
			warning(paste0('There is no folder for ', thisVar, '.'))
		} else {
		
			### get available years
			yearsAvail <- list.dirs(paste0(prDir, '/', thisVar, '/', annualDir), full.names=FALSE, recursive=FALSE)
			yearsAvail <- as.integer(yearsAvail)
			
			earliestRasterYear <- min(yearsAvail)
			latestRasterYear <- max(yearsAvail)

			### create matrix to store extracted values
			thisOut <- matrix(NA, nrow=nrowOut, ncol=windowYears + 1)
			colnames(thisOut) <- paste0(thisVar, '_', windowYears:0, 'yearsPrior')

			### extract by date
			for (countYear in seq_along(uniqueYears)) {
			
				year <- uniqueYears[countYear]
				if (verbose) omnibus::say(thisVar, ' ', as.integer(year))
			
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
					locs <- x[index]
					args <- list(x = rasts, y = locs)
					args <- c(args, dots)
					ext <- do.call(terra::extract, args = args)
					if (!isPoints) ext$weight <- NULL
					ext <- as.matrix(ext)

					# remember
					if (isPoints) {
						rowIndex <- index
					} else {
						rowIndex <- which(substr(meta$date, 1, 4) == year)
					}

					deltaYears <- if (year < latestRasterYear) {
						0
					} else {
						year - latestRasterYear
					}
					
					lastCol <- ncol(thisOut) - deltaYears
					firstCol <- lastCol - ncol(ext) + 1
					thisOut[rowIndex, firstCol:lastCol] <- ext
			
				} # if record date falls within years available in PRISM
			
			} # next date

			out <- if (exists('out', inherits=FALSE)) {
				cbind(out, thisOut)
			} else {
				thisOut
			}
	
		} # folder for variable exists
	
	} # next variable

	# remove unwanted columns
	while (all(is.na(out[ , 1]))) out <- out[ , 2:ncol(out), drop=FALSE]

	meta$date <- NULL
	
	if (any(c(!idTRUE, !weightsTRUE, !cellsTRUE, !xyTRUE))) {
	
		bads <- c('ID', 'weight', 'cell', 'x', 'y')[c(!idTRUE, !weightsTRUE, !cellsTRUE, !xyTRUE, !xyTRUE)]
		keeps <- which(omnibus::notIn(colnames(meta), bads))
		meta <- meta[ , keeps, drop = FALSE]
		
	
	}
	if (nrow(meta) > 0) out <- cbind(meta, out)
	
	out

}
