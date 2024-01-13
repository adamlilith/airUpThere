#' @name prExtractAbsolute
#' @rdname prExtractAbsolute
#' @title Extract values from PRISM across specified time periods

#' @description These functions extract values from interpolated weather rasters from the Parameter Regression on Independent Slopes (PRISM) data product across a range of dates, and the range can vary for each geometry (point, line, or polygon). For example, it could extract all values from 2000-04-22 to 2022-04-22 for one point, and from 2019-01-01 to 2021-12-31 for another. If you wish to extract values across a fixed length of time relative to a specific date (e.g., the 100 days prior to sampling at each point), then see [prExtractRelative()]. Extractions are done at points (versus polygons or lines, for example).
#'
#' The basic input is an object of class `SpatVector`, or a `data.frame` or `matrix`, with each row representing a point location. The function also needs to be pointed toward a folder with PRISM data. The folder with PRISM data must be structured as:
#' * Base folder (e.g., `'C:/PRISM/an81'` or `'C:/PRISM/lt81'`), which contains:
#' * A folder named either `daily`, `monthly`, `annual`, which contains:
#' * One folder per variable (e.g., `tmin`, `tmax`, `vpdmax`), each of which contain:
#' * One folder per year (e.g., `1981`, code`1982`, etc.), each of which rasters like:
#'      * One raster per day named like `prism_tmin_us_30s_19810101.bil`, `prism_tdmean_us_30s_19810102.bil`, etc.
#'      * One raster per month named like `prism_tmin_us_30s_198101.bil`, `prism_tdmean_us_30s_198101.bil`, etc.
#'      * One raster representing the annual value named like `prism_tmin_us_30s_1981.bil`.
#'
#' The function can extract values corresponding to the day/month/year of each record, plus (optionally) a user-specified window of time prior to the day/month/year of each record. For example, you could use this to extract daily climate data for a site collected on April 22, 2014, and all days prior up to 10 years (April 23, 2004). This function is really a fancy wrapper for [terra::extract()], but it does the tough job of getting the directory structures right, pulling all needed rasters, and efficiently grouping records to speed extraction.
#'
#' The function does not assume that data for all PRISM years are available, but it does assume that all relevant rasters for a particular year are available within each yearly folder. If rasters preceding a date only partially cover the window, then values for the part covered will be extracted. For example if you try to extract annual values for a window spanning 2010 to 2020 but only have available rasters for 1981 to 2018, then values for 2010 to 2018 will be extracted. Values that cannot be extracted are represented by `NA` in the output.
#'
#' @param x `SpatVector`.
#'
#' @param startDate,endDate These define the beginning/ending date across which values are to be extracted. Either:
#' * Object of class `Date`. You can name a single date, in which case this date will be used for all records, or a vector of dates, one per point in `x`.
#' * Name of column in `x` with date of each record. Values must be of in YYYY-MM-DD (year-month-day of month) format *or* already of class [Date()]. See [lubridate::ymd()] or related functions for more help.
#' * Names of columns in `x` with year, month, and day of month of each record (in that order). Months must be numeric (i.e., 10, not "October").
#'
#' @param prDir Character. Path of the base directory in which the rasters are stored. It must have a structure with subfolders as described above.
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
#' @param rastSuffix Character. The "suffix" at the end of each weather raster. PRISM rasters are usually shipped in 'BIL' format, so normally this should be `bil`. However, any other suffix corresponding to a raster type that can be opened by the [terra:rast()] function can be used if the rasters have been converted to another format.
#'
#' @param annualDir Name of the highest-level folder in which annual rasters are stored. If you use the `prDownloadAnnual()` function to download PRISM rasters, this will be `'annual'` (default). However, if you are extracting values from a purchased version of PRISM (i.e., for which you have a license), the annual rasters are typically stored in the folder called `'monthly'`.
#'
#' @param template Either `NULL` (default), or `SpatRaster` or path and filename of a PRISM raster. This argument is used to set the dimensions of the output data frame. If `x` is a "points" `SpatVector`, then `template` can be `NULL`, but cell number and coordinate cannot be returned (i.e., setting `cells = TRUE` and `xy = TRUE` in the `...` part of the function call). If `x` is a "lines" or "polygons" vector, then this *must* be supplied so that the function can ascertain how many rows will be in the output table.
#'
#' @param removeNAs Logical: If `TRUE`, remove columns in the output where all values are `NA`. The default is `FALSE`, which returns all columns, but this can result in a very large matrix.
#' 
#' @param verbose Logical: If `TRUE` (default), show progress.
#'
#' @param ... Argument(s) to pass to [terra::extract()].
#'
#' @returns Matrix. `NA` values represent days/months/years that did not fall within the specified window or for which rasters were unavailable. 
#'
#' @example ./man/examples/ex_prExtractAbsolute.r
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
	template = NULL,
	removeNAs = FALSE,
	verbose = TRUE,
	...
) {

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

	### get unique dates ranges
	startDates <- prGetDates(x, startDate)
	endDates <- prGetDates(x, endDate)
	
	if (any(startDates %>d% endDates)) stop('At least one start date occurs after its end date.')
	
	startEndDates <- data.frame(startDates=startDates, endDates=endDates)
	startEndDates <- startEndDates[!duplicated(startEndDates), ]

	# sort by end date then start date
	dateOrder <- order(startEndDates$endDates, startEndDates$startDates)
	startEndDates <- startEndDates[dateOrder, ]
	
	startEndDates$startDates <- lubridate::ymd(startEndDates$startDate)
	startEndDates$endDates <- lubridate::ymd(startEndDates$endDate)
	
	firstStartDate <- startEndDates$startDates[1]
	lastEndDate <- startEndDates$endDates[nrow(startEndDates)]

	### metadata (ID, cell number, weights, coordinates) on extraction from PRISM
	meta <- .prMeta(x = x, template = template, dots = dots, dates = NULL, startDates = startDates, endDates = endDates)
	nrowOut <- nrow(meta)

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
			# availRasterDates <- seq(earliestRasterDate, latestRasterDate, by = '1 day')
			
			# get first date of extraction (first raster available or first date, if later than date of first raster)
			firstExtractDate <- if (earliestRasterDate %<d% firstStartDate) {
				firstStartDate
			} else {
				earliestRasterDate
			}
			
			# get last date of extraction (last raster available or last date, if earlier than date of last raster)
			lastExtractDate <- if (latestRasterDate %>d% lastEndDate) {
				lastEndDate
			} else {
				latestRasterDate
			}

			# create temporary data object to store extraction
			datesNeeded <- seq(firstExtractDate, lastExtractDate, by='1 day')
			numNeeded <- length(datesNeeded)
			
			thisOut <- matrix(NA, nrow=nrowOut, ncol=numNeeded)
			colnames(thisOut) <- paste0(thisVar, '_', datesNeeded)

			### by date range
			for (countDate in 1:nrow(startEndDates)) {
			
				thisStartDate <- startEndDates$startDate[countDate]
				thisEndDate <- startEndDates$endDate[countDate]
				
				if (verbose) omnibus::say(thisVar, ' ', thisStartDate, ' through ', thisEndDate)

				# thisDatesNeeded <- seq(thisStartDate, thisEndDate, by='1 day')

				# if (any(thisDatesNeeded %in% availRasterDates)) {
				if ((!(thisStartDate %>d% latestRasterDate | thisEndDate %<d% earliestRasterDate))) {

					# get start/end dates of extraction (determined by available rasters)
					thisStartExtractDate <- thisStartDate
					thisEndExtractDate <- thisEndDate
					
					if (thisStartExtractDate %<d% earliestRasterDate) thisStartExtractDate <- earliestRasterDate
					if (thisEndExtractDate %>d% latestRasterDate) thisEndExtractDate <- latestRasterDate
				
					# rasters
					rasts <- prStack(prDir=prDir, vars=thisVar, dates=c(thisStartExtractDate, thisEndExtractDate), by='day', span=TRUE, res=res, rastSuffix=rastSuffix)
					
					# extract to records with this date
					index <- which(startDates == thisStartDate & endDates == thisEndDate)
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
						rowIndex <- which(thisStartDate == meta$startDate & thisEndDate == meta$endDate)
					}

					datesExtracted <- seq(thisStartExtractDate, thisEndExtractDate, by='1 day')
					cols <- paste0(thisVar, '_', datesExtracted)
					thisOut[rowIndex, cols] <- ext
					
				} # if desired date range is at least partially within available date range
				
			} # next date range
			
			out <- if (exists('out', inherits=FALSE)) {
				cbind(out, thisOut)
			} else {
				thisOut
			}

		} # if variable exists
		
	} # next variable

	# remove columns that are all NAs
	if (removeNAs) {
	
		nasFx <- function(z) all(is.na(z))
		removes <- which(apply(out, 2, nasFx))
		out <- out[ , -removes, drop = FALSE]
	
	}
	
	meta$startDate <- meta$endDate <- NULL
	
	# remove unwanted columns from meta
	if (any(c(!idTRUE, !weightsTRUE, !cellsTRUE, !xyTRUE))) {
	
		bads <- c('ID', 'weight', 'cell', 'x', 'y')[c(!idTRUE, !weightsTRUE, !cellsTRUE, !xyTRUE, !xyTRUE)]
		keeps <- which(omnibus::notIn(colnames(meta), bads))
		meta <- meta[ , keeps, drop = FALSE]
	
	}
	if (nrow(meta) > 0) out <- cbind(meta, out)
	
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
	template = NULL,
	removeNAs = FALSE,
	verbose = TRUE,
	...
) {

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
	
	### metadata (ID, cell number, weights, coordinates) on extraction from PRISM
	meta <- .prMeta(x = x, template = template, dots = dots, dates = NULL, startDates = startDates, endDates = endDates)
	nrowOut <- nrow(meta)

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
			
			thisOut <- matrix(NA, nrow=nrowOut, ncol=numNeeded)
			colnames(thisOut) <- paste0(thisVar, '_', datesNeeded)

			### by date range
			for (countDate in 1:nrow(startEndDates)) {
			
				thisStartDate <- startEndDates$startDate[countDate]
				thisEndDate <- startEndDates$endDate[countDate]
				
				if (verbose) cat(paste0(thisVar, ' ', thisStartDate, ' through ', thisEndDate, '\n')); flush.console()

				# if desired date range is at least partially within available date range
				if (!(thisStartDate %>d% latestRasterDate | thisEndDate %<d% earliestRasterDate)) {

					# get start/end dates of extraction (determined by available rasters)
					thisStartExtractDate <- thisStartDate
					thisEndExtractDate <- thisEndDate
					
					if (thisStartExtractDate %<d% earliestRasterDate) thisStartExtractDate <- earliestRasterDate
					if (thisEndExtractDate %>d% latestRasterDate) thisEndExtractDate <- latestRasterDate
				
					# rasters
					rasts <- prStack(prDir=prDir, vars=thisVar, dates=c(thisStartExtractDate, thisEndExtractDate), by='month', span=TRUE, res=res, rastSuffix=rastSuffix)
					
					# extract to records with this date
					index <- which(startDates == thisStartDate & endDates == thisEndDate)
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
						rowIndex <- which(thisStartDate == meta$startDate & thisEndDate == meta$endDate)
					}
					datesExtracted <- seqMonths(thisStartExtractDate, thisEndExtractDate)
					cols <- paste0(thisVar, '_', datesExtracted)
					thisOut[rowIndex, cols] <- ext
					
				} # if desired date range is at least partially within available date range
				
			} # next date range
			
			out <- if (exists('out', inherits=FALSE)) {
				cbind(out, thisOut)
			} else {
				thisOut
			}
				
		} # if variable exists
		
	} # next variable
	
	# remove columns that are all NAs
	if (removeNAs) {
	
		nasFx <- function(z) all(is.na(z))
		removes <- which(apply(out, 2, nasFx))
		out <- out[ , -removes, drop = FALSE]
	
	}
	
	meta$startDate <- meta$endDate <- NULL
	
	# remove unwanted columns from meta
	if (any(c(!idTRUE, !weightsTRUE, !cellsTRUE, !xyTRUE))) {
	
		bads <- c('ID', 'weight', 'cell', 'x', 'y')[c(!idTRUE, !weightsTRUE, !cellsTRUE, !xyTRUE, !xyTRUE)]
		keeps <- which(omnibus::notIn(colnames(meta), bads))
		meta <- meta[ , keeps, drop = FALSE]
	
	}
	if (nrow(meta) > 0) out <- cbind(meta, out)
	
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
	annualDir = 'annual',
	template = NULL,
	removeNAs = FALSE,
	verbose = TRUE,
	...
) {

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
	
	### metadata (ID, cell number, weights, coordinates) on extraction from PRISM
	meta <- .prMeta(x = x, template = template, dots = dots, dates = NULL, startDates = startDates, endDates = endDates)
	nrowOut <- nrow(meta)

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
			
			thisOut <- matrix(NA, nrow=nrowOut, ncol=numNeeded)
			colnames(thisOut) <- paste0(thisVar, '_', datesNeeded)

			### by date range
			for (countDate in 1:nrow(startEndDates)) {
			
				thisStartDate <- startEndDates$startDate[countDate]
				thisEndDate <- startEndDates$endDate[countDate]
				
				if (verbose) omnibus::say(thisVar, ' ', thisStartDate, ' through ', thisEndDate)

				# if desired date range is at least partially within available date range
				if (!(thisStartDate %>d% latestRasterDate | thisEndDate %<d% earliestRasterDate)) {

					# get start/end dates of extraction (determined by available rasters)
					thisStartExtractDate <- thisStartDate
					thisEndExtractDate <- thisEndDate
					
					if (thisStartExtractDate %<d% earliestRasterDate) thisStartExtractDate <- earliestRasterDate
					if (thisEndExtractDate %>d% latestRasterDate) thisEndExtractDate <- latestRasterDate
				
					# rasters
					rasts <- prStack(prDir=prDir, vars=thisVar, dates=c(thisStartExtractDate, thisEndExtractDate), by='year', span=TRUE, res=res, rastSuffix=rastSuffix, annualDir=annualDir)
					
					# extract to records with this date
					index <- which(startDates == thisStartDate & endDates == thisEndDate)
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
						rowIndex <- which(thisStartDate == meta$startDate & thisEndDate == meta$endDate)
					}
					datesExtracted <- thisStartExtractDate:thisEndExtractDate
					cols <- paste0(thisVar, '_', datesExtracted)
					thisOut[rowIndex, cols] <- ext
					
				} # if desired date range is at least partially within available date range
				
			} # next date range
			
			out <- if (exists('out', inherits=FALSE)) {
				cbind(out, thisOut)
			} else {
				thisOut
			}
				
		} # if variable exists
		
	} # next variable
	
	# remove columns that are all NAs
	if (removeNAs) {
	
		nasFx <- function(z) all(is.na(z))
		removes <- which(apply(out, 2, nasFx))
		out <- out[ , -removes, drop = FALSE]
	
	}
	
	meta$startDate <- meta$endDate <- NULL
	
	# remove unwanted columns from meta
	if (any(c(!idTRUE, !weightsTRUE, !cellsTRUE, !xyTRUE))) {
	
		bads <- c('ID', 'weight', 'cell', 'x', 'y')[c(!idTRUE, !weightsTRUE, !cellsTRUE, !xyTRUE, !xyTRUE)]
		keeps <- which(omnibus::notIn(colnames(meta), bads))
		meta <- meta[ , keeps, drop = FALSE]
	
	}
	if (nrow(meta) > 0) out <- cbind(meta, out)
	
	out

}
