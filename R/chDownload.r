#' @name chDownload
#' @rdname chDownload
#' @title Download CHELSA TRACE21K (paleo) climate rasters

#' @description These functions download \href{https://chelsa-climate.org/}{CHELSA} climate rasters, elevation, and permanent ice/snow cover and elevation thereof:
#' \itemize{
#'		\item \code{chDownloadPaleoBio}: Paleo/future climate BIOCLIM variables.
#'		\item \code{chDownloadPaleoClim}: Paleo climate "basic" variables (precipitation, temperature).
#'		\item \code{chDownloadPaleoElev}: Paleo elevation and ice cover.
#'		\item \code{chDownloadFutureBio}: Paleo/future climate BIOCLIM variables.
#'		\item \code{chDownloadTODOTODOTODO!!!!!!!!!!!!!!!!!!!!}: OTHER
#'	}
#'
#' @param saveTo Name of the base path to which to save the download. Subfolders will be created within this folder.
#' @param ver Version number. Valid version numbers deped on whether paleo, historical, or future coverages are desired:
#' \itemize{
#'		\item	Paleo (TraCE2K): 1.0
#'		\item	Historical: XYZ
#'		\item	Future under CMIP5: 1.2
#'		\item	Future under CMIP6: 2.1
#' }
#' @param vars Name(s) of variable(s) to download. Valid values depend on the version of WorldClim and whether near-present day or future rasters are fetched. Different versions and time periods of WorldClim use different names for the same variable (e.g., "prec" versus "ppt" versus "pr" for precipitation). To reduce confusion, variable names have been standardized (in this package) to be the same across versions and times. Valid values are:
#' \itemize{
#' 	\item \code{elev}: elevation (function \code{chDownloadPaleoElev})
#' 	\item \code{iceMask}: mask for ice cover (\code{1} for ice or \code{NA} for none) (function \code{chDownloadPaleoElev})
#' 	\item \code{iceElev}: elevation of ice (plus land) (function \code{chDownloadPaleoElev})
#' 	\item \code{snowCoverDays}: days of snow cover (function \code{chDownloadPaleoClim})
#' 	\item \code{swe}: snow water equivalent (function \code{chDownloadPaleoClim})
#' 	\item \code{tmin}: minimum temperature (function \code{chDownloadPaleoClim})
#' 	\item \code{tmax}: maximum temperature (function \code{chDownloadPaleoClim})
#' 	\item \code{ppt}: accumulated precipitation (function \code{chDownloadPaleoClim})
#' }
#' @param cmip Number the Coupled Model Intercomparison Project (CMIP) future rasters to download. Valid values are either \code{5} or \code{6}.
#' @param esm Abbreviations of available earth system models, depending on whether CMIP5 or CMIP6 is desired. A list of valid options can be obtained using \code{\link{chEsm}}.
#' @param ghg Name of the emissions scenario of future rasters. Valid values depend on \code{cmip} (CMIP). For CMIP5 These are:
#' \itemize{
#' 		\item	\code{2.6}: RCP2.6
#' 		\item	\code{4.5}: RCP4.5
#' 		\item	\code{6.0}: RCP6.0
#' 		\item	\code{8.5}: RCP8.5
#' }
#' For CMIP6 these are:
#' \itemize{
#' 		\item	\code{126}: SSP126
#' 		\item	\code{370}: SSP370
#' 		\item	\code{585}: SSP585
#' }
#' @param period Period from which to earth system model climate predictions (which includes some "historic" predictions). The first year of the respective time period is used. Depending on \code{cmip} (CMIP), different values are valid. A list of valid values can be see using \code{\link{chPeriod}}.
#' @param centuries Century or centuries from which to download the paleo-climate/elevation/ice rasters. This can be a value from -200 (the -200th century, or 21000 to 20001 ybp) to 20 (i.e., the 20th century, 1900-1999 CE).
#' @param bios The number of BIOCLIM variable(s) to get (any integers in \code{1:19}).
#' @param months The number of month(s) to get (any integers in \code{1:12}).
#' @param forceUpdate If \code{FALSE} (default), then all existing rasters will be overwritten, regardless of whether or not the existing ones are up-to-date or not.
#' @param verbose If \code{TRUE} (default), display progress.
#' @references
#' Karger, D. N., Nobis, M. P., Normand, S., Graham, C. H., & Zimmermann, N. E. (2021): CHELSA-TraCE21k v1. 0. Downscaled transient temperature and precipitation data since the last glacial maximum. \emph{Climate of the Past Discussions} 1-27. doi: \href{https://dx.doi.org/10.5194/cp-2021-30}{10.5194/cp-2021-30}.
#' @examples
#' 
#' \dontrun{
#' dl <- 'C:/ecology/!Scratch/tc'
#' chDownloadMonthly(dl, 'tmin', 1958)
#' chDownloadElev(dl, 'elev')
#' 
#' }
#' @export

chDownloadPaleoBio <- function(
	saveTo,
	ver,
	bios,
	centuries,
	forceUpdate = FALSE,
	verbose = TRUE
) {

	if (!all(centuries %in% -200:20)) stop('Argument "centuries" must be in the range of -200:20.')
	if (!all(bios %in% 1:19)) stop('Argument "bios" must be in the range of 1:19.')

	success <- expand.grid(var='bio', centuries=centuries, bio=bios, updated=NA)
	
	for (thisCentury in centuries) {

		# save to
		saveToAppended <- paste0(saveTo, '/chelsa_v', ver, '_bio_trace21k')
		dir.create(saveToAppended, showWarnings=FALSE, recursive=TRUE)

		for (thisBio in bios) {

			if (verbose) cat('century', thisCentury, 'bio', thisBio); flush.console()

			# file name and URL
			rastBaseURL <- 'https://os.zhdk.cloud.switch.ch/envicloud/chelsa/chelsa_V1/chelsa_trace/bio/'
			rastFileName <- paste0('CHELSA_TraCE21k_bio', sprintf('%02.0f', thisBio), '_', centuries, '_V', sprintf('%.1f', ver), '.tif')
			
			url <- paste0(rastBaseURL, rastFileName)
			filePath <- paste0(saveToAppended, '/', rastFileName)

			onDisk <- file.exists(paste0(saveToAppended, '/', rastFileName))
			doUpdate <- forceUpdate | !onDisk
			
			downloaded <- FALSE
			if (doUpdate) {

				if (verbose) cat(' | fetching'); flush.console()
				tryNumber <- 1
				
				while (tryNumber <= 10 & !downloaded) {
					
					downloaded <- TRUE
					tryCatch(
						httr::GET(url, httr::write_disk(filePath, overwrite=TRUE)),
						# utils::download.file(url, destfile=filePath, method='auto', quiet=TRUE),
						error=function(e) { downloaded <<- FALSE }
					)
					
					if (nrow(success) > 1) Sys.sleep(1)
					
					tryNumber <- tryNumber + 1
				}

				Sys.sleep(1)
				
			} else {
				if (verbose) cat(' | skipping\n'); flush.console()
			}

			if (downloaded & verbose) cat(' | downloaded\n'); flush.console()

			this <- which(success$centuries == thisCentury & success$bio == thisBio)
			success$updated[this] <- downloaded
			
		} # next "number" (BIOCLIM or month)
		
	} # next centuries

	success

}

chDownloadPaleoClim <- function(
	saveTo,
	ver,
	vars,
	centuries,
	months,
	forceUpdate = FALSE,
	verbose = TRUE
) {

	if (!all(centuries %in% -200:20)) stop('Argument "centuries" must be in the range of -200:20.')
	if (!all(months %in% 1:12)) stop('Argument "months" must be in the range of 1:12.')

	fileVars <- convertVar('ch', vars=vars, ver=ver, period='trace21k', standardToFile=TRUE)

	success <- expand.grid(var=vars, century=centuries, month=months, updated=NA)
	
	for (countVar in seq_along(vars)) {

		thisVar <- vars[countVar]
		thisVarFile <- fileVars[countVar]

		for (thisCentury in centuries) {
	
			# save to
			saveToAppended <- paste0(saveTo, '/chelsa_v', ver, '_', thisVar, '_trace21k')
			dir.create(saveToAppended, showWarnings=FALSE, recursive=TRUE)

			for (thisMonth in months) {
	
				if (verbose) cat(thisVar, 'century', thisCentury, 'month', thisMonth); flush.console()

				# file name and URL
				rastBaseURL <- paste0('https://os.zhdk.cloud.switch.ch/envicloud/chelsa/chelsa_V', ver, '/chelsa_trace/', thifVarFile, '/')
				rastFileName <- paste0('CHELSA_TraCE21k_', thisVarFile, '_', centuries, '_', thisMonth, '_V', sprintf('%.1f', ver), '.tif')
				
				url <- paste0(rastBaseURL, rastFileName)
				filePath <- paste0(saveToAppended, '/', rastFileName)

				onDisk <- file.exists(paste0(saveToAppended, '/', rastFileName))
				doUpdate <- forceUpdate | !onDisk
				
				downloaded <- FALSE
				if (doUpdate) {

					if (verbose) cat(' | fetching'); flush.console()
					tryNumber <- 1
					
					while (tryNumber <= 10 & !downloaded) {
						
						downloaded <- TRUE
						tryCatch(
							httr::GET(url, httr::write_disk(filePath, overwrite=TRUE)),
							# utils::download.file(url, destfile=filePath, method='auto', quiet=TRUE),
							error=function(e) { downloaded <<- FALSE }
						)
						
						if (nrow(success) > 1) Sys.sleep(1)
						
						tryNumber <- tryNumber + 1
					}

					Sys.sleep(1)
					
				} else {
					if (verbose) cat(' | skipping\n'); flush.console()
				}

				if (downloaded & verbose) cat(' | downloaded\n'); flush.console()

				this <- which(success$var == thisVar & success$centuries == thisCentury & success$month == thisMonth)
				success$updated[this] <- downloaded
				
			} # next "number" (BIOCLIM or month)
			
		} # next centuries
			
	} # next variable 

	success

}

chDownloadPaleoElev <- function(
	saveTo,
	ver,
	vars,
	centuries,
	forceUpdate = FALSE,
	verbose = TRUE
) {

	if (!all(centuries %in% -200:20)) stop('Argument "centuries" must be in the range of -200:20.')

	fileVars <- convertVar('ch', vars=vars, ver=ver, period='trace21k', standardToFile=TRUE)

	success <- expand.grid(var=vars, centuries=centuries, updated=NA)
	
	for (countVar in seq_along(vars)) {

		thisVar <- vars[countVar]
		thisVarFile <- fileVars[countVar]

		for (thisCentury in centuries) {
	
			if (verbose) cat(thisVar, 'century', thisCentury); flush.console()

			# save to
			saveToAppended <- paste0(saveTo, '/chelsa_v', ver, '_', thisVar, '_trace21k')
			dir.create(saveToAppended, showWarnings=FALSE, recursive=TRUE)

			# file name and URL
			rastBaseURL <- 'https://os.zhdk.cloud.switch.ch/envicloud/chelsa/chelsa_V1/chelsa_trace/orog/'
			rastFileName <- paste0('CHELSA_TraCE21k_', thisVarFile, '_', centuries, '_V', sprintf('%.1f', ver), '.tif')
				
			url <- paste0(rastBaseURL, rastFileName)
			filePath <- paste0(saveToAppended, '/', rastFileName)

			onDisk <- file.exists(paste0(saveToAppended, '/', rastFileName))
			doUpdate <- forceUpdate | !onDisk
			
			downloaded <- FALSE
			if (doUpdate) {

				if (verbose) cat(' | fetching'); flush.console()
				tryNumber <- 1
				
				while (tryNumber <= 10 & !downloaded) {
					
					downloaded <- TRUE
					tryCatch(
						httr::GET(url, httr::write_disk(filePath, overwrite=TRUE)),
						# utils::download.file(url, destfile=filePath, method='auto', quiet=TRUE),
						error=function(e) { downloaded <<- FALSE }
					)
					
					if (nrow(success) > 1) Sys.sleep(1)
					
					tryNumber <- tryNumber + 1
				}

				Sys.sleep(1)
				
			} else {
				if (verbose) cat(' | skipping\n'); flush.console()
			}

			if (downloaded & verbose) cat(' | downloaded\n'); flush.console()

			this <- which(success$var == thisVar & success$centuries == thisCentury)
			success$updated[this] <- downloaded
			
			
		} # next centuries
			
	} # next variable 

	success

}

chDownloadFutureBio <- function(
	saveTo,
	ver,
	bios,
	cmip,
	period,
	esm,
	ghg,
	forceUpdate = FALSE,
	verbose = TRUE
) {

	if (length(cmip) > 1) stop('Only one CMIP can be downloaded at a time.')
	if (!all(bios %in% 1:19)) stop('Argument "bios" must be in the range of 1:19.')

	chCheckEsm_internal(cmip=cmip, esm)

	success <- expand.grid(var='bio', bio=bios, cmip=cmip, period=period, ghg=ghg, esm=esm, updated=NA)
	
	if (!chIsPeriodLong_internal(period)) {
		period <- chConvertPeriod(cmip, period=period)
	}

	for (thisPeriod in period) {

		for (thisGhg in ghg) {
		
			ghgNice <- if (cmip == 5) {
				paste0('rcp', thisGhg)
			} else if (cmip == 6) {
				paste0('ssp', thisGhg)
			}
		
			for (thisEsm in esm) {
			
				# save to
				saveToAppended <- paste0(saveTo, '/chelsa_v', ver, '_', thisPeriod, '_', ghgNice, '_', thisEsm, '_bio')
				dir.create(saveToAppended, showWarnings=FALSE, recursive=TRUE)

				for (thisBio in bios) {

					if (verbose) cat(thisPeriod, ghgNice, thisEsm, 'bio', thisBio); flush.console()

					# file name and URL
					if (cmip == 5) {
					
					} else if (cmip == 6) {
						
						rastBaseURL <- paste0('https://os.zhdk.cloud.switch.ch/envicloud/chelsa/chelsa_V2/GLOBAL/climatologies/', thisPeriod, '/', thisEsm, '/', ghgNice, '/bio/')
						rastFileName <- paste0('CHELSA_bio', thisBio, '_', thisPeriod, '_', tolower(thisEsm), '_', ghgNice, '_V.', ver, '.tif')
					
					}	
						
					url <- paste0(rastBaseURL, rastFileName)
					filePath <- paste0(saveToAppended, '/', rastFileName)

					onDisk <- file.exists(paste0(saveToAppended, '/', rastFileName))
					doUpdate <- forceUpdate | !onDisk
					
					downloaded <- FALSE
					if (doUpdate) {

						if (verbose) cat(' | fetching'); flush.console()
						tryNumber <- 1
						
						while (tryNumber <= 10 & !downloaded) {
							
							downloaded <- TRUE
							tryCatch(
								httr::GET(url, httr::write_disk(filePath, overwrite=TRUE)),
								# utils::download.file(url, destfile=filePath, method='auto', quiet=TRUE),
								error=function(e) { downloaded <<- FALSE }
							)
							
							if (nrow(success) > 1) Sys.sleep(1)
							
							tryNumber <- tryNumber + 1
						}

						Sys.sleep(1)
						
					} else {
						if (verbose) cat(' | skipping\n'); flush.console()
					}

					if (downloaded & verbose) cat(' | downloaded\n'); flush.console()

					this <- which(success$period == thisPeriod & success$ghg == thisGhg & success$esm == thisEsm & success$bio == thisBio)
					success$updated[this] <- downloaded
					
				} # next "number" (BIOCLIM or month)
				
			} # next ESM
			
		} # next GHG
		
	} # next period

	success

}

