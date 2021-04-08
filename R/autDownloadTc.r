#' Download TerraClimate climate rasters
#'
#' This function downloads \emph{monthly} (TerraClimate)<http://www.climatologylab.org/terraclimate.html> climate rasters.
#'
#' @param saveTo Name of the base path to which to save the download. Subfolders will be created within this folder.
#' @param var Name(s) of variable(s) to download. Valid values include:depend on the version of WorldClim and whether near-present day or future rasters are fetched. Different versions and time periods of WorldClim use different names for the same variable (e.g., "prec" versus "ppt" versus "pr" for precipitation). To reduce confusion, variable names have been standardized (in this package) to be the same across versions and times. Valid values are:
#' \itemize{
#' 	\item \code{elev}: elevation ("file" name: \code{elev})
#' 	\item \code{tmin}: minimum temperature
#' 	\item \code{tmax}: maximum temperature
#' 	\item \code{ppt}: accumulated precipitation
#' 	\item \code{swe}: snow water equivalent
#' 	\item \code{aet}: actual evapotranspiration
#' 	\item \code{pet}: reference (potential) evapotranspiration (ET0)
#' 	\item \code{def}: climate water deficit
#' 	\item \code{PDSI}: Palmer Drought Severity Index
#' 	\item \code{soil}: extractable soil moisture
#' 	\item \code{srad}: downward shortwave flux at the surface (solar radiation)
#' 	\item \code{wind}: average wind speed ("file" name: \code{ws})
#' 	\item \code{q}: cumulative streamflow
#' 	\item \code{vap}: vapor pressure
#' 	\item \code{vpd}: vapor pressure deficit
#' }
#' @param year Year(s) of the period from which to download monthly climate rasters. Valid years include 1958 through the present (often one or two years prior to the current year). This can be \code{NULL} only if the variable being downloaded is \code{elevation}.
#' @param update If \code{TRUE} (default) and a raster is already saved to disk, update the raster if a newer version is available.
#' @param forceUpdate If \code{TRUE}, update existing rasters even if the version saved to disk is the same as the version that is available. The default is \code{FALSE}.
#' @param verbose If \coed{TRUE} (default), display progress.
#' @return NetCDF rasters are saved to disk. The function also returns a data frame indicating if the desired file(s) were already on the disk and if they were downloaded.
#' @references
#' Abatzoglou, J.T., Dobrowski, S.Z., Parks, S.A., and Hegewisch, K.C. 2018. TerraClimate, a high-resolution global dataset of monthly climate and climatic water balance from 1958-2015. \emph{Scientific Data} 5:170191. doi: \href{https://dx.doi.org/10.1038/sdata.2017.191}{10.1038/sdata.2017.191}.
#' @examples
#' 
#' \dontrun{
#' dl <- 'C:/ecology/!Scratch/tc' 
#' autDownloadTc(dl, 'tmin', 1958)
#' autDownloadTc(dl, 'elev')
#' 
#' }
#' @export

autDownloadTc <- function(
	saveTo,
	var,
	year = NULL,
	update = TRUE,
	forceUpdate = FALSE,
	verbose = TRUE
) {

	# base download URLs
	fileService <- 'http://thredds.northwestknowledge.net:8080/thredds/fileServer/TERRACLIMATE_ALL/data/'
	metaService <- 'http://thredds.northwestknowledge.net:8080/thredds/catalog/TERRACLIMATE_ALL/data/catalog.html'


	if (is.null(year)) year <- NA

	standVar <- autConvertTcVar(var, standardToFile=FALSE)
	fileVar <- autConvertTcVar(var, standardToFile=TRUE)
	
	wantElev <- any(standVar == 'elev')
	wantMonthly <- any(standVar != 'elev')

	# elevation
	if (wantElev) {
	
		updatedElev <- .downloadTcElev(saveTo, update, forceUpdate, verbose)
		success <- data.frame(var='elev', year=NA, versionOnServer=NA, versionOnDisk=NA, updated=updatedElev)
		
	}
	
	if (wantMonthly) {

		# get date each file was last modified
		catalog <- xml2::read_html(metaService)
		catalog <- xml2::xml_find_all(catalog, './/table')
		catalog <- rvest::html_table(catalog)[[1]]
		names(catalog) <- c('file', 'size', 'lastModified')
		catalog <- catalog[-1, ]
		catalog$lastModified <- as.Date(catalog$lastModified)

		if (wantElev) {
			monthlyVarFile <- fileVar[-which(fileVar %in% 'elevation')]
			monthlyVarStand <- standVar[-which(standVar %in% 'elev')]
		} else {
			monthlyVarFile <- fileVar
			monthlyVarStand <- standVar
		}
	
		thisSuccess <- expand.grid(var=monthlyVarStand, year=year, versionOnServer=NA, versionOnDisk=NA, updated=NA)
		
		success <- if (exists('success', inherits=FALSE)) {
			rbind(success, thisSuccess)
		} else {
			thisSuccess
		}
		
		for (countVar in seq_along(monthlyVarFile)) {

			thisVarFile <- monthlyVarFile[countVar]
			thisVarStand <- monthlyVarStand[countVar]

			for (thisYear in year) {
		
				if (verbose) cat(thisVarStand, thisYear); flush.console()

				# save to
				saveToAppended <- paste0(saveTo, '/', thisVarFile)
				dir.create(saveToAppended, showWarnings=FALSE, recursive=TRUE)
		
				# file name and URL
				infoFileName <- paste0('TerraClimate_', thisVarFile, '_', thisYear, '_info.txt')
				rastFileName <- paste0('TerraClimate_', thisVarFile, '_', thisYear, '.nc')
				
				url <- paste0(fileService, rastFileName)
				
				infoPath <- paste0(saveToAppended, '/', infoFileName)
				filePath <- paste0(saveToAppended, '/', rastFileName)
				versionOnDisk <- if (file.exists(infoPath) & file.exists(filePath)) {
					read.csv(infoPath)$versionOnDisk
				} else {
					NA
				}
				
				versionOnServer <- catalog$lastModified[catalog$file == rastFileName]
				
				doUpdate <- forceUpdate | (is.na(versionOnDisk) || (update & versionOnDisk < versionOnServer))
				
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

				if (downloaded) {
					if (verbose) cat(' | downloaded\n'); flush.console()
					info <- data.frame(versionOnDisk=versionOnServer)
					write.csv(info, infoPath, row.names=FALSE)
				}

				this <- which(success$var==thisVarStand & success$year == thisYear)
				success$versionOnServer[this] <- versionOnServer
				success$versionOnDisk[this] <- versionOnDisk
				success$updated[this] <- downloaded
				
			} # next year
				
		} # next variable 
		
	} # if wanting monthly variables

	success

}

.downloadTcElev <- function(saveTo, update, forceUpdate, verbose) {

	if (verbose) cat('elev'); flush.console()
	
	rastFileName <- 'metdata_elevationdata.nc'
	filePath <- paste0(saveTo, '/', rastFileName)
	alreadyHave <- file.exists(filePath)
	
	url <- 'https://climate.northwestknowledge.net/METDATA/data/metdata_elevationdata.nc'
	
	doUpdate <- (forceUpdate | !file.exists(filePath) | (file.exists(filePath) & update))
	
	if (verbose) {
		if (doUpdate) {
			cat(' | fetching')
		} else {
			cat(' | skipping')
		}
		flush.console()
	}

	downloaded <- FALSE
	if (doUpdate) {

		tryNumber <- 1
		
		while (tryNumber <= 10 & !downloaded) {
			
			downloaded <- TRUE
			tryCatch(
				utils::download.file(url, destfile=filePath, method='auto', quiet=TRUE),
				error=function(e) { downloaded <<- FALSE }
			)

			Sys.sleep(1)
			
			tryNumber <- tryNumber + 1
		}

	} # if new download or overwriting

	if (verbose) {
		if (downloaded) {
			cat(' | downloaded\n')
		} else {
			cat(' | not downloaded\n')
		}
		flush.console()
	}

	downloaded

}
