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
#' @param overwrite If \code{FALSE} (default), do not overwrite existing rasters.
#' @param verbose If \coed{TRUE} (default), display progress.
#' @return NetCDF rasters are saved to disk. The function also returns a data frame indicating if the desired file(s) were already on the disk and if they were downloaded.
#' @references
#' Abatzoglou, J.T., Dobrowski, S.Z., Parks, S.A., and Hegewisch, K.C. 2018. TerraClimate, a high-resolution global dataset of monthly climate and climatic water balance from 1958-2015. \emph{Scientific Data} 5:170191. doi: \href{https://dx.doi.org/10.1038/sdata.2017.191}{10.1038/sdata.2017.191}.
#' @examples
#' 
#' \dontrun{
#' dl <- 'C:/ecology/!Scratch/tc' 
#' tcDownload(dl, 'tmin', 1958)
#' tcDownload(dl, 'elev')
#' 
#' }
#' @export

tcDownload <- function(
	saveTo,
	var,
	year = NULL,
	overwrite = FALSE,
	verbose = TRUE
) {

	# base download URL
	service <- 'http://thredds.northwestknowledge.net:8080/thredds/fileServer/TERRACLIMATE_ALL/data/'

	if (is.null(year)) year <- NA

	standVar <- tcConvertVar(var, standardToFile=FALSE)
	fileVar <- tcConvertVar(var, standardToFile=TRUE)
	
	wantElev <- any(standVar == 'elev')
	wantMonthly <- any(standVar != 'elev')

	# elevation
	if (wantElev) {
	
		thisSuccess <- .tcDownloadElev(saveTo, overwrite, verbose)
		success <- data.frame(var='elev', year=NA, alreadyHave=thisSuccess[['alreadyHave']], downloaded=thisSuccess[['downloaded']])
		
	}
	
	if (wantMonthly) {
	
		if (wantElev) {
			monthlyVarFile <- fileVar[-which(fileVar %in% 'elevation')]
			monthlyVarStand <- standVar[-which(standVar %in% 'elev')]
		} else {
			monthlyVarFile <- fileVar
			monthlyVarStand <- standVar
		}
	
		thisSuccess <- expand.grid(monthlyVarStand, year, alreadyHave=NA, downloaded=NA)
		names(thisSuccess)[1:2] <- c('var', 'year')
		
		success <- if (exists('success', inherits=FALSE)) {
			rbind(success, thisSuccess)
		} else {
			thisSuccess
		}
		
		for (thisVar in monthlyVarFile) {

			for (thisYear in year) {
		
				if (verbose) cat(thisVar, thisYear); flush.console()

				# save to
				saveToAppended <- paste0(saveTo, '/', thisVar, '/', thisYear)
				dir.create(saveToAppended, showWarnings=FALSE, recursive=TRUE)
		
				# file name and URL
				fileName <- paste0('TerraClimate_', thisVar, '_', thisYear, '.nc')
				
				url <- paste0(service, fileName)
				
				filePath <- paste0(saveToAppended, '/', fileName)
				alreadyHave <- file.exists(filePath)
				
				if (verbose) {
					if (alreadyHave) {
						cat(paste0(' | file already on disk', ifelse(overwrite, ': overwriting', ': skipping')))
					} else {
						cat(' | file not on disk: downloading')
					}
					flush.console()
				}

				downloaded <- FALSE
				if (!alreadyHave | overwrite) {

					tryNumber <- 1
					
					while (tryNumber <= 10 & !downloaded) {
						
						downloaded <- TRUE
						tryCatch(
							utils::download.file(url, destfile=filePath, mode='wb', quiet=TRUE),
							error=function(e) { downloaded <<- FALSE }
						)

						if (nrow(success) > 1) Sys.sleep(1)
						
						tryNumber <- tryNumber + 1
					}

					Sys.sleep(1)
					
				} # if new download or overwriting

				if (verbose) {
					if (downloaded) {
						cat(' | downloaded\n')
					} else {
						cat(' | not downloaded\n')
					}
					flush.console()
				}

				success$alreadyHave[success$var==thisVar & success$year == thisYear] <- alreadyHave
				success$downloaded[success$var==thisVar & success$year == thisYear] <- downloaded
				
			} # next year
				
		} # next variable 
		
	} # if wanting monthly variables

	success

}

.tcDownloadElev <- function(saveTo, overwrite, verbose) {

	if (verbose) cat('elev'); flush.console()
	
	fileName <- 'metdata_elevationdata.nc'
	filePath <- paste0(saveTo, '/', fileName)
	alreadyHave <- file.exists(filePath)
	
	url <- 'https://climate.northwestknowledge.net/METDATA/data/metdata_elevationdata.nc'
	
	if (verbose) {
		if (alreadyHave) {
			cat(paste0(' | file already on disk', ifelse(overwrite, ': overwriting', ': skipping')))
		} else {
			cat(' | file not on disk: downloading')
		}
		flush.console()
	}

	downloaded <- FALSE
	if (!alreadyHave | overwrite) {

		tryNumber <- 1
		
		while (tryNumber <= 10 & !downloaded) {
			
			downloaded <- TRUE
			tryCatch(
				utils::download.file(url, destfile=filePath, mode='wb', quiet=TRUE),
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

	c(alreadyHave=alreadyHave, downloaded=downloaded)

}
