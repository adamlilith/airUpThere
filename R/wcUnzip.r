#' Unzip downloaded WorldClim rasters.
#'
#' This function unzips downloaded WorldClim climate rasters. The rasters are assumed to be in a pre-defined folder structure (created by, for example, \code{\link[airUpThere]{wcDownload}}), and they will be extracted to a new folder structure. It may duplicate some functionality of the \code{getData} function in the \pkg{raster} package. Users have the option of renaming rasters so they have "standard" names (otherwise, historical and future rasters for the same WorldClim version will have different names).
#'
#' @param ver WorldClim Version. Either \code{1.4} or \code{2.1}. If future rasters are desired, then using 1.4 fetches rasters from CMIP5, and using 2.1 fetches rasters from CMIP5. Note that some resolutions and/or combinations of variables and earth system models may not be available.
#' @param res Resolution(s). One or more of 10 (10 arcminutes), 5 (5 arcminutes), 2.5 (2.5 arcminutes), and/or 30 (30 arcseconds).
#' @param var Name(s) of variable(s) to download. Valid values depend on the version of WorldClim and whether near-present day or future rasters are fetched. One or more value of each valid set can be specified.
#' \itemize{
#' 		\item WorldClim 1.4, near-present day: \code{tmin}, \code{tmax}, \code{tmean}, \code{prec}, and/or \code{bio}.
#' 		\item WorldClim 1.4, future: \code{tn}, \code{tx}, \code{pr}, and/or \code{bi}.
#' 		\item WorldClim 2.1, near-present day: \code{tmin}, \code{tmax}, \code{tmean}, \code{prec}, \code{srad}, \code{wind}, \code{vapr}, \code{bio}, and/or \code{elev}.
#' 		\item WorldClim 2.1, future: \code{tmin}, \code{tmax}, \code{prec}, and/or \code{bioc}.
#' }
#' @param esm Either \code{NULL} (default: download near-present day rasters) or the name(s) of one or more earth system models (global circulation models) for downloading future rasters. You can get the available names from \code{data(wcEsm)}.
#' @param ghg Either Greenhouse gas emissions scenario. Valid values depend on the version of WorldClim. One or more of a valid set can be specified. This argument is ignored if near present-day rasters are being downloaded.
#' \itemize{
#' 	\item WorldClim 1.4 (CMIP5): These are representative concentration pathways (RCPs), and valid values are are one or more of 26, 45, 60, and/or 85.
#'	\item WorldClim 2.1 (CMIP6): These are shared socioeconomic pathways (SSPs), and valid values are one or more of 126, 245, 370, and/or 585.
#' }
#' @param year Year(s) of the period from which to download future climate rasters. For WorldClim 1.4 (CMIP5) valid values are 2050 and 2070. For WorldClim 2+ (CMIP6) valid values are one or more of 2030, 2050, 2070, and/or 2090. This is the "middle" year of the period. For example, using "2030" will obtain climate rasters from the period 2021-2040. This is ignored if near present-day rasters are being downloaded.
#' @param overwrite If \code{FALSE} (default), do not overwrite existing rasters.
#' @param saveTo Name of the base path to which to save the download. Subfolders will be created within this folder.
#' @param return One or more zipped raster sets are saved to disk. The function also returns a data frame indicating if the desired file(s) were already on the disk and if they were downloaded.
#' @references
#' Fick, S.E. and Hijmans, R.J. 2017. WorldClim 2: New 1-km spatial resolution climate surfaces for global land areas. \emph{International Journal of Climatology} 37:4302-4315. doi: \href{https://doi.org/10.1002/joc.5086}{10.1002/joc.5086} /cr
#' Hijmans, R.J., Cameron, S.E., Parra, J.L., Jones, P.G., and Jarvis, A. 2005. Very high resolution interpolated climate surfaces for global land areas. \emph{International Journal of Climatology} 25:1965-1978. doi: \href{https://doi.org/10.1002/joc.1276}{10.1002/joc.1276}.
#' @examples
#' 
#' \dontrun{
#' wcDownload(1.4, 10, 'tmin')
#' wcDownload(2.1, 10, 'tmin')
#'
#' wcDownload(1.4, 10, 'tn', esm='AC', ghg=85, year=2070)
#' wcDownload(2.1, 10, 'tmin', esm='BCC-CSM2-MR', ghg=585, year=2070)
#' 
#' }
#' @export

wcUnzip <- function(
	zipFrom,
	unzipTo,
	ver,
	res,
	var,
	esm = NULL,
	ghg = NULL,
	year = NULL,
	overwrite = FALSE
) {

	ok <- wcCheckVer(ver)
	historical <- is.null(esm)

	### historical
	if (historical) {
	
		success <- expand.grid(ver, res, var, alreadyHave=NA, downloaded=NA)
		names(success)[1:3] <- c('ver', 'res', 'var')
	
		for (thisRes in res) {
	
			resUnit <- wcGetRes(ver, res, time='historical')
			saveToAppended <- paste0(unzipTo, '/', resUnit, '/historical')
			dir.create(saveToAppended, showWarnings=FALSE, recursive=TRUE)
	
			for (thisVar in var) {

				cat('ver', ver, '| res', thisRes, '| var', thisVar)
				flush.console()

				fileVar <- wcConvertVar(ver=ver, time='historical', var=thisVar, standardToFile=TRUE)
				stdVar <- wcConvertVar(ver=ver, time='historical', var=thisVar, standardToFile=FALSE)

				fileName <- if (ver == 1.4) {
					paste0(fileVar, '_', resUnit, '_bil.zip')
				} else if (ver == 2.1) {
					fileName <- paste0('wc2.1_', resUnit, '_', fileVar, '.zip')
				}
!!!
				filePath <- paste0(saveToAppended, '/', fileName)
				alreadyHave <- file.exists(filePath)
				
				if (alreadyHave) {
					cat(paste0(' | file already on disk', ifelse(overwrite, ': overwriting', ': skipping')))
				} else {
					cat(' | file not on disk: downloading')
				}
				flush.console()

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

					if (nrow(success) > 1) Sys.sleep(1)
					
				} # if new download or overwriting

				if (downloaded) {
					cat(' | downloaded\n')
				} else {
					cat(' | not downloaded\n')
				}
				flush.console()

				success$alreadyHave[success$res==thisRes & success$var==thisVar] <- alreadyHave
				success$downloaded[success$res==thisRes & success$var==thisVar] <- downloaded

			} # next variable
			
		} # next resolution
		
	######################
	### CMPI5 or CMIP6 ###
	######################
	
	} else if (ver %in% c(1.4, 2.1) & !historical) {

		success <- expand.grid(ver, res, esm, year, ghg, var, alreadyHave=NA, downloaded=NA)
		names(success)[1:6] <- c('ver', 'res', 'esm', 'year', 'ghg', 'var')
			
		for (thisRes in res) {

			resUnit <- wcGetRes(ver, thisRes, time='future')

			for (thisYear in year) {
				
				yearCode <- wcGetYear(ver, thisYear)
						
				for (thisGhg in ghg) {

					ok <- wcCheckGhg(ver, thisGhg)
					ghgNice <- wcNiceGhg(ver, thisGhg)
					
					saveToAppended <- paste0(saveTo, '/', resUnit, '/', thisYear, ' ', ghgNice)
					dir.create(saveToAppended, showWarnings=FALSE, recursive=TRUE)
				
					for (thisVar in var) {

						fileVar <- wcConvertVar(ver=ver, time='future', var=thisVar, standardToFile=TRUE)
					
						for (thisEsm in esm) {

							cat('ver', ver, '| res', thisRes, '| year', thisYear, '| ghg', thisGhg, '| var', thisVar, '| esm', thisEsm)
							flush.console()
							
							esmCode <- wcGetEsm(ver, thisEsm)
							
							# URL
							if (ver == 1.4) {
								
								fileName <- paste0(esmCode, thisGhg, fileVar, yearCode, '.zip')
								url <- paste0('http://biogeo.ucdavis.edu/data/climate/cmip5/', resUnit, '/', fileName)
								filePath <- paste0(saveToAppended, '/', fileName)

							} else if (ver == 2.1) {
							
								fileName <- paste0('wc2.1_', resUnit, '_', fileVar, '_', esmCode, '_ssp', thisGhg, '_', yearCode, '.zip')
								url <- paste0('http://biogeo.ucdavis.edu/data/worldclim/v2.1/fut/', resUnit, '/', fileName)
								filePath <- paste0(saveToAppended, '/', fileName)
								
							}

							# download
							alreadyHave <- file.exists(filePath)
							
							if (alreadyHave) {
								cat(' | file already exists')
							} else {
								cat(' | file does not exist')
							}
							flush.console()
							
							downloaded <- FALSE
							if (!alreadyHave | overwrite) {

								tryNumber <- 1
								
								while (tryNumber <= 10 & !downloaded) {
									
									downloaded <- TRUE
									tryCatch(
										utils::download.file(url, destfile=filePath, mode='wb', quiet=TRUE),
										error=function(e) { downloaded <<- FALSE }
									)
									
									tryNumber <- tryNumber + 1
								}

								if (nrow(success) > 1) Sys.sleep(1)
								
							} # if new download or overwriting

							if (downloaded) {
								cat(' | downloaded\n')
							} else {
								cat(' | not downloaded\n')
							}
							flush.console()

							success$alreadyHave[success$res==thisRes & success$esm==thisEsm & success$year==thisYear & success$ghg==thisGhg & success$var==thisVar] <- alreadyHave
							success$downloaded[success$res==thisRes & success$esm==thisEsm & success$year==thisYear & success$ghg==thisGhg & success$var==thisVar] <- downloaded
							
						} # next ESM
						
					} # next variable
					
				} # next SSP
			
				if (length(year) > 1 & thisYear == tail(year, 1)) cat('\n'); flush.console()
			
			} # next year
			
			if (length(res) > 1 & thisRes == tail(res, 1)) cat('\n'); flush.console()
			
		} # next resolution

	} # if CMIP6

	success

}
