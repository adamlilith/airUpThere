#' @name wcDownload
#' @rdname wcDownload
#' @title Download WorldClim climate rasters

#' @description These functions download WorldClim climate rasters:
#' \itemize{
#'		\item \code{wcDownloadHist}: Historical (near-present) climate normals from WorldClim versions 1.4 and 2.1.
#'		\item \code{wcDownloadFut}: Future climate scenario rasters from WorldClim versions 1.4 and 2.1.
#'		\item \code{wdDownloadDecade}: Monthly averages for particular historical decades from WorldClim version 2.1.
#'		\item \code{ecDownloadElev}: Elevation from WorldClim version 2.1.
#'	}

#' @param saveTo Name of the base path to which to save the download. Subfolders will be created within this folder.
#' @param ver WorldClim Version. Either \code{1.4} or \code{2.1}. Note that some resolutions and/or combinations of variables and earth system models may not be available.
#' @param res Resolution of the rasters. Valid values include:
#' \itemize{
#'		\item WC 1.4 and 2.1 historical normals: \code{10} (10 arcmin), 5} (5 arcmin), 2.5} (2.5 arcmin), and/or  \code{30} (30 arcsec).
#'		\item WC 1.4 future scenario normals: \code{10} (10 arcmin), \code{5} (5 arcmin), \code{2.5} (2.5 arcmin), and/or \code{30} (30 arcsec).
#'		\item WC 2.1 future scenario normals: \code{10} (10 arcmin), \code{5} (5 arcmin), and/or \code{2.5} (2.5 arcmin).
#'		\item WC 2.1 decadal monthly averages: \code{2.5} (2.5 arcmin).
#'		\item WC 2.1 elevation: \code{10} (10 arcmin), \code{5} (5 arcmin), \code{2.5} (2.5 arcmin), and/or  \code{30} (30 arcsec).
#' }
#' @param vars Name(s) of variable(s) to download. Different versions and time periods of WorldClim use different names for the same variable (e.g., "prec" versus "ppt" versus "pr" for precipitation). To reduce confusion, variable names have been standardized (in this package) to be the same across versions and times. Valid values are:
#' \itemize{
#' 	\item \code{'tmin'}: minimum temperature (available for WC 1.4 and 2.1 historical and future, and WC 2.1 decadal)
#' 	\item \code{'tmax'}: maximum temperature (available for WC 1.4 and 2.1 historical and future, and WC 2.1 decadal)
#' 	\item \code{'tmean'}: mean temperature (available for WC 1.4 and 2.1 historical)
#' 	\item \code{'ppt'}: precipitation (available for WC 1.4 and 2.1 historical and future, and WC 2.1 decadal)
#' 	\item \code{'bio'}: BIOCLIM variables (available for WC 1.4 and 2.1 historical and future)
#' 	\item \code{'srad'}: solar radiation (available for WC 2.1 historical)
#' 	\item \code{'wind'}: average wind speed (available for WC 2.1 historical)
#' 	\item \code{'vap'}: vapor pressure deficit (available for WC 2.1 historical)
#' 	\item \code{'elev'}: elevation (available for WC 2.1)
#' }
#' @param esm Name(s) of one or more earth system models (global circulation models) for downloading future rasters. You can get the available names from \code{data(wcEsm)}. Valid values depend on whether WC version 1.4 or 2.1 is being called:
#' \itemize{
#'		\item WorldClim 1.4 (CMIP5):
#'		\itemize{
#'			\item \code{'ACCESS1-0'} or \code{'AC'}
#'			\item \code{'BCC-CSM1-1'} or \code{'BC'}
#'			\item \code{'CCSM4'} or \code{'CC'}
#'			\item \code{'CESM1-CAM5-1-FV2'} or \code{'CE'}
#'			\item \code{'CNRM-CM5'} or \code{'CN'}
#'			\item \code{'GFDL-CM3'} or \code{'GF'}
#'			\item \code{'GFDL-ESM2G'} or \code{'GD'}
#'			\item \code{'GISS-E2-R'} or \code{'GS'}
#'			\item \code{'HadGEM2-AO'} or \code{'HD'}
#'			\item \code{'HadGEM2-CC'} or \code{'HG'}
#'			\item \code{'HadGEM2-ES'} or \code{'HE'}
#'			\item \code{'INMCM4'} or \code{'IN'}
#'			\item \code{'IPSL-CM5A-LR'} or \code{'IP'}
#'			\item \code{'MIROC-ESM-CHEM'} or \code{'MI'}
#'			\item \code{'MIROC-ESM'} or \code{'MR'}
#'			\item \code{'MIROC5'} or \code{'MC'}
#'			\item \code{'MPI-ESM-LR'} or \code{'MP'}
#'			\item \code{'MRI-CGCM3'} or \code{'MG'}
#'			\item \code{'NorESM1-M'} or \code{'NO'}
#'		}
#' 		\item WorldClim 2.1 (CMIP6):
#'		\itemize{
#'			\item \code{'BCC-CSM2-MR'} or \code{'BC'}
#'			\item \code{'CNRM-CM6-1'} or \code{'CC'}
#'			\item \code{'CNRM-ESM2-1'} or \code{'CE'}
#'			\item \code{'CanESM5'} or \code{'CA'}
#'			\item \code{'GFDL-ESM4'} or \code{'GF'}
#'			\item \code{'IPSL-CM6A-LR'} or \code{'IP'}
#'			\item \code{'MIROC-ES2L'} or \code{'MR'}
#'			\item \code{'MIROC6'} or \code{'MC'}
#'			\item \code{'MRI-ESM2-0'} or \code{'ME'}
#'		}
#' }
#' @param ghg Greenhouse gas emissions scenario for future rasters. Valid values depend on the version of WorldClim. One or more of a valid set can be specified. This argument is ignored if near present-day rasters are being downloaded.
#' \itemize{
#' 		\item WorldClim 1.4 (CMIP5): These are representative concentration pathways (RCPs), and valid values are are one or more of 26, 45, 60, and/or 85.
#'		\item WorldClim 2.1 (CMIP6): These are shared socioeconomic pathways (SSPs), and valid values are one or more of 126, 245, 370, and/or 585.
#' }
#' @param period Year(s) of the time period from which to download climate rasters. Valid values depend on the version of WorldClim and whether future or historical decadal averages are being downloaded:
#' \itemize{
#'		\item WC 1.4 future scenario normals:
#'			\itemize{
#'				\item \code{2050} (average across 2041-2060)
#'				\item \code{2070} (average across 2061-2080)
#'			}
#'		\item WC 2.1 future scenario normals:
#' 			\itemize{
#'				\item \code{2030} (average across 2021-2040)
#'				\item \code{2050} (average across 2041-2060)
#'				\item \code{2070} (average across 2061-2080)
#'				\item \code{2090} (average across 2081-2100)
#'			}
#'		\item WC 2.1 decadal monthly averages:
#'			\itemize{
#'				\item \code{1960} (average across 1960-1969)
#'				\item \code{1970} (average across 1970-1979)
#'				\item \code{1980} (average across 1980-1989)
#'				\item \code{1990} (average across 1990-1999)
#'				\item \code{2000} (average across 2000-2000)
#'				\item \code{2010} (average across 2010-2018)
#'			}
#' }
#' @param overwrite If \code{FALSE} (default), do not overwrite existing rasters.
#' @param verbose If \code{TRUE} (default), display progress.
#'
#' @return One or more zipped raster sets are saved to disk. The function also returns a data frame indicating if the desired file(s) were already on the disk and if they were downloaded.
#' @references
#' Fick, S.E. and Hijmans, R.J. 2017. WorldClim 2: New 1-km spatial resolution climate surfaces for global land areas. \emph{International Journal of Climatology} 37:4302-4315. doi: \href{https://doi.org/10.1002/joc.5086}{10.1002/joc.5086} \cr
#' Hijmans, R.J., Cameron, S.E., Parra, J.L., Jones, P.G., and Jarvis, A. 2005. Very high resolution interpolated climate surfaces for global land areas. \emph{International Journal of Climatology} 25:1965-1978. doi: \href{https://doi.org/10.1002/joc.1276}{10.1002/joc.1276}.
#' @examples
#' 
#' \dontrun{
dl <- 'C:/ecology/!Scratch/wc'

# historical (near-present)
wcDownloadHist(dl, ver=1.4, res=10, vars=c('tmin', 'tmax'))
wcDownloadHist(dl, ver=2.1, res=10, vars=c('tmin', 'tmax'))

# future: using BCC-CSM1-1 and BCC-CSM2-MR ESMs for RCP 6.0 and SSP 370
wcDownloadFut(dl, ver=1.4, res=10, vars='tmin', esm='BC', ghg=60, period=2050)
wcDownloadFut(dl, ver=2.1, res=10, vars='tmin', esm='BC', ghg=370, period=2050)

# historical decadal: for 1960-1969
wcDownloadDecadal(dl, vars='tmin', period=1960)

# elevation
wcDownloadElev(dl, res=10)

#' 
#' }
#' @export


#' @describeIn wcDownload Download WorldClim historical normal rasters
#' @export
wcDownloadHist <- function(
	saveTo,
	ver,
	res,
	vars,
	overwrite = FALSE,
	verbose = TRUE
) {

	### hard-coded URLs
	wc1_4HistUrl <- 'http://biogeo.ucdavis.edu/data/climate/worldclim/1_4/grid/cur/'
	wc2_1HistUrl <- 'http://biogeo.ucdavis.edu/data/worldclim/v2.1/base/'

	ok <- wcCheckVer_internal(ver)

	success <- expand.grid(ver=ver, res=res, vars=vars, alreadyHave=NA, downloaded=NA)

	for (thisRes in res) {

		resUnit <- wcGetRes(ver, res, period='historical')
		saveToAppended <- paste0(saveTo, '/', resUnit, '/historical')
		dir.create(saveToAppended, showWarnings=FALSE, recursive=TRUE)

		for (thisVar in vars) {

			if (verbose) cat('WC historical | ver', ver, '| res', thisRes, '| vars', thisVar); flush.console()

			# fileVar <- wcConvertVar(ver=ver, period='historical', vars=thisVar, standardToFile=TRUE)
			fileVar <- convertVar(src='wc', vars=thisVar, ver=ver, period='historical', standardToFile=TRUE)

			if (ver == 1.4) {

				fileName <- paste0(fileVar, '_', resUnit, '_bil.zip')
				url <- paste0(wc1_4HistUrl, fileName)
				
			} else if (ver == 2.1) {
			
				fileName <- paste0('wc2.1_', resUnit, '_', fileVar, '.zip')
				url <- paste0(wc2_1HistUrl, fileName)
			
			}

			filePath <- paste0(saveToAppended, '/', fileName)
			alreadyHave <- file.exists(filePath)
			
			if (verbose) {
				if (alreadyHave) {
					cat(paste0(' | file already on disk', ifelse(overwrite, ': overwriting', ': skipping')))
				} else if (verbose) {
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

				if (nrow(success) > 1) Sys.sleep(1)
				
			} # if new download or overwriting

			if (verbose) {
				if (downloaded) {
					cat(' | downloaded\n')
				} else {
					cat(' | not downloaded\n')
				}
				flush.console()
			}

			success$alreadyHave[success$res==thisRes & success$vars==thisVar] <- alreadyHave
			success$downloaded[success$res==thisRes & success$vars==thisVar] <- downloaded

		} # next variable
		
	} # next resolution

	success

}

#' @describeIn wcDownload Download WorldClim future normal rasters
#' @export
wcDownloadFut <- function(
	saveTo,
	ver,
	res,
	vars,
	esm,
	ghg,
	period,
	overwrite = FALSE,
	verbose = TRUE
) {

	### hard-coded URLs
	wc1_4FutUrl <- 'http://biogeo.ucdavis.edu/data/climate/cmip5/'
	wc2_1FutUrl <- 'http://biogeo.ucdavis.edu/data/worldclim/v2.1/fut/'

	ok <- wcCheckVer_internal(ver)

	success <- expand.grid(ver=ver, res=res, esm=esm, period=period, ghg=ghg, vars=vars, alreadyHave=NA, downloaded=NA)
		
	for (thisRes in res) {

		resUnit <- wcGetRes(ver, thisRes, period='future')

		for (thisPeriod in period) {
			
			periodCode <- wcGetPeriod(ver, thisPeriod, type='future')
					
			for (thisGhg in ghg) {

				ok <- wcCheckGhg_internal(ver, thisGhg)
				ghgNice <- wcNiceGhg(ver, thisGhg)
				
				saveToAppended <- paste0(saveTo, '/', resUnit, '/', thisPeriod, ' ', ghgNice)
				dir.create(saveToAppended, showWarnings=FALSE, recursive=TRUE)
			
				for (thisVar in vars) {

					# fileVar <- wcConvertVar(ver=ver, period='future', vars=thisVar, standardToFile=TRUE)
					fileVar <- convertVar(src='wc', vars=thisVar, ver=ver, period='future', standardToFile=TRUE)
				
					for (thisEsm in esm) {

						if (verbose) cat('WC future | ver', ver, '| res', thisRes, '| period', thisPeriod, '| ghg', thisGhg, '| vars', thisVar, '| esm', thisEsm); flush.console()
						
						esmCode <- wcGetEsm_internal(ver, thisEsm)
						
						# URL
						if (ver == 1.4) {
							
							fileName <- paste0(esmCode, thisGhg, fileVar, periodCode, '.zip')
							url <- paste0(wc1_4FutUrl, resUnit, '/', fileName)
							filePath <- paste0(saveToAppended, '/', fileName)

						} else if (ver == 2.1) {
						
							fileName <- paste0('wc2.1_', resUnit, '_', fileVar, '_', esmCode, '_ssp', thisGhg, '_', periodCode, '.zip')
							url <- paste0(wc2_1FutUrl, resUnit, '/', fileName)
							filePath <- paste0(saveToAppended, '/', fileName)
							
						}

						# download
						alreadyHave <- file.exists(filePath)
						
						if (verbose) {
							if (alreadyHave) {
								cat(' | file already exists')
							} else {
								cat(' | file does not exist')
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
								
								tryNumber <- tryNumber + 1
							}

							if (nrow(success) > 1) Sys.sleep(1)
							
						} # if new download or overwriting

						if (verbose) {
							if (downloaded) {
								cat(' | downloaded\n')
							} else {
								cat(' | not downloaded\n')
							}
							flush.console()
						}

						success$alreadyHave[success$res==thisRes & success$esm==thisEsm & success$period==thisPeriod & success$ghg==thisGhg & success$vars==thisVar] <- alreadyHave
						success$downloaded[success$res==thisRes & success$esm==thisEsm & success$period==thisPeriod & success$ghg==thisGhg & success$vars==thisVar] <- downloaded
						
					} # next ESM
					
				} # next variable
				
			} # next SSP
		
			if (verbose & length(period) > 1 & thisPeriod == tail(period, 1)) cat('\n'); flush.console()
		
		} # next period
		
		if (verbose) if (length(res) > 1 & thisRes == tail(res, 1)) cat('\n'); flush.console()
		
	} # next resolution

	success

}


#' @describeIn wcDownload Download WorldClim historical decadal average rasters
#' @export
wcDownloadDecadal <- function(
	saveTo,
	vars,
	period,
	overwrite = FALSE,
	verbose = TRUE
) {

	### hard-coded URLs
	wc2_1DecadalUrl <- 'https://biogeo.ucdavis.edu/data/worldclim/v2.1/hist/' # wc2.1_2.5m_tmin_1960-1969.zip'
	res <- 2.5
	ver <- 2.1

	ok <- wcCheckVer_internal(ver)

	success <- expand.grid(ver=ver, res=res, vars=vars, period=period, alreadyHave=NA, downloaded=NA)


	resUnit <- wcGetRes(ver, res, period='decadal')
	saveToAppended <- paste0(saveTo, '/', resUnit, '/decadal')
	dir.create(saveToAppended, showWarnings=FALSE, recursive=TRUE)

	for (thisPeriod in period) {
	
		periodCode <- wcGetPeriod(ver, thisPeriod, type='decadal')

		for (thisVar in vars) {
		
			if (verbose) cat('WC decadal | vars', thisVar); flush.console()

			if (!(thisVar %in% c('tmin', 'tmax', 'ppt'))) {
				warning('This variable is not available in the historical decadal set. Skipping download.')
			} else {

				# fileVar <- wcConvertVar(ver=ver, period='historical', vars=thisVar, standardToFile=TRUE)
				fileVar <- convertVar(src='wc', vars=thisVar, ver=ver, period='historical', standardToFile=TRUE)

				# assuming version 2.1
				fileName <- paste0('wc2.1_', resUnit, '_', fileVar, '_', periodCode, '.zip')
				url <- paste0(wc2_1DecadalUrl, fileName)

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

						Sys.sleep(1)
						
						tryNumber <- tryNumber + 1
					}

					if (nrow(success) > 1) Sys.sleep(1)
					
				} # if new download or overwriting

				if (verbose) {
					if (downloaded) {
						cat(' | downloaded\n')
					} else {
						cat(' | not downloaded\n')
					}
					flush.console()
				}

				this <- which(success$vars==thisVar & success$thisPeriod==period)
				success$alreadyHave[this] <- alreadyHave
				success$downloaded[this] <- downloaded
				
			} # variable is available in the decadal set

		} # next variable
		
	} # next decade

	success

}

#' @describeIn wcDownload Download WorldClim elevation rasters
#' @export
wcDownloadElev <- function(
	saveTo,
	res,
	overwrite = FALSE
	verbose = TRUE
) {

	### hard-coded URLs
	wc2_1ElevUrl <- 'https://biogeo.ucdavis.edu/data/worldclim/v2.1/base/'
	vars <- 'elev'
	ver <- 2.1

	success <- expand.grid(ver=ver, res=res, vars=vars, alreadyHave=NA, downloaded=NA)

	for (thisRes in res) {
	
		if (verbose) cat('WC elevation | res', thisRes, '| vars ', vars); flush.console()

		resUnit <- wcGetRes(ver=ver, res, period='historical')
		saveToAppended <- paste0(saveTo, '/', resUnit)
		dir.create(saveToAppended, showWarnings=FALSE, recursive=TRUE)

		# fileVar <- wcConvertVar(ver=ver, period='historical', vars=thisVar, standardToFile=TRUE)
		fileVar <- convertVar(src='wc', vars='elev', ver=ver, period='historical', standardToFile=TRUE)

		fileName <- paste0('wc2.1_', resUnit, '_', fileVar, '.zip')
		url <- paste0(wc2_1ElevUrl, fileName)

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

				Sys.sleep(1)
				
				tryNumber <- tryNumber + 1
			}

			if (nrow(success) > 1) Sys.sleep(1)
			
		} # if new download or overwriting

		if (verbose) {
			if (downloaded) {
				cat(' | downloaded\n')
			} else {
				cat(' | not downloaded\n')
			}
			flush.console()
		}

		success$alreadyHave[success$res==thisRes & success$vars==thisVar] <- alreadyHave
		success$downloaded[success$res==thisRes & success$vars==thisVar] <- downloaded
		
	} # next resolution
	
	success

}
