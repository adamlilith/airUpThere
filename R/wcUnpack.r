#' @name wcUnzip
#' @rdname wcUnpack
#' @title Unzip WorldClim climate rasters

#' @description These functions unzip WorldClim climate rasters:
#' \itemize{
#'		\item \code{wcUnpackHist}: Historical (near-present) climate normals from WorldClim versions 1.4 and 2.1.
#'		\item \code{wcUnpackFut}: Future climate scenario rasters from WorldClim versions 1.4 and 2.1.
#'		\item \code{wdDownloadDecade}: Monthly averages for particular historical decades from WorldClim version 2.1.
#'		\item \code{ecDownloadElev}: Elevation from WorldClim version 2.1.
#'	}

#' @param unpackFrom Name of the base path to which WC zipped raster files have been saved.
#' @param unpackTo Name of the base path to which to WC rasters will be unzipped. Subfolders will be created within this folder.
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
#' @param fail If \code{FALSE} (default), stop with an error if a zip file is missing. Otherwise, continue with a warning.
#'
#' @return One or more zipped raster sets are saved to disk. The function also returns a data frame indicating if the desired file(s) were already on the disk and if they were downloaded.
#' @references
#' Fick, S.E. and Hijmans, R.J. 2017. WorldClim 2: New 1-km spatial resolution climate surfaces for global land areas. \emph{International Journal of Climatology} 37:4302-4315. doi: \href{https://doi.org/10.1002/joc.5086}{10.1002/joc.5086} \cr
#' Hijmans, R.J., Cameron, S.E., Parra, J.L., Jones, P.G., and Jarvis, A. 2005. Very high resolution interpolated climate surfaces for global land areas. \emph{International Journal of Climatology} 25:1965-1978. doi: \href{https://doi.org/10.1002/joc.1276}{10.1002/joc.1276}.
#' @examples
#' 
#' \dontrun{
#' from1_4 <- 'C:/ecology/!Scratch/wc 1.4 zip'
#' to1_4 <- 'C:/ecology/!Scratch/wc 1.4 unzipped'
#' 
#' from2_1 <- 'C:/ecology/!Scratch/wc 2.1 zip'
#' to2_1 <- 'C:/ecology/!Scratch/wc 2.1 unzipped'
#' 
#' # historical (near-present)
#' wcUnpackHist(from1_4, to1_4, ver=1.4, res=10, vars=c('tmin', 'tmax'))
#' wcUnpackHist(from2_1, to2_1, ver=2.1, res=10, vars=c('tmin', 'tmax'))
#' 
#' # future: using BCC-CSM1-1 and BCC-CSM2-MR ESMs for RCP 6.0 and SSP 370
#' wcUnpackFut(from1_4, to1_4, ver=1.4, res=10, vars='tmin', esm='BC', ghg=60, period=2050)
#' wcUnpackFut(from2_1, to2_1, ver=2.1, res=10, vars='tmin', esm='BC', ghg=370, period=2050)
#' 
#' # historical decadal: for 1960-1969
#' wcUnpackDecadal(dl, vars='tmin', period=1960)
#' 
#' # elevation
#' wcUnpackElev(dl, res=10)
#' 
#' }
#' @export
NULL

#' @describeIn wcUnpack Download WorldClim historical normal rasters
#' @export
wcUnpackHist <- function(
	unpackFrom,
	unpackTo,
	ver,
	res,
	vars,
	overwrite = FALSE,
	verbose = TRUE,
	fail = FALSE
) {

	wcCheckVer_internal(ver)
	wcCheckVars_internal(ver=ver, vars=vars, period='historical')

	for (thisRes in res) {

		resFile <- wcConvertRes(ver=ver, res=thisRes, period='historical', standardToFile=TRUE)
		resWithUnit <- wcConvertRes(ver=ver, res=resFile, period='historical', standardToFile=FALSE)
		
		unpackFromAppended <- paste0(unpackFrom, '/worldclim_', ver, '_archive_', resWithUnit, '_historical')
		unpackToAppended <- paste0(unpackTo, '/worldclim_', ver, '_', resWithUnit, '_historical')
		dir.create(unpackToAppended, showWarnings=FALSE, recursive=TRUE)

		for (thisVar in vars) {

			if (verbose) cat('WC historical | ver', ver, '| res', thisRes, '| var', thisVar); flush.console()

			varFile <- convertVar(src='wc', vars=thisVar, ver=ver, period='historical', standardToFile=TRUE)

			# unpack from file path and name
			packedFileName <- getFileOrURL_internal(what='rastFilePattern', src='wc', ver=ver, period='historical', elevation=FALSE)
			packedFileName <- gsub(packedFileName, pattern='<<varFile>>', replacement=varFile)
			packedFileName <- gsub(packedFileName, pattern='<<resFile>>', replacement=resFile)

			unpackFromPathFile <- paste0(unpackFromAppended, '/', packedFileName)
			haveZipFile <- file.exists(unpackFromPathFile)
			
			haveUnzippedFiles <- successfulUnpack <- FALSE
			if (!haveZipFile & fail) {
				
				msg <- paste0('\nNo zip file for ', thisVar, ' at ', thisRes, ' resolution for WorldClim ', ver, ' exists.')
				stop(msg)
				
			} else if (!haveZipFile & !fail) {
		
				unzippedFileNames <- NA

			} else {
			
				unzippedFileNames <- unzip(unpackFromPathFile, list=TRUE)$Name
				haveUnzippedFiles <- any(file.exists(paste0(unpackToAppended, '/', unzippedFileNames)))
			
				unzip(unpackFromPathFile, exdir=unpackToAppended, junkpaths=TRUE, overwrite=overwrite)
				successfulUnpack <- all(file.exists(paste0(unpackToAppended, '/', unzippedFileNames)))
				
			}

			overwritten <- (overwrite & haveUnzippedFiles)

			thisSuccess <- data.frame(ver=ver, var=thisVar, res=thisRes, packedFileName=packedFileName, unpackedFile=unzippedFileNames, unpacked=successfulUnpack, overwritten=overwritten)

			success <- if (exists('success', inherits=FALSE)) {
				rbind(success, thisSuccess)
			} else {
				thisSuccess
			}
			
			if (verbose) {
				if (successfulUnpack) {
					cat(' | succesful\n')
				} else {
					cat(' | unsuccessful\n')
				}
				flush.console()
			}

		} # next variable
		
	} # next resolution

	success

}

#' @describeIn wcUnpack Download WorldClim future normal rasters
#' @export
wcUnpackFut <- function(
	unpackFrom,
	unpackTo,
	ver,
	res,
	vars,
	esm,
	ghg,
	period,
	overwrite = FALSE,
	verbose = TRUE,
	fail = FALSE
) {

	wcCheckVer_internal(ver)
	wcCheckVars_internal(ver=ver, vars=vars, period='future')
	
	for (thisRes in res) {

		resFile <- wcConvertRes(ver=ver, res=thisRes, period='future', standardToFile=TRUE)
		resWithUnit <- wcConvertRes(ver=ver, res=resFile, period='future', standardToFile=FALSE)

		for (thisPeriod in period) {
			
			periodFile <- wcConvertPeriod(ver, thisPeriod, type='future')
					
			for (thisGhg in ghg) {

				wcCheckGhg_internal(ver, thisGhg)
				ghgNice <- wcConvertGhg(ver, thisGhg)
				
				unpackFromAppended <- paste0(unpackFrom, '/worldclim_', ver, '_archive_', resWithUnit, '_', ghgNice, '_', periodFile)
				unpackToAppended <- paste0(unpackTo, '/worldclim_', ver, '_', resWithUnit, '_', ghgNice, '_', periodFile)
				dir.create(unpackToAppended, showWarnings=FALSE, recursive=TRUE)
			
				for (thisVar in vars) {

					fileVar <- convertVar(src='wc', vars=thisVar, ver=ver, period='future', standardToFile=TRUE)
				
					for (thisEsm in esm) {

						if (verbose) cat('WC future | ver', ver, '| res', thisRes, '| period', thisPeriod, '| ghg', thisGhg, '| vars', thisVar, '| esm', thisEsm); flush.console()
						
						esmCode <- wcConvertEsm_internal(ver, thisEsm)
						
						# unpack from file path and name
						unpackFromFile <- if (ver == 1.4) {
							paste0(esmCode, thisGhg, fileVar, periodFile, '.zip')
						} else if (ver == 2.1) {
							paste0('wc2.1_', resFile, '_', fileVar, '_', esmCode, '_ssp', thisGhg, '_', periodFile, '.zip')
						}

						unpackFromPathFile <- paste0(unpackFromAppended, '/', unpackFromFile)
						haveZipFile <- file.exists(unpackFromPathFile)
						
						haveUnzippedFiles <- successfulUnpack <- FALSE
						if (!haveZipFile & fail) {
							
							msg <- paste0('\nNo zip file for ', thisEsm, ' for ', thisGhg, ' for ', thisPeriod, ' for ', thisVar, ' at ', thisRes, ' resolution for WorldClim ', ver, ' exists.')
							stop(msg)
							
						} else if (!haveZipFile & !fail) {
					
							unzippedFileNames <- NA

						} else {
						
							unzippedFileNames <- basename(unzip(unpackFromPathFile, list=TRUE)$Name)
							haveUnzippedFiles <- any(file.exists(paste0(unpackToAppended, '/', unzippedFileNames)))
							unzip(unpackFromPathFile, exdir=unpackToAppended, junkpaths=TRUE, overwrite=overwrite)
							
							successfulUnpack <- all(file.exists(paste0(unpackToAppended, '/', unzippedFileNames)))
							
						}

						overwritten <- (overwrite & haveUnzippedFiles)

						thisSuccess <- data.frame(ver=ver, var=thisVar, res=thisRes, esm=thisEsm, ghg=thisGhg, period=thisPeriod, file=unzippedFileNames, havePacked=haveZipFile, unpacked=successfulUnpack, overwritten=overwritten)

						success <- if (exists('success', inherits=FALSE)) {
							rbind(success, thisSuccess)
						} else {
							thisSuccess
						}
						
						if (verbose) {
							if (successfulUnpack) {
								cat(' | succesful\n')
							} else {
								cat(' | unsuccessful\n')
							}
							flush.console()
						}
						
					} # next ESM
					
				} # next variable
				
			} # next SSP
		
			if (verbose & length(period) > 1 & thisPeriod == tail(period, 1)) cat('\n'); flush.console()
		
		} # next period
		
		if (verbose) if (length(res) > 1 & thisRes == tail(res, 1)) cat('\n'); flush.console()
		
	} # next resolution

	success

}


#' @describeIn wcUnpack Download WorldClim historical decadal average rasters
#' @export
wcUnpackDecadal <- function(
	unpackFrom,
	unpackTo,
	vars,
	period,
	overwrite = FALSE,
	verbose = TRUE,
	fail = FALSE
) {

	# hard-coded
	thisRes <- 2.5
	
	wcCheckVer_internal(ver)
	wcCheckVars_internal(ver=ver, vars=vars, period='decadal')

	resFile <- wcConvertRes(ver=ver, res=thisRes, period='decadal', standardToFile=TRUE)
	resWithUnit <- wcConvertRes(ver=ver, res=resFile, period='decadal', standardToFile=FALSE)

	unpackFromAppended <- paste0(unpackFrom, '/', resWithUnit, '/decadal')
	unpackToAppended <- paste0(unpackTo, '/', resWithUnit, '/decadal')
	dir.create(unpackToAppended, showWarnings=FALSE, recursive=TRUE)

	for (thisPeriod in period) {
	
		periodFile <- wcConvertPeriod(ver, thisPeriod, type='decadal')

		for (thisVar in vars) {
		
			if (verbose) cat('WC decadal | vars', thisVar); flush.console()

			if (!(thisVar %in% c('tmin', 'tmax', 'ppt'))) {
				warning('\nThis variable is not available in the historical decadal set. Skipping.')
			} else {

				# fileVar <- wcConvertVar(ver=ver, period='historical', vars=thisVar, standardToFile=TRUE)
				fileVar <- convertVar(src='wc', vars=thisVar, ver=ver, period='historical', standardToFile=TRUE)

				# assuming version 2.1
				fileName <- paste0('wc2.1_', resFile, '_', fileVar, '_', periodFile, '.zip')

				unpackFromPathFile <- paste0(unpackFromAppended, '/', fileName)
				haveZipFile <- file.exists(unpackFromPathFile)
				
				if (verbose) {
					if (haveZipFile) {
						cat(paste0(' | file already unpacked', ifelse(overwrite, ': overwriting', ': skipping')))
					} else {
						cat(' | unpacking')
					}
					flush.console()
				}

				successfulUnpack <- haveUnzippedFiles <- FALSE
				if (!haveZipFile & fail) {
					
					msg <- paste0('\nNo zip file for ', thisVar, ' for ', thisPeriod, ' exists.')
					stop(msg)
					
				} else if (!haveZipFile & !fail) {
					
					unzippedFileNames <- NA
					
				} else {
				
					unzippedFileNames <- unzip(unpackFromPathFile, list=TRUE)$Name
					haveUnzippedFiles <- any(file.exists(paste0(unpackToAppended, '/', unzippedFileNames)))
				
					unzip(unpackFromPathFile, exdir=unpackToAppended, junkpaths=TRUE, overwrite=overwrite)
					
					successfulUnpack <- all(file.exists(paste0(unpackToAppended, '/', unzippedFileNames)))
					
				}

				overwritten <- (overwrite & haveUnzippedFiles)

				thisSuccess <- data.frame(ver=ver, var=thisVar, period=thisPeriod, file=unzippedFileNames, havePacked=haveZipFile, unpacked=successfulUnpack, overwritten=overwritten)

				success <- if (exists('success', inherits=FALSE)) {
					rbind(success, thisSuccess)
				} else {
					thisSuccess
				}

				if (verbose) {
					if (successfulUnpack) {
						cat(' | successful\n')
					} else {
						cat(' | unsuccessful\n')
					}
					flush.console()
				}

				this <- which(success$var==thisVar & success$thisPeriod==period)
				success$alreadyHave[this] <- alreadyHave
				success$downloaded[this] <- downloaded
				
			} # variable is available in the decadal set

		} # next variable
		
	} # next decade

	success

}

#' @describeIn wcUnpack Download WorldClim elevation rasters
#' @export
wcUnpackElev <- function(
	unpackFrom,
	unpackTo,
	res,
	overwrite = FALSE,
	verbose = TRUE,
	fail = FALSE
) {

	### hard-coded
	vars <- thisVar <- 'elev'
	ver <- 2.1

	for (thisRes in res) {
	
		if (verbose) cat('WC elevation | res', thisRes, '| vars ', vars); flush.console()

		resFile <- wcConvertRes(ver=ver, res=thisRes, period='historical', standardToFile=TRUE)
		resWithUnit <- wcConvertRes(ver=ver, res=resFile, period='historical', standardToFile=FALSE)

		unpackFromAppended <- paste0(unpackFrom, '/', resWithUnit)
		unpackToAppended <- paste0(unpackTo, '/', resWithUnit)
		dir.create(unpackToAppended, showWarnings=FALSE, recursive=TRUE)

		fileVar <- convertVar(src='wc', vars=thisVar, ver=ver, period='historical', standardToFile=TRUE)

		fileName <- paste0('wc2.1_', resFile, '_', fileVar, '.zip')

				unpackFromPathFile <- paste0(unpackFromAppended, '/', fileName)
				haveZipFile <- file.exists(unpackFromPathFile)
				
				if (verbose) {
					if (haveZipFile) {
						cat(paste0(' | file already unpacked', ifelse(overwrite, ': overwriting', ': skipping')))
					} else {
						cat(' | unpacking')
					}
					flush.console()
				}

				successfulUnpack <- haveUnzippedFiles <- FALSE
				if (!haveZipFile & fail) {
					
					msg <- paste0('\nNo zip file for elevation for ', thisRes, ' resolution for WorldClim 2.1 exists.')
					stop(msg)
					
				} else if (!haveZipFile & !fail) {
					
					unzippedFileNames <- NA
					
				} else {
				
					unzippedFileNames <- unzip(unpackFromPathFile, list=TRUE)$Name
					haveUnzippedFiles <- any(file.exists(paste0(unpackToAppended, '/', unzippedFileNames)))
				
					unzip(unpackFromPathFile, exdir=unpackToAppended, junkpaths=TRUE, overwrite=overwrite)
					
					successfulUnpack <- all(file.exists(paste0(unpackToAppended, '/', unzippedFileNames)))
					
				}

				overwritten <- (overwrite & haveUnzippedFiles)

				thisSuccess <- data.frame(ver=ver, var=thisVar, file=unzippedFileNames, havePacked=haveZipFile, unpacked=successfulUnpack, overwritten=overwritten)

				success <- if (exists('success', inherits=FALSE)) {
					rbind(success, thisSuccess)
				} else {
					thisSuccess
				}

				if (verbose) {
					if (successfulUnpack) {
						cat(' | successful\n')
					} else {
						cat(' | unsuccessful\n')
					}
					flush.console()
				}
		
	} # next resolution
	
	success

}
