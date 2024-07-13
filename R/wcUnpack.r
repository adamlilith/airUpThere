#' Unzip WorldClim climate rasters
#'
#' @description These functions unzip WorldClim climate rasters:
#'	* `wcUnpackHist`: Historical (near-present) climate normals from WorldClim versions 1.4 and 2.1.
#'	* `wcUnpackFut`: Future climate scenario rasters from WorldClim versions 1.4 and 2.1.
#'	* `wdDownloadDecade`: Monthly averages for particular historical decades from WorldClim version 2.1.
#'	* `ecDownloadElev`: Elevation from WorldClim version 2.1.
#'
#' @param unpackFrom Name of the base path to which WC zipped raster files have been saved.
#' @param unpackTo Name of the base path to which to WC rasters will be unzipped. Subfolders will be created within this folder.
#' @param ver WorldClim Version. Either `1.4` or `2.1`. Note that some resolutions and/or combinations of variables and earth system models may not be available.
#' @param res Resolution of the rasters. Valid values include:
#'	* WC 1.4 and 2.1 historical normals: `10` (10 arcmin), 5` (5 arcmin), 2.5` (2.5 arcmin), and/or  `30` (30 arcsec).
#'	* WC 1.4 future scenario normals: `10` (10 arcmin), `5` (5 arcmin), `2.5` (2.5 arcmin), and/or `30` (30 arcsec).
#'	* WC 2.1 future scenario normals: `10` (10 arcmin), `5` (5 arcmin), and/or `2.5` (2.5 arcmin).
#'	* WC 2.1 decadal monthly averages: `2.5` (2.5 arcmin).
#'	* WC 2.1 elevation: `10` (10 arcmin), `5` (5 arcmin), `2.5` (2.5 arcmin), and/or  `30` (30 arcsec).
#'
#' @param vars Name(s) of variable(s) to download. Different versions and time periods of WorldClim use different names for the same variable (e.g., "prec" versus "ppt" versus "pr" for precipitation). To reduce confusion, variable names have been standardized (in this package) to be the same across versions and times. Valid values are:
#' * `'tmin'`: minimum temperature (available for WC 1.4 and 2.1 historical and future, and WC 2.1 decadal)
#' * `'tmax'`: maximum temperature (available for WC 1.4 and 2.1 historical and future, and WC 2.1 decadal)
#' * `'tmean'`: mean temperature (available for WC 1.4 and 2.1 historical)
#' * `'ppt'`: precipitation (available for WC 1.4 and 2.1 historical and future, and WC 2.1 decadal)
#' * `'bio'`: BIOCLIM variables (available for WC 1.4 and 2.1 historical and future)
#' * `'srad'`: solar radiation (available for WC 2.1 historical)
#' * `'wind'`: average wind speed (available for WC 2.1 historical)
#' * `'vap'`: vapor pressure deficit (available for WC 2.1 historical)
#' * `'elev'`: elevation (available for WC 2.1)
#' 
#' @param esm Name(s) of one or more earth system models (global circulation models) for downloading future rasters. You can get the available names from `data(wcEsm)`. Valid values depend on whether WC version 1.4 or 2.1 is being called:
#'	* WorldClim 1.4 (CMIP5):
#'		* `'ACCESS1-0'` or `'AC'`
#'		* `'BCC-CSM1-1'` or `'BC'`
#'		* `'CCSM4'` or `'CC'`
#'		* `'CESM1-CAM5-1-FV2'` or `'CE'`
#'		* `'CNRM-CM5'` or `'CN'`
#'		* `'GFDL-CM3'` or `'GF'`
#'		* `'GFDL-ESM2G'` or `'GD'`
#'		* `'GISS-E2-R'` or `'GS'`
#'		* `'HadGEM2-AO'` or `'HD'`
#'		* `'HadGEM2-CC'` or `'HG'`
#'		* `'HadGEM2-ES'` or `'HE'`
#'		* `'INMCM4'` or `'IN'`
#'		* `'IPSL-CM5A-LR'` or `'IP'`
#'		* `'MIROC-ESM-CHEM'` or `'MI'`
#'		* `'MIROC-ESM'` or `'MR'`
#'		* `'MIROC5'` or `'MC'`
#'		* `'MPI-ESM-LR'` or `'MP'`
#'		* `'MRI-CGCM3'` or `'MG'`
#'		* `'NorESM1-M'` or `'NO'`
#' 	* WorldClim 2.1 (CMIP6):
#'		* `'BCC-CSM2-MR'` or `'BC'`
#'		* `'CNRM-CM6-1'` or `'CC'`
#'		* `'CNRM-ESM2-1'` or `'CE'`
#'		* `'CanESM5'` or `'CA'`
#'		* `'GFDL-ESM4'` or `'GF'`
#'		* `'IPSL-CM6A-LR'` or `'IP'`
#'		* `'MIROC-ES2L'` or `'MR'`
#'		* `'MIROC6'` or `'MC'`
#'		* `'MRI-ESM2-0'` or `'ME'`
#'
#' @param ghg Greenhouse gas emissions scenario for future rasters. Valid values depend on the version of WorldClim. One or more of a valid set can be specified. This argument is ignored if near present-day rasters are being downloaded.
#' 	* WorldClim 1.4 (CMIP5): These are representative concentration pathways (RCPs), and valid values are are one or more of 26, 45, 60, and/or 85.
#'	* WorldClim 2.1 (CMIP6): These are shared socioeconomic pathways (SSPs), and valid values are one or more of 126, 245, 370, and/or 585.
#'
#' @param period Year(s) of the time period from which to download climate rasters. Valid values depend on the version of WorldClim and whether future or historical decadal averages are being downloaded:
#'	* WC 1.4 future scenario normals:
#'			* `2050` (average across 2041-2060)
#'			* `2070` (average across 2061-2080)
#'	* WC 2.1 future scenario normals:
#'			* `2030` (average across 2021-2040)
#'			* `2050` (average across 2041-2060)
#'			* `2070` (average across 2061-2080)
#'			* `2090` (average across 2081-2100)
#'	* WC 2.1 decadal monthly averages:
#'			* `1960` (average across 1960-1969)
#'			* `1970` (average across 1970-1979)
#'			* `1980` (average across 1980-1989)
#'			* `1990` (average across 1990-1999)
#'			* `2000` (average across 2000-2000)
#'			* `2010` (average across 2010-2018)
#'
#' @param overwrite If `FALSE` (default), do not overwrite existing rasters.
#' @param verbose If `TRUE` (default), display progress.
#' @param fail If `FALSE` (default), stop with an error if a zip file is missing. Otherwise, continue with a warning.
#'
#' @returns One or more zipped raster sets are saved to disk. The function also returns a data frame indicating if the desired file(s) were already on the disk and if they were downloaded.
#'
#' @references
#' Fick, S.E. and Hijmans, R.J. 2017. WorldClim 2: New 1-km spatial resolution climate surfaces for global land areas. *International Journal of Climatology* 37:4302-4315. \doi{10.1002/joc.5086}.
#'
#' Hijmans, R.J., Cameron, S.E., Parra, J.L., Jones, P.G., and Jarvis, A. 2005. Very high resolution interpolated climate surfaces for global land areas. *International Journal of Climatology* 25:1965-1978. \doi{10.1002/joc.1276}.
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
#' @rdname wcUnpack
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
