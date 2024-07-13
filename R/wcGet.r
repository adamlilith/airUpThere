#' Quickly(?) obtain a set of WorldClim rasters
#'
#' This function is a wrapper for a corresponding pair of [wcDownload()] and [wcUnpack()] functions. The files are saved to a temporary directory but returned to **R** as `SpatRaster` objects. Note that depending on the size of the desired rasters, the function could take a long time!  Rasters are saved to a temporary directory on your system that should be emptied if R closes normally. However, if it does not, then it is possible for the temporary files to be saved between sessions and thus take a lot of space.
#'
#' @param ver WorldClim Version. Either `1.4` or `2.1`.
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
#' `
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
#' @param saveTo,unzipTo Directory in which files are downloaded and unzipped. If `NULL` (default), then rasters will be saved in a temporary directory.
#' @param warn If `TRUE` (default), provide a warning for cases where rasters being downloaded are huge.
#'
#' @return One or more `SpatRaster` objects.
#' @examples
#'
#' \dontrun{
#' elev <- wcGet(2.1, res=10, var='elev')
#' tmin <- wcGet(2.1, res=10, var='tmin')
#' tmax <- wcGet(2.1, res=10, var='tmax', esm='bc', ghg=245, period=2050)
#' 
#' # decadal <- wcGet(2.1, var='tmin', period=1960) # takes a *long* time!!!
#' }
#' @export
wcGet <- function(
	ver,
	res,
	var,
	esm = NULL,
	ghg = NULL,
	period = NULL,
	saveTo = NULL,
	unpackTo = NULL,
	warn = TRUE
) {

	if (length(ver) > 1) stop('Only one version can be specified at a time.')
	if (length(res) > 1) stop('Only one resolution can be specified at a time.')
	if (length(var) > 1) stop('Only one variable can be specified at a time.')
	if (!is.null(esm)) if (length(esm) > 1) stop('Only one ESM can be specified at a time.')
	if (!is.null(ghg)) if (length(ghg) > 1) stop('Only one greenhouse gas emissions scenario can be specified at a time.')
	if (!is.null(period)) if (length(period) > 1) stop('Only one period can be specified at a time.')

	set <- c(esm, ghg, period)
	if (any(is.null(set) & any(!is.null(set)))) stop('It looks like you want to get future climate rasters.\nYou need to define each of "esm", "ghg", and "period".')

	if (is.null(saveTo)) {
		scratch <- tempdir()
		saveTo <- paste0(scratch, '/zip', round(10^6 * runif(1)))
	}
		
	if (is.null(unpackTo)) {
		scratch <- tempdir()
		unpackTo <- paste0(scratch, '/unzip', round(10^6 * runif(1)))
	}
		
	dir.create(saveTo, showWarnings=FALSE, recursive=TRUE)
	dir.create(unpackTo, showWarnings=FALSE, recursive=TRUE)

	# elevation
	if (var=='elev') {
	
		ok <- wcDownloadElev(saveTo=saveTo, res=res, verbose=FALSE)
		ok <- wcUnpackElev(upackFrom=saveTo, unpackTo=unpackTo, res=res, verbose=FALSE)
		
	# historical
	} else if (is.null(esm) & is.null(ghg) & is.null(period)) {
	
		ok <- wcDownloadHist(saveTo=saveTo, ver=ver, res=res, vars=var, verbose=FALSE)
		ok <- wcUnpackHist(upackFrom=saveTo, unpackTo=unpackTo, ver=ver, res=res, vars=var, verbose=FALSE)
		
	# decadal
	} else if (is.null(esm) & is.null(ghg) & !is.null(period)) {	

		if (warn) {
			resp <- invisible(readline(prompt='Decadal rasters are very large and will take a long time to download. Press [ENTER] to continue or any other key to stop.'))
			
			if (resp != '') stop('Abandoned attempt.')
			
		}
		ok <- wcDownloadDecadal(saveTo=saveTo, ver=ver, res=res, vars=var, period=period, verbose=FALSE)
		ok <- wcUnpackDecadal(upackFrom=saveTo, unpackTo=unpackTo, ver=ver, res=res, vars=var, period=period, verbose=FALSE)

	# future
	} else if (!is.null(esm) & !is.null(ghg) & !is.null(period)) {	

		ok <- wcDownloadFut(saveTo=saveTo, ver=ver, res=res, vars=var, esm=esm, ghg=ghg, period=period, verbose=FALSE)
		ok <- wcUnpackFut(unpackFrom=saveTo, unpackTo=unpackTo, ver=ver, res=res, vars=var, esm=esm, ghg=ghg, period=period, verbose=FALSE)

	}
	
	rastNames <- if (ver == 1.4) {
		list.files(unpackTo, pattern='.bil', full.names=TRUE)
	} else if (ver == 2.1) {
		list.files(ok$file, pattern='.tif', full.names=TRUE)
	}

	rasts <- terra::rast(rastNames)
	rasts

}
