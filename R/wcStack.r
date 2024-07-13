#' @name wcStack
#' @rdname wcStack
#' @title Stack WorldClim decadal or future rasters
#'
#' @description These functions create a raster "stack" of similar rasters from WorldClim. For example, you can create a stack that has all `tmin` rasters for several greenhouse gas emissions scenarios and time periods. All rasters much be the same resolution. The rasters must be "unpacked" first using [wcUnpack()].
#'
#' @param dir Name of base directory in which files have been saved.
#' @param ver WorldClim Version. Either `1.4` or `2.1`. Note that some resolutions and/or combinations of variables and earth system models may not be available.
#' @param res Resolution of the rasters. Valid values include:
#'	* WC 1.4 future scenario normals: `10` (10 arcmin), `5` (5 arcmin), `2.5` (2.5 arcmin), and/or `30` (30 arcsec).
#'	* WC 2.1 future scenario normals: `10` (10 arcmin), `5` (5 arcmin), and/or `2.5` (2.5 arcmin).
#'	* WC 2.1 decadal monthly averages: `2.5` (2.5 arcmin).
#'
#' @param Name of variable(s) to stack *plus` the month/BIOCLIM number. Typically you would just want to stack a single variable (e.g., all rasters representing minimum temperature in January, or `tmin1`).  Valid values are:
#' * `'tmin'`*X*`: minimum temperature of month `*X*` (available for WC 2.1 decadal)
#' * `'tmax'`*X*`: maximum temperature of month `*X*` (available for 2.1 decadal)
#' * `'ppt'`*X*`: precipitation of month `*X*` (available for WC 1.4 and 2.1 future, and WC 2.1 decadal)
#' * `'bio'`*X*`: BIOCLIM variables number `*X*` (available for WC 1.4 and 2.1 future)
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
#'		`
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
#' `
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
#' @param warn If `TRUE` (default), warn if a raster is missing.
#' @param fail If `FALSE` (default), do not throw an error if a raster is missing.
#' @examples
#' NULL
#'
#' @export
wcStackFut <- function(
	dir,
	ver,
	res,
	vars,
	esm,
	ghg,
	period, 
	warn = TRUE,
	fail = FALSE
) {

	resFile <- wcConvertRes(ver=ver, res=res, period='future', standardToFile=TRUE)
	resWithUnit <- wcConvertRes(ver=ver, res=resFile, period='future', standardToFile=FALSE)

	if (ver >= 2) {

		for (thisVar in vars) {
		
			if (grepl(thisVar, pattern='tmin') | grepl(thisVar, pattern='tmax')) {
				varName <- substr(thisVar, 1, 4)
				varNum <- substr(thisVar, 5, nchar(thisVar))
			} else if (grepl(thisVar, pattern='ppt') | grepl(thisVar, pattern='bio')) {
				varName <- substr(thisVar, 1, 3)
				varNum <- substr(thisVar, 4, nchar(thisVar))
			}
			
			varNum <- as.integer(varNum)
			
			varFile <- convertVar(src='wc', vars=varName, ver=ver, period='future', standardToFile=TRUE)
		
			for (thisEsm in esm) {
			
				esmFile <- wcConvertEsm_internal(ver, thisEsm)
			
				for (thisGhg in ghg) {
		
					ghgNice <- wcConvertGhg(ver, thisGhg)
		
					for (thisPeriod in period) {
					
						periodFile <- wcConvertPeriod(ver, thisPeriod, type='future')
					
						fileName <- paste0(
							dir, '/worldclim_', ver, '_',
							resWithUnit, '_',
							ghgNice, '_',
							periodFile, '/wc', ver, '_',
							resFile, '_',
							varFile, '_',
							esmFile, '_',
							ghgNice, '_',
							periodFile,
							'.tif'
						)
						
						fileExists <- file.exists(fileName)
						if (!fileExists) {
						
							if (fail) {
								stop('Raster does not exist: ', fileName)
							} else if (warn) {
								warning('Raster does not exist: ', fileName)
							}
							
						} else {
							
							r <- terra::rast(fileName)
							r <- r[[varNum]]
							
							names(r) <- paste0(thisVar, '_', thisGhg, '_', thisPeriod, '_', thisEsm)
							
							rasts <- if (!exists('rasts', inherits=FALSE)) {
								r
							} else {
								c(rasts, r)
							}
							
						}
							
					
					} # next period
		
				} # next GHG
				
			} # next ESM
		
		} # next variable
		
	} # if version >= 2

	rasts

}
