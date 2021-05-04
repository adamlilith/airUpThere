#' Quickly(?) obtain a set of WorldClim rasters
#'
#' This function is a wrapper for a corresponding pair of \code{wcDownload} and \code{wcUnpack} functions. The files are saved to a temporary directory but returned to R as \code{SpatRast} objects. Note that depending on the size of the desired rasters, the function could take a long time!  Rasters are saved to a temporary directory on your system that should be emptied if R closes normally. However, if it does not, then it is possible for the temporary files to be saved between sessions and thus take a lot of space.
#'
#' @param ver WorldClim Version. Either \code{1.4} or \code{2.1}.
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
#' @param saveTo,unzipTo Directory in which files are downloaded and unzipped. If \code{NULL} (default), then rasters will be saved in a temporary directory.
#' @param warn If \code{TRUE} (default), provide a warning for cases where rasters being downloaded are huge.
#'
#' @return One or more \code{SpatRaster} objects.
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

getWc <- function(
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

	if (any(c(!is.null(esm), !is.null(ghg)))) stop('One or more of "esm" and "ghg" is NULL.')

	if (is.null(saveTo)) {

		scratch <- tempdir()
		saveTo <- paste0(scratch, '/zip', round(10^6 * runif(1)))
		
	}
		
	if (is.null(unpackTo)) {

		scratch <- tempdir()
		unpackTo <- paste0(scratch, '/unzip', round(10^6 * runif(1)))
		
	}
		
	dir.create(saveToZip, showWarnings=FALSE, recursive=TRUE)
	dir.create(saveToUnzip, showWarnings=FALSE, recursive=TRUE)

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
		ok <- wcUnpackFut(upackFrom=saveTo, unpackTo=unpackTo, ver=ver, res=res, vars=var, esm=esm, ghg=ghg, period=period, verbose=FALSE)

	}
	
	rastNames <- if (ver == 1.4) {
		list.files(saveToUnzip, pattern='.bil', full.names=TRUE)
	} else if (ver == 2.1) {
		list.files(saveToUnzip, pattern='.tif', full.names=TRUE)
	}

	rasts <- terra::rast(rastNames)
	rasts

}
