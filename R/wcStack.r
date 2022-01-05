#' @name wcStack
#' @rdname wcStack
#' @title Stack WorldClim decadal or future rasters

#' @description These functions create a raster "stack" of similar rasters from WorldClim. For example, you can create a stack that has all \code{tmin} rasters for several greenhouse gas emissions scenarios and time periods. All rasters much be the same resolution. The rasters must be "unpacked" first using \code{\link[airUpThere]{wcUnpack}}.

#' @param dir Name of base directory in which files have been saved.
#' @param ver WorldClim Version. Either \code{1.4} or \code{2.1}. Note that some resolutions and/or combinations of variables and earth system models may not be available.
#' @param res Resolution of the rasters. Valid values include:
#' \itemize{
#'		\item WC 1.4 future scenario normals: \code{10} (10 arcmin), \code{5} (5 arcmin), \code{2.5} (2.5 arcmin), and/or \code{30} (30 arcsec).
#'		\item WC 2.1 future scenario normals: \code{10} (10 arcmin), \code{5} (5 arcmin), and/or \code{2.5} (2.5 arcmin).
#'		\item WC 2.1 decadal monthly averages: \code{2.5} (2.5 arcmin).
#' }
#' @param Name of variable(s) to stack \emph{plus} the month/BIOCLIM number. Typically you would just want to stack a single variable (e.g., all rasters representing minimum temperature in January, or \code{tmin1}).  Valid values are:
#' \itemize{
#' 	\item \code{'tmin'}\emph{X}: minimum temperature of month \emph{X} (available for WC 2.1 decadal)
#' 	\item \code{'tmax'}\emph{X}: maximum temperature of month \emph{X} (available for 2.1 decadal)
#' 	\item \code{'ppt'}\emph{X}: precipitation of month \emph{X} (available for WC 1.4 and 2.1 future, and WC 2.1 decadal)
#' 	\item \code{'bio'}\emph{X}: BIOCLIM variables number \emph{X} (available for WC 1.4 and 2.1 future)
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
#' @param warn If \code{TRUE} (default), warn if a raster is missing.
#' @param fail If \code{FALSE} (default), do not throw an error if a raster is missing.

#' @examples

#' @export
NULL



#' @describeIn wcStack Stack WorldClim future rasters
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