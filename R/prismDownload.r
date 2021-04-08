#' Download PRISM climate rasters
#'
#' This function downloads PRISM climate rasters. It duplicate functionality of the \pkg{prism} package.
#'
#' @param saveTo Name of the base path to which to save the download. Subfolders will be created within this folder.
#' @param unzipTo Name of the base path to which to unzip downloaded rasters. Subfolders will be created within this folder.
#' @param ver One or more of: \code{'annual'}, \code{'monthly'}, and/or \code{'daily'}.
#' @param res Resolution(s). One or more of 10 (10 arcminutes), 5 (5 arcminutes), 2.5 (2.5 arcminutes), and/or 30 (30 arcseconds).
#' @param var Name(s) of variable(s) to download:
#' \itemize{
#' 	\item \code{tmin}: minimum temperature (available for all)
#' 	\item \code{tmax}: maximum temperature (available for all)
#' 	\item \code{tmean}: mean temperature (available for all)
#' 	\item \code{ppt}: precipitation (available for all)
#' 	\item \code{tdmean}: Dew-point temperature (available for PRISM subscription service only).
#' 	\item \code{vpdmin}: Minimum vapor pressure deficit (available for PRISM subscription service only).
#' 	\item \code{vpdmax}: Maximum vapor pressure deficit (available for PRISM subscription service only).
#' }
#' @param startDate,endDate Dates that encompasses the time period desired. This must be in YYYY-MM-DD format (e.g., \code{1981-01-22} or objects of class \coed{Date}. Note that even if the monthly or annual rasters are desired, the dates must still be specified as YYYY-MM-DD. The day or day and month will be ignored, in this case. For example, \code{startDate = '1981-01-01'} and \code{endDate = '1982-01-01'} will fetch 13 months of monthly climate rasters, or 2 years of annual rasters. Valid values of dates depend on the version of PRISM:
#' 	\itemize{
#' 		\item Annual and monthly: 1895 to present
#' 		\item Daily (subscription only): 1981 to present
#' }
#' @param updateOnly If \code{TRUE} (default), only updated rasters will be downloaded. The rasters being updated will be overwritten. If \code{FALSE}, then all rasters will be overwritten, regardless of whether they have been updated or not.
#' @param public If \code{TRUE} (default), download the free, public ("4-km") raster set. If \code{FALSE}, download from the subscription service. If \code{NULL}, then use the URL specified in \code{service}.
#' @param service This is hard-coded into the script and thus usually ignored unless \code{public = NULL}. In that case, one can specify the URL of the PRISM download service being called. The default (\coed{public = TRUE}) is to get the free ("4-km") version from \code{http://services.nacse.org/prism/data/public/4km}. The subscription version (\coed{public = FALSE}) is from \code{http://services.nacse.org/prism/data/subscription/800m}. To use the subscription option, you must using a IP addresses registered with PRISM staff.
#' @param resol Character, name of the resolution being downloaded. Normally, users do not need to worry about this unless they specify the \code{service} from which rasters are being downloaded. Typical values are either \code{'4km'} or \coed{'30s'}. This is used in the file names.
#' 
#' @return One or more zipped raster sets are saved to disk. The function also returns a data frame indicating if the desired file(s) were already on the disk and if they were downloaded.
#' @references
#' Daly, C., Halbleib, M., Smith, J.I., Gibson, W.P., Doggett, M.K., Taylor, G.H., Curtis, J., and Pasteris, P.A. 2008. Physiographically-sensitive mapping of temperature and precipitation across the conterminous United States. International Journal of Climatology, 28: 2031-2064 \href{https://doi.org/10.1002/joc.1688}{DOI: 10.1002/joc.1688} \cr
#' Daly, C., J.I. Smith, and K.V. Olson. 2015. Mapping atmospheric moisture climatologies across the conterminous United States. PloS ONE 10:e0141140. \href{https://doi.org/10.1371/journal.pone.0141140}{DOI: 10.1371/journal.pone.0141140}. \cr
#' @examples
#' 
#' \dontrun{
#' dl <- 'C:/ecology/!Scratch/prism'
#' startDate <- '1981-01-01'
#' endDate <- '1981-02-01'
#' prismDownload(dl, 'annual', 'tmin', startDate, endDate)
#' prismDownload(dl, 'monthly', 'tmin', startDate, endDate)
#' # prismDownload(dl, 'daily', 'tmin', startDate, endDate) # subscription
#' 
#' }
#' @export

prismDownload <- function(
	saveTo,
	unzipTo,
	ver,
	var,
	startDate,
	endDate,
	updateOnly = TRUE,
	public = TRUE,
	http = NULL,
	resol = NULL
) {

	# service
	if (is.null(public)) {
		http <- service
	} else if (public) {
		http <- 'http://services.nacse.org/prism/data/public/4km' # free, public service
		resol <- '4km'
	} else if (!public) {
		http <- 'http://services.nacse.org/prism/data/subscription/800m' # subscription service
		resol <- '30s'
	}
	
	# dates
	startDate <- as.Date(startDate)
	endDate <- as.Date(endDate)

	# directories
	dir.create(paste0(saveTo, '/', resol), showWarnings=FALSE, recursive=TRUE)
	dir.create(paste0(unzipTo, '/', resol), showWarnings=FALSE, recursive=TRUE)

	# create tempfile path
	tempFile <- paste0(tempfile(), '.txt')

	### annual
	##########

saveTo = 'E:/ecology/!Scratch/prism/saveTo'
unzipTo = 'E:/ecology/!Scratch/prism/unzipTo'
		
		if ('annual' %in% ver) {
		
			# years
			startYear <- substr(as.character(startDate), 1, 4)
			endYear <- substr(as.character(endDate), 1, 4)
			
			startYear <- as.integer(startYear)
			endYear <- as.integer(endYear)
			
			years <- endYear:startYear
			
			# cycle years
			for (year in years) {
			
				# cycle variables
				for (thisVar in var) {
					
					url <- paste0(http, '/', thisVar, '/', year)
					
					fileName <- paste0('annual_', year, '.zip')
					thisSaveTo <- paste0(saveTo, '/annual/', thisVar, '/', year)
					dir.create(thisSaveTo, showWarnings=FALSE, recursive=TRUE)
				
					thisSaveToFileName <- paste0(thisSaveTo, '/', fileName)
				
					downloaded <- FALSE
					tryNumber <- 1
					
					while (tryNumber <= 10 & !downloaded) {
						
						downloaded <- TRUE
						tryCatch(
							utils::download.file(url, destfile=thisSaveToFileName, mode='wb', quiet=TRUE),
							error=function(e) { downloaded <<- FALSE }
						)

						Sys.sleep(1)
						
						tryNumber <- tryNumber + 1
					}

						if (nrow(success) > 1) Sys.sleep(1)
						
					
					}
					
				} # next variable
			} # next year
		} # if annual


}
