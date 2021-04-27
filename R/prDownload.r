#' Download PRISM climate rasters
#'
#' This function downloads PRISM climate rasters. It duplicate functionality of the \pkg{prism} package.
#'
#' @param saveTo Name of the base path to which to save the download. Subfolders will be created within this folder.  Note that existing files may be overwritten, depending on the values of \code{unzipTo}, \code{update}, and \code{forceUpdate}.
#' @param ver One or more of: \code{'annual'}, \code{'monthly'}, and/or \code{'daily'}.
#' @param vars Name(s) of variable(s) to download:
#' \itemize{
#' 	\item \code{tmin}: minimum temperature (available for all)
#' 	\item \code{tmax}: maximum temperature (available for all)
#' 	\item \code{tmean}: mean temperature (available for all)
#' 	\item \code{ppt}: precipitation (available for all)
#' 	\item \code{tdmean}: Dew-point temperature (available for PRISM subscription service only).
#' 	\item \code{vpdmin}: Minimum vapor pressure deficit (available for PRISM subscription service only).
#' 	\item \code{vpdmax}: Maximum vapor pressure deficit (available for PRISM subscription service only).
#' }
#' @param startDate,endDate Dates that encompasses the time period desired. This must be in YYYY-MM-DD format (e.g., \code{1981-01-08} or objects of class \code{Date}. Note that even if the monthly or annual rasters are desired, the dates must still be specified as YYYY-MM-DD. The day or day and month will be ignored, in this case. For example, \code{startDate = '1981-01-01'} and \code{endDate = '1982-01-01'} will fetch 13 months of monthly climate rasters, or 2 years of annual rasters. Valid values of dates depend on the version of PRISM:
#' 	\itemize{
#' 		\item Annual and monthly: 1895 to present (free "4-km" version and LT81 version)
#' 		\item Annual and monthly: 1981 to present (AN81d and AN81m versions)
#' 		\item Daily (subscription only): 1981 to present
#' }
#' @param unzipTo Optional. If \code{NULL}, then only 
#' @param update If \code{TRUE} (default), only rasters that have been generated after the one(s) on disk will be downloaded. The rasters being updated will be overwritten. If \code{FALSE}, only rasters that are missing will be downloaded (existing rasters will not be overwritten).
#' @param forceUpdate If \code{FALSE} (default), then all existing rasters will be overwritten, regardless of whether or not the existing ones are up-to-date or not.
#' @param public If \code{TRUE} (default), download the free, public ("4-km") raster set. If \code{FALSE}, download from the subscription service. If \code{NULL}, then use the URL specified in \code{service}.
#' @param service This is the base URL for downloading PRISM rasters. It is actually hard-coded into the script and thus usually ignored unless \code{public = NULL}. In that case, one can specify the URL of the PRISM download service being called. The default (\coed{public = TRUE}) is to get the free ("4-km") version from \code{service://services.nacse.org/prism/data/public/4km}. The subscription version (\code{public = FALSE}) is from \code{service://services.nacse.org/prism/data/subscription/800m}. To use the subscription option, you must using a IP addresses registered with PRISM staff.
#' @param res Character, name of the resolution being downloaded. Normally, users do not need to worry about this unless they specify the \code{service} from which rasters are being downloaded. Typical values are either \code{'4km'} or \coed{'30s'}. This is used in the file names.
#'
#' @details Note that downloading from the public version has its limits... Specifically, if you try to download the same raster in the same day, the repeated attempts will instead fetch a text file that reads like, "You have tried to download the file PRISM_tmin_stable_4kmM3_1981_bil.zip more than twice in one day (Pacific local time).  Note that repeated offenses may result in your IP address being blocked."
#' 
#' @return One or more zipped raster sets are saved to disk. The function also returns a data frame indicating if the desired file(s) were already on the disk and if they were downloaded.
#' @references
#' Daly, C., Halbleib, M., Smith, J.I., Gibson, W.P., Doggett, M.K., Taylor, G.H., Curtis, J., and Pasteris, P.A. 2008. Physiographically-sensitive mapping of temperature and precipitation across the conterminous United States. International Journal of Climatology, 28: 2031-2064 \href{https://doi.org/10.1002/joc.1688}{DOI: 10.1002/joc.1688} \cr
#'
#' Daly, C., J.I. Smith, and K.V. Olson. 2015. Mapping atmospheric moisture climatologies across the conterminous United States. PloS ONE 10:e0141140. \href{https://doi.org/10.1371/journal.pone.0141140}{DOI: 10.1371/journal.pone.0141140}. \cr
#'
#' @examples
#' 
#' \dontrun{
#' dl <- 'C:/ecology/!Scratch/prism'
#' startDate <- '1981-01-01'
#' endDate <- '1981-02-01'
#' prDownload(dl, 'annual', 'tmin', startDate, endDate)
#' prDownload(dl, 'monthly', 'tmin', startDate, endDate)
#' # prDownload(dl, 'daily', 'tmin', startDate, endDate) # subscription
#' 
#' }
#' @export

prDownload <- function(
	saveTo,
	ver,
	vars,
	startDate,
	endDate,
	unzipTo = NULL,
	update = TRUE,
	forceUpdate = FALSE,
	public = TRUE,
	service = NULL,
	res = NULL
) {

	# for troubleshooting
	if (FALSE) {
	
		saveTo <- 'E:/ecology/!Scratch/prism'
		ver <- 'daily'
		vars <- 'tmin'
		startDate <- '1981-01-01'
		endDate <- '1981-01-03'
		update <- TRUE
		forceUpdate <- FALSE
		# public <- TRUE
		public <- FALSE
		service <- NULL
		res <- NULL
	
	}

	# service
	if (public) {
		if (is.null(service)) service <- 'http://services.nacse.org/prism/data/public/4km' # free, public service
		res <- '4km'
	} else if (!public) {
		if (is.null(service)) service <- 'http://services.nacse.org/prism/data/subscription/800m' # subscription service
		res <- '30s'
	}
	
	# dates
	startDate <- as.Date(startDate)
	endDate <- as.Date(endDate)

	# directories
	dir.create(paste0(saveTo, '/', res), showWarnings=FALSE, recursive=TRUE)

	# create tempfile path
	tempFile <- paste0(tempfile(), '.txt')
	
	takeNap <- (length(ver) > 1 | startDate != endDate | length(vars) > 1)

	### annual
	##########

		if ('annual' %in% ver) {
		
			# years
			startYear <- getYMD(startDate, 'y')
			endYear <- getYMD(endDate, 'y')
			
			years <- startYear:endYear
			
			success <- expand.grid(ver=ver, year=years, vars=vars, versionOnServer=NA, versionOnDisk=NA, updated=NA)
			
			# cycle years
			for (year in years) {
			
				# cycle variables
				for (thisVar in vars) {
					
					url <- paste0(service, '/', thisVar, '/', year)
					
					thisSaveTo <- paste0(saveTo, '/annual/', thisVar)
					dir.create(thisSaveTo, showWarnings=FALSE, recursive=TRUE)
				
					thisSaveToFileName <- paste0(thisSaveTo, '/', thisVar, '_annual_', year, '.zip')
				
					downloaded <- FALSE
					tryNumber <- 1
					
					while (tryNumber <= 10 & !downloaded) {
						
						downloaded <- TRUE
						tryCatch(
							# utils::download.file(url, destfile=thisSaveToFileName, mode='wb', quiet=TRUE),
							httr::GET(url, httr::write_disk(thisSaveToFileName, overwrite=TRUE)),
							error=function(e) { downloaded <<- FALSE }
						)

						if (takeNap) Sys.sleep(1)
						
						tryNumber <- tryNumber + 1
					}
					
					
					
				} # next variable
			} # next year
		} # if annual


}
