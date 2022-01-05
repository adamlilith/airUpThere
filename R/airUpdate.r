#' Update URLs and variable names used by airUpThere
#'
#' The purpose of this function is to enable easy updates to new versions of climate data sources as they become available without having to update the entire package. The function downloads the latest version of URLs, file name patterns, and variable names used by \code{airUpThere}. It is not necessary to run this function before using \pkg{airUpThere}, but if you find that files will not download using a \code{xxDownload} function, please try running this function then trying again.
#' @return Creates objects named \code{airUrls} and \code{airVars}. Invisibly returns \code{TRUE} or \code{FALSE}, depending on success.
#' @examples
#'
#' airUpdate()
#'
#' @export
airUpdate <- function(verbose=TRUE) {

	# URL for updates
	url <- 'https://github.com/adamlilith/airUpThere_updates/raw/main/airData.rda'

	filePath <- paste0(tempdir(), '/airData.rda')

	tryNumber <- 1
	
	downloaded <- FALSE
	while (tryNumber <= 10 & !downloaded) {
		
		downloaded <- TRUE
		tryCatch(
			utils::download.file(url, destfile=filePath, method='auto', quiet=TRUE),
			error=function(e) { downloaded <<- FALSE }
		)
		
		if (!downloaded) Sys.sleep(1)
		
		tryNumber <- tryNumber + 1
	}
	
	if (downloaded) {
		
		load(filePath)
		if (verbose) cat('airUpThere metadata has been successfully updated.\n'); flush.console()
		
	} else {
		if (verbose) cat('airUpThere metadata has *not* been successfully updated. Using package data.\n'); flush.conosle()
		data('airData', envir=.GlobalEnv)
	}
	
	airUrls <<- airData$airUrls
	airVars <<- airData$airVars
	ghgs <<- airData$ghgs
	
	wcEsm <<- airData$wcEsm
	wcPeriod <<- airData$wcPeriod
	wcRes <<- airData$wcRes

	invisible(downloaded)

}
