#' Nice name for GHG WorldClim scenarios
#' 
#' Create a nice name for a WorldClim future climate scenario.
#' @param ver Version number: 1.4 or 2.1
#' @param ghg Greenhouse gas emissions scenario as a \coed{numeric} value. This is the RCP or SSP number (without a decimal point, for RCPs).
#' @return Character
#' @examples
#' wcConvertGhg(1.4, 4.5)
#' wcConvertGhg(1.4, 8.5)
#' wcConvertGhg(2.1, 126)
#' wcConvertGhg(2.1, 370)
#' @export
wcConvertGhg <- function(ver, ghg) {

	if (ver == 1.4) {
		ghg <- as.character(ghg)
		if (ghg == '6') ghg <- '60'
		ghg <- paste0(substr(ghg, 1, 1), '.', substr(ghg, 2, 2))
	}
	ghgNice <- if (ver == 1.4) {
		paste0('rcp_', ghg)
	} else if (ver == 2.1) {
		paste0('ssp_', ghg)
	} else {
		stop('This is an invalid version greenhouse gas emissions scenario.')
	}
	
	ghgNice
	
}

