#' Convert name resolution of WorldClim in "standard" and URL/file formats
#' 
#' Convert between the "standard" and "file/URL" versions of names of resolutions in WorldClim. You can see a full table of resolutions using \code{data(wcRes)}.
#' @param ver Either \code{1.4} or \coed{2.1}.
#' @param res Either "nice" names of resolutions (any of \code{10}, \code{5}, \code{2.5}, or \code{30}), or "file/URL" names:
#' 	\itemize{
#'		\item WorldClim Version 1.4, historical: \code{10m}, \code{5m}, \code{2-5m}, or \code{30s}
#'		\item WorldClim Version 1.3, future: \code{10m}, \code{5m}, \code{2_5m}, or \code{30s}
#'		\item WorldClim Version 2.1: \code{10m}, \code{5m}, \code{2.5m}, or \code{30s}
#'	}
#' @param period Either \code{'historical'}, \code{'future'}, or \code{'decadal'}.
#' @param standardToFile If \code{TRUE}, \code{res} is assumed to be in "standard" format, so will be translated into "file/URL" format. If \code{FALSE}, then it will be assumed to be in "file/URL" format and will be translated into "standard" format (possibly with unit, if \code{incUnit} is \code{TRUE}).
#' @param incUnit If \code{TRUE}, include unit if \code{standardToFile} is also \code{TRUE}. If \code{standardToFile} is \code{FALSE}, this is ignored.
#' @return Character
#' @examples
#' wcConvertRes(1.4, 2.5, 'historical', TRUE)
#' wcConvertRes(1.4, '2-5m', 'historical', FALSE)
#' wcConvertRes(1.4, 2.5, 'future', TRUE)
#' wcConvertRes(1.4, '2_5m', 'future', FALSE)
#' wcConvertRes(2.1, 2.5, 'historical', TRUE)
#' wcConvertRes(2.1, '2.5m', 'historical', FALSE)
#' @export
wcConvertRes <- function(ver, res, period, standardToFile=TRUE, incUnit=TRUE) {

	data('wcRes', envir = environment())

	col <- paste0('worldclim_', ver, '_', period)
	if (standardToFile) {
		index <- which(wcRes$standard == res)
		if (length(index) == 0) stop('This is an invalid resolution for WorldClim ', ver, '.')
		out <- wcRes[index, col]
	} else {
		index <- which(wcRes[ , col, drop=TRUE] == res)
		if (length(index) == 0) stop('This is an invalid resolution for WorldClim ', ver, '.')
		out <- wcRes$standard[index]
		if (incUnit) out <- paste0(out, '_', wcRes$unit[index])
	}
	
	out
	
}

