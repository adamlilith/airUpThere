#' airUpThere: Download, process, and use climate rasters from WorldClim, PRISM, TerraClimate, and more!
#'
#' This package contains utility functions for obtaining, processing, and using climate rasters from gridded climate data sets, including WorldClim, CHELSA, TerraClimate, and PRISM.
#'
#' Create an issue on \href{https://github.com/adamlilith/airUpThere/issues}{GitHub}.
#'
#' @details
#'
#' @section airUpThere essentials:
#'		\code{\link{airUpdate}}: Retrieve latest URLs \cr
#'
#' @section CHELSA:
#' 		\code{\link{chDownloadPaleoBio}}, \code{\link{chDownloadPaleoClim}}, \code{\link{chDownloadPaleoElev}} Download CHELSA paleo-climate and paleo-elevation/ice rasters \cr
#' 		\code{\link{chDownloadFutureBio}}, \code{\link{chDownloadFutureClim}} Download CHELSA future climate \cr
#' 		\code{\link{chEsm}} List earth system models available for CHELSA \cr
#' 		\code{\link{chConvertPeriod}} Translate betweeen "short" and "long" version of future time periods \cr
#' 		\code{\link{chPeriod}} Future time periods available for CHELSA \cr
#'
#' @section PRISM:
#' 		\code{\link{prDownload}} Download PRISM climate rasters \cr
#' 		\code{\link{prExtractRelative}} Extract values from PRISM climate rasters across a temporal window relative to series of dates (e.g., all values up to 10 days prior to set of dates) \cr
#' 		\code{\link{prExtractAbsolute}} Extract values from PRISM climate rasters across a specified temporal window (e.g., all values between 1 January 1981 and 31 January 1981, inclusive) \cr
#' 		\code{\link{prStack}} Create a stack of rasters representing particular dates or across a time series \cr
#' 		\code{\link{prVars}} Names of available PRISM variables. \cr
#'
#' @section WorldClim:
#' 		\code{\link{wcDownload}} Download WorldClim rasters (present/future of versions 1.4 and 2.1, elevation, decadal averages) \cr
#' 		\code{\link{wcConvertVar}} Convert names of WorldClim variables from "file" to "standard" format \cr
#' 		\code{\link{wcConvertRes}} Name of WorldClim resolution as used in URLs and file names. \cr
#' 		\code{\link{wcConvertPeriod}} Name of WorldClim future climate period as used in URLs and file names. \cr
#' 		\code{\link{wcConvertGhg}} Nice name for WorldClim future climate scenarios \cr
#' 		\code{\link{wcEsm}} Names and abbreviations of earth systems models represented in WorldClim \cr
#' 		\code{\link{wcGet}} Quickly(?) obtain a set of WorldClim rasters \cr
#' 		\code{\link{wcVars}} Names of available WorldClim variables. \cr
#' 		\code{\link{wcRes}} Names of resolutions available for WorldClim. \cr
#'
#' @section TerraClimate:
#' 		\code{\link{tcDownloadMonthly}} Download TerraClimate monthly climate rasters \cr
#' 		\code{\link{tcDownloadElev}} Download TerraClimate elevation raster \cr
#' 		\code{\link{tcStack}} Get a stack of TerraClimate rasters \cr
#' 		\code{\link{tcVars}} Names of available TerraClimate variables. \cr
#'
#' @section Useful utilities:
#'		\code{\link{compareDates}}, \code{\link{\%>d%\}}, \code{\link{\%>=d%\}}, \code{\link{\%==d%\}}, \code{\link{\%<d%\}}, \code{\link{\%<=d%\}}, and \code{\link{\%!=d%\}} Compare dates in YYYY, YYYY-MM, or YYYY-MM-DD formats. \cr
#'		\code{\link{convertVar}} Convert between "file/URL" and "standard" names of variables. \cr
#' 		\code{\link{formatYYYYMM}} Get YYYY-MM format from a date. \cr
#'		\code{\link{ghgNames}} Names of Representative Concentration Pathways (RCPs) and Shared Socioeconomic Pathways (SSPs). \cr
#'		\code{\link{getCoords}} Get coordinates from a data frame, SpatialPoints, or SpatVector object. \cr
#' 		\code{\link{getYMD}} Get year, month, or day from a date. \cr
#'		\code{\link{monthAsNum}} Convert month name to number. \cr
#'		\code{\link{monthDiff}} Difference in months between two dates expressed as YYYY-MM. \cr
#' 		\code{\link{seqMonths}} Generate a sequence of years and months from a starting and ending date. \cr
#'
#'
#' ## Example data
#' [airData][airData]: Meta-data on each of the climate/weather products managed by **airUpThere**\cr
#' [airExample()]: Easily load example data\cr
#' [nad83][nad83]: North American Datum 1983 (NAD83)\cr
#' [stl][stl]: Outline of the City of Saint Louis, Missouri, USA\cr
#' [parks][parks]: Outline of two parks in Saint Louis\cr
#' [trees][trees]: Location of trees in Saint Louis\cr
#' [wgs84][wgs84]: World Geodetic System 1984 (WGS84)\cr
#'
#' @references
#' @section CHELSA
#' Karger, D.N., Conrad, O., Böhner, J., Kawohl, T., Kreft, H., Soria-Auza, R.W., Zimmermann, N.E., Linder, P., Kessler, M. (2017). Climatologies at high resolution for the Earth land surface areas. Scientific Data. 4 170122. \href{https://doi.org/10.1038/sdata.2017.122}{DOI: 10.1038/sdata.2017.122}
#'
#' Karger D.N., Conrad, O., Böhner, J., Kawohl, T., Kreft, H., Soria-Auza, R.W., Zimmermann, N.E,, Linder, H.P., Kessler, M.. Data from: Climatologies at high resolution for the earth’s land surface areas. Dryad Digital Repository. \href{http://dx.doi.org/doi:10.5061/dryad.kd1d4}{10.5061/dryad.kd1d4} 
#'
#' @section PRISM
#' Daly, C., Halbleib, M., Smith, J.I., Gibson, W.P., Doggett, M.K., Taylor, G.H., Curtis, J., and Pasteris, P.A. 2008. Physiographically-sensitive mapping of temperature and precipitation across the conterminous United States. International Journal of Climatology, 28: 2031-2064 \href{https://doi.org/10.1002/joc.1688}{DOI: 10.1002/joc.1688} \cr
#'
#' Daly, C., J.I. Smith, and K.V. Olson. 2015. Mapping atmospheric moisture climatologies across the conterminous United States. PloS ONE 10:e0141140. \href{https://doi.org/10.1371/journal.pone.0141140}{DOI: 10.1371/journal.pone.0141140}. \cr
#'
#' @section TerraClimate
#' Abatzoglou, J.T., Dobrowski, S.Z., Parks, S.A., and Hegewisch, K.C. 2018. TerraClimate, a high-resolution global dataset of monthly climate and climatic water balance from 1958-2015. \emph{Scientific Data} 5:170191. doi: \href{https://dx.doi.org/10.1038/sdata.2017.191}{10.1038/sdata.2017.191}. \cr
#'
#' @section WorldClim
#' Fick, S.E. and Hijmans, R.J. 2017. WorldClim 2: New 1-km spatial resolution climate surfaces for global land areas. \emph{International Journal of Climatology} 37:4302-4315. doi: \href{https://doi.org/10.1002/joc.5086}{10.1002/joc.5086} \cr
#'
#' Hijmans, R.J., Cameron, S.E., Parra, J.L., Jones, P.G., and Jarvis, A. 2005. Very high resolution interpolated climate surfaces for global land areas. \emph{International Journal of Climatology} 25:1965-1978. doi: \href{https://doi.org/10.1002/joc.1276}{10.1002/joc.1276}.
#'
#' @docType package
#' @author Adam B. Smith
#' @name airUpThere
#' @keywords internal
NULL
