#' airUpThere: Extract data from PRISM weather rasters (daily/monthly/annual)
#'
#' @description These functions extract values from interpolated weather rasters from the Parameter Regression on Independent Slopes (PRISM) data product (Daly et al. 2008 and 2015). Depending on the set, PRISM rasters represent interpolated values of daily, monthly, or annual weather. Extractions are done at points (versus polygons or lines, for example). Extractions can be for specific dates or a user-defined temporal window preceding each date plus focal dates (e.g., each of the 10 days before the date, plus the date). \cr\cr
#'
#' Create an issue on \href{https://github.com/adamlilith/airUpThere/issues}{GitHub}.
#' @details
#' 		\code{\link{prismExtractDaily}}: Extract values of daily weather variables. \cr
#' 		\code{\link{prismExtractMonthly}}: Extract values of monthly weather variables. \cr
#' 		\code{\link{prismExtractYearly}}: Extract values of annual weather variables. \cr
#' @references
#' Daly, C., Halbleib, M., Smith, J.I., Gibson, W.P., Doggett, M.K., Taylor, G.H., Curtis, J., and Pasteris, P.A. 2008. Physiographically-sensitive mapping of temperature and precipitation across the conterminous United States. International Journal of Climatology, 28: 2031-2064 \href{https://doi.org/10.1002/joc.1688}{DOI: 10.1002/joc.1688} \cr
#' Daly, C., J.I. Smith, and K.V. Olson. 2015. Mapping atmospheric moisture climatologies across the conterminous United States. PloS ONE 10:e0141140. \href{https://doi.org/10.1371/journal.pone.0141140}{DOI: 10.1371/journal.pone.0141140}. \cr
#' Fick, S.E. and Hijmans, R.J. 2017. WorldClim 2: New 1-km spatial resolution climate surfaces for global land areas. \emph{International Journal of Climatology} 37:4302-4315. \href{https://doi.org/10.1002/joc.5086}{DOI: https://doi.org/10.1002/joc.5086} \cr
#' Hijmans, R.J., Cameron, S.E., Parra, J.L., Jones, P.G., and Jarvis, A. 2005. Very high resolution interpolated climate surfaces for global land areas. \emph{International Journal of Climatology} 25:1965-1978. \href{https://doi.org/10.1002/joc.1276}{DOI: https://doi.org/10.1002/joc.1276}
#' @docType package
#' @author Adam B. Smith
#' @name airUpThere
NULL
