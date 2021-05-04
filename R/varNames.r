#' "Standard" and "file/URL" names of variables
#'
#' Names of WorldClim, CHELSA, TerraClimate, and PRISM variables. "Standard" and "file" names are provided. Variable names can vary between and even within data sources. For example, WorldClim 2.1 uses "bio" to refer to BIOCLIM variables in files representing historical climate, but "bioc" in files representing future climate. This data frame provides a crosswalk between the source/version names used in filenames and URLs and a "standard" name. You can use \code{\link{convertVar}} to convert between file-name and standard-name formats of variables. \cr
#' Note that the exact definitions and units of variables may vary by source even though they have the same filename and standard name. For example, PRISM rasters represent minimum temperature (tmin) in 10ths of a degree C, whereas WorldClim 2.1 historical and future rasters represent minimum temperature (tmin) in degrees C. The time periods over which the same variable is calculated may also vary by source and version.  Please consult the publication(s) and metadata associated with each data source for further information.
#'
#' @docType data
#'
#' @usage data(varNames)
#'
#' @format An object of class \code{'data.frame'}.
#'
#' @keywords variables
#
#' @seealso \code{\link[airUpThere]{convertVar}}
#'
#' @examples
#' data(varNames)
#' varNames
'varNames'
