#' Names, abbreviations, version numbers, and URLs for each climate data source
#'
#' Names/abbreviations, URLs for raster files, URLs for timestamp files, and file pattern names for climate data sources. It is advisable to update `airData` using [airUpdate()] before using the "download" functions in **airUpThere**.
#'
#' @docType data
#'
#' @usage data(airData)
#'
#' @format An object of class `list`. The list contains a series of vectors and tables:
#'
#' * `airUrls`: A `data.frame` of URLs for each data source.
#' * `airVars`: Names of WorldClim, CHELSA, TerraClimate, and PRISM variables. "Standard" and "file" names are provided. Variable names can vary between and even within data sources. For example, WorldClim 2.1 uses "bio" to refer to BIOCLIM variables in files representing historical climate, but "bioc" in files representing future climate. This data frame provides a crosswalk between the source/version names used in filenames and URLs and a "standard" name. You can use `convertVar()` to convert between file-name and standard-name formats of variables. The exact definitions and units of variables may vary by source even though they have the same filename and standard name. For example, PRISM rasters represent minimum temperature (tmin) in 10ths of a degree C, whereas WorldClim 2.1 historical and future rasters represent minimum temperature (tmin) in degrees C. The time periods over which the same variable is calculated may also vary by source and version.  Please consult the publication(s) and metadata associated with each data source for further information.
#' * `ghgs`: A list of `data.frame`s with abbreviations for greenhouse gas emission scenarios for each CMIP.
#' * `wcEsm`: A list of `data.frame`s with metadata on WorldClim rasters.
#' * `chEsm`: A list of `data.frame`s with metadata on CHELSA rasters.
#'
#' @seealso [airUpdate()]
#'
#' @examples
#' data(airData)
#' airData
#' @export
'airData'
