#' Get a stack of TerraClimate rasters
#'
#' This function returns a stack of TerraClimate rasters for one or more variables for one or more years.
#'
#' @param rastDir Name of the base path in which TerraClimate rasters are saved. It is assumed that inside the base path there are subfolders, one per TerraClimate variable, and inside these are NetCDF rasters representing "stacks" of monthly climate values, one per year. The assumed folder structure and file-naming scheme is exactly the same as that used by \code{\link[airUpThere]{tcDownload}}, so if the rasters were downloaded using that function, this function should work.
#'
#' @param vars Name(s) of variable(s) to use.
#' @param year Year(s) of the period from which to apply the function.
#'
#' @return Object of class \code{SpatRaster}. Each raster in the stack will be named as per <variable>_<year>_<month>. For example, \code{tmin_1958_1} for the raster with minimum temperature for January of 1958. Standard variable names will be used (see \code{\link[airUpThere]{tcVars}}).
#' @examples
#' 
#' \dontrun{
#' 
#' tmins <- tcStack(rastDir, 'tmin', year=1971:1972)
#' tmins
#' 
#' }
#' @export

tcStack <- function(
	rastDir,
	vars,
	year
) {

	standVar <- aut_tcConvertVar(vars, standardToFile=FALSE)
	fileVar <- aut_tcConvertVar(vars, standardToFile=TRUE)

	### create a raster stack of all variables
	for (countVar in seq_along(standVar)) {

		thisVarFile <- fileVar[countVar]
		thisVarStand <- standVar[countVar]

		for (thisYear in year) {
	
			# path/file names
			rastPathAndFileName <- paste0(rastDir, '/', thisVarFile, '/TerraClimate_', thisVarFile, '_', thisYear, '.nc')
			
			out <- terra::rast(rastPathAndFileName)
			names(out) <- paste0(thisVarStand, '_', thisYear, '_', 1:12)
			
			masterStack <- if (exists('masterStack', inherits=FALSE)) {
				c(masterStack, out)
			} else {
				out
			}
			
		} # next year
			
	} # next variable
	
	out
	
}

