#' Obtain example data that comes with airUpThere
#'
#' @description This function makes retrieving example data that comes with **airUpThere** easy.
#'
#' @param x Character: The name of a data set:
#' * [parks]: Outline of two parks in the City of Saint Louis, Missouri, USA ("polygons" `SpatVector`)
#' * [stl]: Outline of the City of Saint Louis
#' * [trees]: Randomly sampled locations of trees managed by the City of Saint Louis ("points", `SpatVector`)
#'
#' @returns A `SpatVector` or `SpatRaster`.
#'
#' @examples
#' 
#' stl <- airExample('stl')
#' stl
#' plot(stl)
#' 
#' @export
airExample <- function(x) {

	vectors <- c('parks', 'stl', 'trees')
	tables <- c()
	rasters <- c()

	if (!inherits(x, 'character')) {
		stop('Please supply the name of an example raster or spatial vector in airUpThere.')
	} else {
		if (x %in% c(vectors, tables)) {

			file <- system.file('extdata', paste0(x, '.gpkg'), package = 'airUpThere')
			out <- terra::vect(file)

		} else if (x %in% rasters) {

			file <- system.file('extdata', paste0(x, '.tif'), package = 'airUpThere')
			out <- terra::rast(file)

		} else {
			stop('Please supply the name of a data object available in airUpThere.')
		}
	}

	out

}
