
library(terra)

data(nad83)

# Base folder with PRISM rasters
prDir <- 'F:/ecology/PRISM/working/an81'

# Example data (polygons)
parks <- airExample('parks')
crs(parks) <- nad83 # NB: More accurate than projecting from WGS84 to NAD83
parks$startDate <- c('2011-09-01', '1859-09-01', '2022-09-01', '2055-09-01')
parks$endDate <- c('2011-09-03', '1859-09-03', '2022-09-09', '2055-09-01')

# Template raster only required for extracting to lines or polygons
template <- rast('F:/ecology/PRISM/PRISM_us_dem_800m.tif')

dy <- prExtractAbsoluteDaily(
    x = parks,
    startDate = 'startDate',
    endDate = 'endDate',
    prDir = prDir,
    vars = 'tmin',
    res = 30,
    rastSuffix = 'tif',
	template = template,
	removeNAs = TRUE,
    verbose = TRUE,
	ID = TRUE, weights = TRUE, cells = TRUE, xy = TRUE
)

# monthly
parks$startDate <- c('2000-09-01', '1859-09-01', '2022-09-01', '2055-09-01')
parks$endDate <- c('2000-09-03', '1859-09-03', '2022-11-30', '2055-09-01')

mo <- prExtractAbsoluteMonthly(
    x = parks,
    startDate = 'startDate',
    endDate = 'endDate',
    prDir = prDir,
    vars = 'tmin',
    res = 30,
    rastSuffix = 'tif',
    template = template,
	removeNAs = TRUE,
    verbose = TRUE,
	ID = TRUE, weights = TRUE, cells = TRUE, xy = TRUE
)

# annual
parks$startDate <- c('2000-09-01', '1859-09-01', '2020-09-01', '2055-09-01')
parks$endDate <- c('2000-09-01', '1859-09-03', '2022-11-30', '2057-09-01')

yr <- prExtractAbsoluteAnnual(
    x = parks,
    startDate = 'startDate',
    endDate = 'endDate',
    prDir = prDir,
    vars = 'tmin',
    res = 30,
    rastSuffix = 'tif',
    annualDir = 'annual',
	removeNAs = TRUE,
	template = template,
    verbose = TRUE,
	ID = TRUE, weights = TRUE, cells = TRUE, xy = TRUE
)
