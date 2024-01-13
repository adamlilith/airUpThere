library(terra)

data(nad83)

# Base folder with PRISM rasters
prDir <- 'F:/ecology/PRISM/working/an81'

# Example data (polygons)
parks <- airExample('parks')
crs(parks) <- nad83 # NB: More accurate than projecting from WGS84 to NAD83
parks$date <- c('2011-09-01', '1859-09-01', '2022-09-01', '2055-09-01')

# Template raster only required for extracting to lines or polygons
template <- rast('F:/ecology/PRISM/PRISM_us_dem_800m.tif')

# daily
dy <- prExtractRelativeDaily(
    x = parks,
    date = 'date',
    prDir = prDir,
    vars = 'tmin',
    res = 30,
    rastSuffix = 'tif',
    windowYears = 0,
    windowDays = 7,
	template = template,
    verbose = TRUE,
	ID = TRUE, cells = TRUE, xy = TRUE, weights = TRUE
)
 
# monthly
mo <- prExtractRelativeMonthly(
    x = parks,
    date = 'date',
    prDir = prDir,
    vars = 'tmin',
    res = 30,
    rastSuffix = 'tif',
    windowYears = 0,
    windowMonths = 5,
	template = template,
    verbose = TRUE,
	ID = TRUE, cells = TRUE, xy = TRUE, weights = TRUE
)
 
# annual
source('C:/Ecology/Drive/R/airUpThere/R/prExtractRelative.r')

yr <- prExtractRelativeAnnual(
    x = parks,
    date = 'date',
    prDir = prDir,
    vars = 'tmin',
    res = 30,
    rastSuffix = 'tif',
    annualDir = 'annual',
    windowYears = 3,
	template = template,
    verbose = TRUE,
	ID = TRUE, cells = TRUE, xy = TRUE, weights = TRUE
)
