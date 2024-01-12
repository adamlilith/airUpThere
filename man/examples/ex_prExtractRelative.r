x <- data.frame(
 	long=rep(-97.66, 5),
 	lat=rep(38.37, 5),
 	date=c('2015-12-31', '1981-01-05', '2020-12-01',
 	'2019-01-05', '1895-05-01')
)
  
# daily
prDir <- 'F:/ecology/PRISM/working/an81'
y <- prExtractRelativeDaily(
    x,
    date = 'date',
    longLat = c('long', 'lat'),
    prDir = prDir,
    vars = 'tmin',
    res = 30,
    rastSuffix = 'tif',
    windowYears = 0,
    windowDays = 7,
    verbose = TRUE
)
 
# monthly
prDir <- 'F:/ecology/PRISM/working/an81'
y <- prExtractRelativeMonthly(
 	x,
 	date = 'date',
 	longLat = c('long', 'lat'),
 	prDir = prDir,
 	vars = 'tmin',
		res = 30,
 	rastSuffix = 'tif',
 	windowYears = 0,
 	windowMonths = 5,
 	verbose = TRUE
)
 
# annual
prDir <- 'F:/ecology/PRISM/working/an81'
y <- prExtractRelativeAnnual(
 	x,
 	date = 'date',
		longLat = c('long', 'lat'),
		prDir = prDir,
		vars = 'tmin',
		res = 30,
		rastSuffix = 'tif',
		annualDir = 'monthly',
		windowYears = 3,
		verbose = TRUE
)
