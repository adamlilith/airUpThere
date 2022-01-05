### Download WorldClim
### Adam B. Smith | 2021-04 | Missouri Botanical Garden | adam.smith@mobot.org
###
### source('E:/Ecology/Drive/R/airUpThere/test/testing.r')
###
### CONTENTS ###
### setup ###
### download & unpack version 1.4 HISTORICAL ###
### download & unpack version 1.4 FUTURE ###
### download & unpack version 2.1 HISTORICAL ###
### download & unpack version 2.1 FUTURE ###
### download & unpack version 2.1 DECADAL ###
### download & unpack version 2.1 ELEVATION ###

#############
### setup ###
#############

	rm(list=ls())

	dataDrive <- 'E:'

	# library(airUpThere)
	library(omnibus)
	
	srcs <- list.files('E:/ecology/Drive/R/airUpThere/R', pattern='.r', full.names=TRUE)
	for (src in srcs) source(src)
	
	srcs <- list.files('E:/ecology/Drive/R/airUpThere/data', pattern='.rda', full.names=TRUE)
	for (src in srcs) load(src)
	
	# data(varNames)
	# data(wcEsm)

	saveTo <- unpackTo <- paste0(dataDrive, '/Ecology/!Scratch/WorldClim')

# say('##########################################################')
# say('### download & unpack version WORLDCLIM 1.4 HISTORICAL ###')
# say('##########################################################')

	ver <- 1.4
	
	res <- c(10)
	vars <- 'tmin'

	ok <- wcDownloadHist(saveTo, ver=ver, res=res, vars=vars, overwrite=FALSE, verbose=TRUE)
	ok
	
	ok <- wcUnpackHist(saveTo, unpackTo, ver=ver, res=res, vars=vars, overwrite=FALSE, verbose=TRUE)
	ok
	
say('##############################################')
say('### download & unpack WORLDCLIM 1.4 FUTURE ###')
say('##############################################')

	ver <- 1.4
	
	res <- c(10)
	ghg <- c(26)
	period <- c(2050)
	vars <- 'tmin'
	esm <- 'BC'

	ok <- wcDownloadFut(saveTo, ver=ver, res=res, vars=vars, ghg=ghg, period=period, esm=esm, overwrite=FALSE, verbose=TRUE)
	ok
	# ok <- wcUnpackFut(saveTo, unpackTo, ver=ver, res=res, vars=vars, ghg=ghg, period=period, esm=esm, overwrite=FALSE, verbose=TRUE)
		


