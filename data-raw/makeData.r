### MAKE DATA FOR airUpThere
###
### Whatever data is created here *must* be assigned globally in airUpdate(), too.
###
### source('C:/Ecology/Drive/R/airUpThere/data-raw/makeData.r')
### source('E:/Ecology/Drive/R/airUpThere/data-raw/makeData.r')

drive <- 'C:'
# drive <- 'E:'

airData <- list()

############
### URLs ###
############

	airUrls <- read.csv(paste0(drive, '/Ecology/Drive/R/airUpThere/data-raw/airUrls.csv'))
	airData <- c(airData, list(airUrls=airUrls))

######################
### variable names ###
######################

	airVars <- read.csv(paste0(drive, '/Ecology/Drive/R/airUpThere/data-raw/airVars.csv'))
	airData <- c(airData, list(airVars=airVars))

#####################
### GHG scenarios ###
#####################

	ghgs <- list(
		cmip5 = data.frame(
			rcp = c('2.6', '4.5', '6.0', '8.5')
		),
		cmip6 = data.frame(
			ssp = c(126, 245, 370, 585)
		)
	)
	
	airData <- c(airData, list(ghgs=ghgs))

#############################
### WorldClim resolutions ###
#############################

	wcRes <- read.csv(paste0(drive, '/Ecology/Drive/R/airUpThere/data-raw/wcRes.csv'))
	airData <- c(airData, list(wcRes=wcRes))

########################
### CHELSA ESM names ###
########################

	cmip5 <- read.csv(paste0(drive, '/Ecology/Drive/R/airUpThere/data-raw/chEsm_cmip5.csv'))
	cmip6 <- read.csv(paste0(drive, '/Ecology/Drive/R/airUpThere/data-raw/chEsm_cmip6.csv'))
	chEsm <- list(
		cmip5 = cmip5$esm,
		cmip6 = cmip6$esm
	)
	
	airData <- c(airData, list(chEsm=chEsm))

##########################
### CHELSA ESM periods ###
##########################

	cmip5 <- read.csv(paste0(drive, '/Ecology/Drive/R/airUpThere/data-raw/chPeriods_cmip5.csv'))
	cmip6 <- read.csv(paste0(drive, '/Ecology/Drive/R/airUpThere/data-raw/chPeriods_cmip6.csv'))
	chPeriod <- list(
		cmip5 = cmip5,
		cmip6 = cmip6
	)
	
	airData <- c(airData, list(chPeriod=chPeriod))

###########################
### WorldClim ESM names ###
###########################

	wcEsm <- list(
		cmip5 = data.frame(
			long = c(
				'ACCESS1-0',
				'BCC-CSM1-1',
				'CCSM4',
				'CESM1-CAM5-1-FV2',
				'CNRM-CM5',
				'GFDL-CM3',
				'GFDL-ESM2G',
				'GISS-E2-R',
				'HadGEM2-AO',
				'HadGEM2-CC',
				'HadGEM2-ES',
				'INMCM4',
				'IPSL-CM5A-LR',
				'MIROC-ESM-CHEM',
				'MIROC-ESM',
				'MIROC5',
				'MPI-ESM-LR',
				'MRI-CGCM3',
				'NorESM1-M'
			),
			short = c(
				'AC',
				'BC',
				'CC',
				'CE',
				'CN',
				'GF',
				'GD',
				'GS',
				'HD',
				'HG',
				'HE',
				'IN',
				'IP',
				'MI',
				'MR',
				'MC',
				'MP',
				'MG',
				'NO'
			)
		),
		cmip6 = data.frame(
			long=c(
				'BCC-CSM2-MR',
				'CNRM-CM6-1',
				'CNRM-ESM2-1',
				'CanESM5',
				'GFDL-ESM4',
				'IPSL-CM6A-LR',
				'MIROC-ES2L',
				'MIROC6',
				'MRI-ESM2-0'
			),
			short=c(
				'BC',
				'CC',
				'CE',
				'CA',
				'GF',
				'IP',
				'MR',
				'MC',
				'ME'
			)
		)
	)

	airData <- c(airData, list(wcEsm=wcEsm))
	
########################	
### WorldClim period ###
########################	
	
	wcPeriod <- list(
		cmip5 = data.frame(
			long = c(2050, 2070),
			short = c(50, 70)
		),
		cmip6 = data.frame(
			long = c('2021-2040', '2041-2060', '2061-2080', '2081-2100'),
			short = c(2030, 2050, 2070, 2090)
		),
		decadal = data.frame(
			long = c('1960-1969', '1970-1979', '1980-1989', '1990-1999', '2000-2009', '2010-2018'),
			short = c(1960, 1970, 1980, 1990, 2000, 2010)
		)
	)

	airData <- c(airData, list(wcPeriod=wcPeriod))

##########################
### create update file ###
##########################	

	save(airData, file=paste0(drive, '/Ecology/Drive/R/airUpThere/data/airData.rda'))
	save(airData, file=paste0(drive, '/Ecology/Drive/R/airUpThere_updates/airData.rda'))
