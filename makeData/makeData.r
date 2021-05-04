library(tibble)
drive <- 'E:'

############
### URLs ###
############

	airUrls <- read.csv(paste0(drive, '/Ecology/Drive/R/airUpThere/makeData/airUrls.csv'))
	save(airUrls, file=paste0(drive, '/Ecology/Drive/R/airUpThere/data/airUrls.rda'))
	save(airUrls, file=paste0(drive, '/Ecology/Drive/R/airUpThere_updates/airUrls.rda'))

######################
### variable names ###
######################

	varNames <- read.csv(paste0(drive, '/Ecology/Drive/R/airUpThere/makeData/varNames.csv'))
	save(varNames, file=paste0(drive, '/Ecology/Drive/R/airUpThere/data/varNames.rda'))

#####################
### GHG scenarios ###
#####################

	ghgNames <- list(
		cmip5 = data.frame(
			rcp = c('2.6', '4.5', '6.0', '8.5')
		),
		cmip6 = data.frame(
			ssp = c(126, 245, 370, 585)
		)
	)
	
	save(ghgNames, file=paste0(drive, '/Ecology/Drive/R/airUpThere/data/ghgNames.rda'))
	

#############################
### WorldClim resolutions ###
#############################

	wcRes <- read.csv(paste0(drive, '/Ecology/Drive/R/airUpThere/makeData/wcRes.csv'))
	save(wcRes, file=paste0(drive, '/Ecology/Drive/R/airUpThere/data/wcRes.rda'))

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

	save(wcEsm, file=paste0(drive, '/Ecology/Drive/R/airUpThere/data/wcEsm.rda'))
	