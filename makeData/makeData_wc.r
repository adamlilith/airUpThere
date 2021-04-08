###########################
### WorldClim ESM names ###
###########################

	wcEsm <- list(
		cmip5 = list(
			longNames = c(
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
			shortNames = c(
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
		cmip6 = list(
			longNames=c(
				'BCC-CSM2-MR',
				'CNRM-CM6-1',
				'CNRM-ESM2-1',
				'CanESM5',
				'GFDL-ESM4',
				'IPSL-CM6A-LR',
				'MIROC-ES2L',
				'MIROC6',
				'MRI-ESM2-0'
			)
		)
	)

	save(wcEsm, file='E:/Ecology/Drive/R/airUpThere/data/wcEsm.rda')

################################
### WorldClim variable names ###
################################

	wcVars <- read.csv('E:/Ecology/Drive/R/airUpThere/makeData/wcVars.csv')
	save(wcVars, file='E:/Ecology/Drive/R/airUpThere/data/wcVars.rda')
	
