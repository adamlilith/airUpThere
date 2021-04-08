source('E:/Ecology/Drive/R/airUpThere/R/wcDownload.r')
source('E:/Ecology/Drive/R/airUpThere/R/wcUtils.r')

load('E:/Ecology/Drive/R/airUpThere/data/wcEsm.rda')
load('E:/Ecology/Drive/R/airUpThere/data/wcVars.rda')

overwrite <- FALSE

res <- c(10, 5, 2.5)

wcDownload(ver=1.4, res=res, var=c('tmin', 'tmax', 'tmean', 'ppt', 'bio'), saveTo='E:/ecology/Climate/WorldClim/Version 1.4/ORIGINALS', overwrite=overwrite)

wcDownload(ver=2.1, res=res, var=c('tmin', 'tmax', 'tmean', 'ppt', 'srad', 'wind', 'vapr', 'bio', 'elev'), saveTo='E:/ecology/Climate/WorldClim/Version 2.1/ORIGINALS', overwrite=overwrite)

cmip5 <- wcEsm$cmip5$shortNames
# cmip6 <- wcEsm$cmip6$longNames

wcDownload(ver=1.4, res=res, esm=cmip5, var=c('tmin', 'tmax', 'ppt', 'bio'), year=c(2050, 2070), ghg=c(26, 45, 60, 85), saveTo='E:/ecology/Climate/WorldClim/Version 1.4/ORIGINALS', overwrite=overwrite)

wcDownload(ver=2.1, res=res, esm=cmip6, var=c('tmin', 'tmax', 'ppt', 'bio'), year=c(2030, 2050, 2070), ghg=c(126, 245, 370, 585), saveTo='E:/ecology/Climate/WorldClim/Version 2.1/ORIGINALS', overwrite=overwrite)

