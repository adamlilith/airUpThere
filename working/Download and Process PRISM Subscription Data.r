### DOWNLOAD PRISM SUBSCRIPTION DATA
### Adam B. Smith | 2020-12 | Missouri Botanical Garden | adam.smith@mobot.org
###
### source('D:/ecology/Climate/PRISM/!code/Download and Process PRISM Subscription Data.r')
### source('H:/Global Change Program/Research/PRISM/!code/Download and Process PRISM Subscription Data.r')
### source('I:/ecology/Climate/PRISM/!code/Download and Process PRISM Subscription Data.r')
###
### CONTENTS
### setup ###
### download, convert, and copy DAILY ###
### download, convert, and copy MONTHLY ###

#############
### setup ###
#############

	memory.limit(memory.limit() * 2^30)
	rm(list=ls())
	options(keep.source=FALSE) # manage memory
	gc()
	print('')
	print(date())
	
	library(lubridate)
	library(terra)
	
	### PRISM library
		
		library(prism) # modified to allow user to specify URL
		
		# if (!require(prism)) remotes::install_github('ropensci/prism')

		# files <- list.files('E:/Ecology/Drive/R/prism/R', full.names=TRUE)
		# files <- files[grepl(tolower(files), pattern='.r')]
		# for (file in files) source(file)
		
# cat('#########################################\n')
# cat('### download, convert, and copy DAILY ###\n')
# cat('#########################################\n'); flush.console()

	# ### This script checks to see 1) if files in the "subscription" directory exist, and if so, 2) if files on the server are newer than files in the "subscription" directory. If the subscription directory does not have the specific files or the files are older than those on the server, a new set is downloaded, converted to a GeoTIFF, and saved in the "working" directory. A "release date web service" file is saved along with the BIL version of the raster in the subscription folder.

	# ### real deal
	
		# downloadDir <- 'I:/ecology/Climate/PRISM/subscription/an81/'
		# workDir <- 'I:/ecology/Climate/PRISM/working/an81/'
	
		# # downloadDir <- 'H:/Global Change Program/Research/PRISM/subscription/an81/'
		# # workDir <- 'H:/Global Change Program/Research/PRISM/working/an81/'
	
		# vars <- c('tmin', 'tmax', 'tmean', 'ppt', 'tdmean', 'vpdmin', 'vpdmax')
		# # vars <- c('tdmean', 'vpdmin', 'vpdmax')
		# # vars <- c('ppt')
		
		# # startDate <- '1981-01-01'
		# # endDate <- '2019-12-31'
		
		# startDate <- '2021-01-01'
		# endDate <- '2021-02-28'
		
		# # startDate <- '1981-01-01'
		# # endDate <- '1981-01-02'
		
	# # # ### trial
	
		# # downloadDir <- 'D:/ecology/Climate/PRISM/subscription/an81/'
		# # workDir <- 'D:/ecology/Climate/PRISM/working/an81/'
	
		# # # vars <- c('tmin', 'tmax', 'tmean', 'ppt', 'tdmean', 'vpdmin', 'vpdmax')
		# # vars <- c('tmin')
		
		# # startDate <- '1981-01-01'
		# # endDate <- '1981-01-02'

	# ### general (do not comment this out)
		
		# dates <- seq(as.POSIXct(startDate), as.POSIXct(endDate), by='1 days')
		# dates <- as.character(dates)
		# dates <- substr(dates, 1, 10)

		# service <- 'http://services.nacse.org/prism/data/subscription/800m'

	# ### download function
	
	# downloadRaster <- function(saveToDir, workDir, var, date, service) {
	
		# # download
		# prism_set_dl_dir(saveToDir)
	
		# x <- get_prism_dailys(
		  # type = var, 
		  # dates = date, 
		  # keepZip = FALSE,
		  # service = service
		# )

		# cat(' | converting and copying'); flush.console()

		# # convert and copy
		# r <- rast(paste0(saveToDir, '/data_', var, '_', yr, mo, dy, '/prism_', var, '_us_30s_', yr, mo, dy, '.bil'))
		
		# dir.create(paste0(workDir, '/', var, '/daily/', yr), recursive=TRUE, showWarnings=FALSE)
		
		# name <- paste0('prism_', var, '_us_30s_', yr, mo, dy)
		# fileName <- paste0(workDir, '/', var, '/daily/', yr, '/prism_', var, '_us_30s_', yr, mo, dy, '.tif')
		# wopt <- list(filetype='GTIff', datatype='FLT4S', memfrac=0.7, names=name)
		# writeRaster(r, fileName, overwrite=TRUE, wopt=wopt)

		# file.copy(paste0(saveToDir, '/data_', var, '_', yr, mo, dy, '/prism_', var, '_us_30s_', yr, mo, dy, '.info.txt'), paste0(workDir, '/', var, '/daily/', yr, '/prism_', var, '_us_30s_', yr, mo, dy, '.info.txt'), overwrite=TRUE)
		
	# }
	
	# ### download DAILY
	# for (var in vars) {

		# for (date in dates) {

			# cat(date(), 'DAILY', var, date); flush.console()

			# yr <- substr(date, 1, 4)
			# mo <- substr(date, 6, 7)
			# dy <- substr(date, 9, 10)

			# # destination folder
			# saveToDir <- paste0(downloadDir, '/daily/', var, '/', yr)
			# dir.create(saveToDir, recursive=TRUE, showWarnings=FALSE)

			# # get create date of version on server
			# url <- paste0('http://services.nacse.org/prism/data/subscription/800m/releaseDate/', var, '/', yr, mo, dy)
			# tempTo <- 'C:/ecology/!Scratch/temp.tab'
			# utils::download.file(url, destfile=tempTo, mode='wb', quiet=TRUE)

			# verOnServer <- read.delim(tempTo, header=FALSE)
			# names(verOnServer) <- c('rastDate', 'createDate', 'var', 'version', 'service')
			# serverCreateDate <- ymd(verOnServer$createDate)

			# # does a "version" file exist in "subscription"? need new version?
			# if (
				# file.exists(paste0(saveToDir, '/data_', var, '_', yr, mo, dy, '/prism_', var, '_us_30s_', yr, mo, dy, '.version.txt'))
			# ) {
			
				# verInSubscrip <- read.delim(paste0(saveToDir, '/data_', var, '_', yr, mo, dy, '/prism_', var, '_us_30s_', yr, mo, dy, '.version.txt'))
				# names(verInSubscrip) <- c('rastDate', 'createDate', 'var', 'version', 'service')
				# subscripCreateDate <- ymd(verInSubscrip$createDate)

				# # do we need to update?
				# if (is.na(serverCreateDate) | serverCreateDate > subscripCreateDate) {
					
					# cat(' | updating from server'); flush.console()
					
					# # remove old raster in subscription folder
					# suffixes <- c('bil', 'bil.aux.xml', 'hdr', 'info.txt', 'prj', 'stn.csv', 'stx', 'xml')
					# for (suffix in suffixes) {
					
						# file <- paste0(paste0(saveToDir, '/data_', var, '_', yr, mo, dy, '/prism_', var, '_us_30s_', yr, mo, dy, '.', suffix))
						# file.remove(file)
						
					# }
					
					# # download raster
					# downloadRaster(saveToDir=saveToDir, workDir=workDir, var=var, date=date, service=service)

					# # save new version information
					# write.table(verOnServer, paste0(saveToDir, '/data_', var, '_', yr, mo, dy, '/prism_', var, '_us_30s_', yr, mo, dy, '.version.txt'), sep='\t')
					
				# } else {
				
					# cat(' | already have latest version'); flush.console()
					
				# }
				
			# # fresh download
			# } else {
			
				# cat(' | downloading from server'); flush.console()
			
				# # download raster
				# downloadRaster(saveToDir=saveToDir, workDir=workDir, var=var, date=date, service=service)
					
				# # save new version information
				# write.table(verOnServer, paste0(saveToDir, '/data_', var, '_', yr, mo, dy, '/prism_', var, '_us_30s_', yr, mo, dy, '.version.txt'), sep='\t')

			# }
			
			# cat('\n'); flush.console()
			
		# }
		
	# }

cat('###########################################\n')
cat('### download, convert, and copy MONTHLY ###\n')
cat('###########################################\n'); flush.console()

	### This script checks to see 1) if files in the "subscription" directory exist, and if so, 2) if files on the server are newer than files in the "subscription" directory. If the subscription directory does not have the specific files or the files are older than those on the server, a new set is downloaded, converted to a GeoTIFF, and saved in the "working" directory. A "release date web service" file is saved along with the BIL version of the raster in the subscription folder.

	### real deal
	
		downloadDir <- 'I:/ecology/Climate/PRISM/subscription/an81/'
		workDir <- 'I:/ecology/Climate/PRISM/working/an81/'
	
		# downloadDir <- 'H:/Global Change Program/Research/PRISM/subscription/an81m/'
		# workDir <- 'H:/Global Change Program/Research/PRISM/working/an81m/'
	
		vars <- c('tmin', 'tmax', 'tmean', 'ppt', 'tdmean', 'vpdmin', 'vpdmax')
		# vars <- c('ppt')
		
		startDate <- '1981-01-01'
		endDate <- '2020-12-31'
		
	# # ### trial
	
		# downloadDir <- 'D:/ecology/Climate/PRISM/subscription/an81m/'
		# workDir <- 'D:/ecology/Climate/PRISM/working/an81m/'
	
		# # vars <- c('tmin', 'tmax', 'tmean', 'ppt', 'tdmean', 'vpdmin', 'vpdmax')
		# vars <- c('tmin')
		
		# startDate <- '1981-01-01'
		# endDate <- '1981-01-02'

	### general (do not comment this out)
		
		dates <- seq(as.POSIXct(startDate), as.POSIXct(endDate), by='1 month')
		dates <- as.character(dates)
		dates <- substr(dates, 1, 10)

		service <- 'http://services.nacse.org/prism/data/subscription/800m'

	### download function
	
	downloadRasterMonthly <- function(saveToDir, workDir, var, yr, mo, service) {
	
		# download
		prism_set_dl_dir(saveToDir)
	
		yrNum <- as.integer(yr)
		moNum <- as.integer(mo)
	
		x <- get_prism_monthlys(
		  type = var, 
		  years = yrNum,
		  mon = moNum, 
		  keepZip = FALSE,
		  service = service
		)

		cat(' | converting and copying'); flush.console()

		# convert and copy
		r <- rast(paste0(saveToDir, '/data_', var, '_', yr, mo, '/prism_', var, '_us_30s_', yr, mo, '.bil'))
		dir.create(paste0(workDir, '/', var, '/monthly/', yr), recursive=TRUE, showWarnings=FALSE)
		name <- paste0('prism_', var, '_us_30s_', yr, mo)
		fileName <- paste0(workDir, '/', var, '/monthly/', yr, '/prism_', var, '_us_30s_', yr, mo, '.tif')
		wopt <- list(filetype='GTIff', datatype='FLT4S', memfrac=0.7, names=name)
		writeRaster(r, fileName, overwrite=TRUE, wopt=wopt)
		file.copy(paste0(saveToDir, '/data_', var, '_', yr, mo, '/prism_', var, '_us_30s_', yr, mo, '.info.txt'), paste0(workDir, '/', var, '/monthly/', yr, '/prism_', var, '_us_30s_', yr, mo, '.info.txt'), overwrite=TRUE)

	}
	
	### download MONTHLY
	for (var in vars) {

		for (date in dates) {

			cat(date(), 'MONTHLY', var, date); flush.console()

			yr <- substr(date, 1, 4)
			mo <- substr(date, 6, 7)

			# destination folder
			saveToDir <- paste0(downloadDir, '/monthly/', var, '/', yr)
			dir.create(saveToDir, recursive=TRUE, showWarnings=FALSE)

			# get create date of version on server
			url <- paste0('http://services.nacse.org/prism/data/subscription/800m/releaseDate/', var, '/', yr, mo)
			tempTo <- 'C:/ecology/!Scratch/temp.tab'
			utils::download.file(url, destfile=tempTo, mode='wb', quiet=TRUE)

			verOnServer <- read.delim(tempTo, header=FALSE)
			names(verOnServer) <- c('rastDate', 'createDate', 'var', 'version', 'service')
			serverCreateDate <- ymd(verOnServer$createDate)

			# does a "version" file exist in "subscription"? need new version?
			if (
				file.exists(paste0(saveToDir, '/data_', var, '_', yr, mo, '/prism_', var, '_us_30s_', yr, mo, '.version.txt'))
			) {
			
				verInSubscrip <- read.delim(paste0(saveToDir, '/data_', var, '_', yr, mo, '/prism_', var, '_us_30s_', yr, mo, '.version.txt'))
				names(verInSubscrip) <- c('rastDate', 'createDate', 'var', 'version', 'service')
				subscripCreateDate <- ymd(verInSubscrip$createDate)

				# do we need to update?
				if (is.na(serverCreateDate) | serverCreateDate > subscripCreateDate) {
					
					cat(' | updating from server'); flush.console()
					
					# remove old raster in subscription folder
					suffixes <- c('bil', 'bil.aux.xml', 'hdr', 'info.txt', 'prj', 'stn.csv', 'stx', 'xml')
					for (suffix in suffixes) {
					
						file <- paste0(paste0(saveToDir, '/data_', var, '_', yr, mo, dy, '/prism_', var, '_us_30s_', yr, mo, '.', suffix))
						file.remove(file)
						
					}
					
					# download raster
					downloadRasterMonthly(saveToDir=saveToDir, workDir=workDir, var=var, mo=mo, service=service)

					# save new version information
					write.table(verOnServer, paste0(saveToDir, '/data_', var, '_', yr, mo, '/prism_', var, '_us_30s_', yr, mo, '.version.txt'), sep='\t')
					
				} else {
				
					cat(' | already have latest version'); flush.console()
					
				}
				
			# fresh download
			} else {
			
				cat(' | downloading from server'); flush.console()
			
				# download raster
				downloadRasterMonthly(saveToDir=saveToDir, workDir=workDir, var=var, yr=yr, mo=mo, service=service)
					
				# save new version information
				write.table(verOnServer, paste0(saveToDir, '/data_', var, '_', yr, mo, '/prism_', var, '_us_30s_', yr, mo, '.version.txt'), sep='\t')

			}
			
			cat('\n'); flush.console()
			
		}
		
	}

cat('&&&&&&&&&&&&&&&\n')
cat('&&& DONE!!! &&&\n')
cat('&&&&&&&&&&&&&&&\n')
flush.console()
