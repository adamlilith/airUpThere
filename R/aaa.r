# .onLoad <- function(lib, pkg) {
	
# }

.onAttach <- function(lib, pkg) {

	ver <- read.dcf(file = system.file('DESCRIPTION', package = pkg), fields = 'Version')
	packageStartupMessage(paste(pkg, ver))
	packageStartupMessage('It is advisable to update ', sQuote('airUrls'), ' with airUpdate() before using download functions.')
	
}
