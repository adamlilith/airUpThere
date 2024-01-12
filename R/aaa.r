# .onLoad <- function(lib, pkg) {
	
# }

.onAttach <- function(lib, pkg) {

	ver <- read.dcf(file = system.file('DESCRIPTION', package = pkg), fields = 'Version')
	packageStartupMessage(paste(pkg, ver))
	packageStartupMessage('It is recommended to update ', sQuote('airUrls'), ' before using download functions with function airUpdate().')
	
}
