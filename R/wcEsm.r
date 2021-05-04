#' Names of earth system models with data products available from WorldClim
#'
#' Names of earth system models (global circulation models) with data products available from WorldClim.
#' @param ver WorldClim version. Valid values include \code{1.4} or \code{2.1}.
#' @return Data frame.
#' @examples
#' 
#' wcEsm(1.4)
#' wcEsm(2.1)
#'
#' @export
wcEsm <- function(ver) {

	wcCheckVer_internal(ver)

	if (ver == 1.4) {

		out <- data.frame(
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
		)
		
	} else if (ver == 2.1) {
	
		out <- data.frame(
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
		
	}
	
	out

}
