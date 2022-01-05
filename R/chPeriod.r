#' Get time periods for CHELSA future projections
#'
#' Return a data frame with time periods for CHELSA future projections.
#'
#' @param cmip Number the Coupled Model Intercomparison Project (CMIP) future ratsers to download. Valid values are either \code{5} or \code{6}.
#' @return Character vector.
#' @examples
#' chPeriod(5)
#' chPeriod(6)
#' @export
chPeriod <- function(cmip) {

	airData$chPeriod[[paste0('cmip', cmip)]]

}
