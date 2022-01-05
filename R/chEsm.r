#' Get names of CHELSA earth system models
#'
#' Return a vector of earth system model predictions available from CHELSA.
#'
#' @param cmip Number the Coupled Model Intercomparison Project (CMIP) future ratsers to download. Valid values are either \code{5} or \code{6}.
#' @return Character vector.
#' @examples
#' chEsm(5)
#' chEsm(6)
#' @export
chEsm <- function(cmip) {

	airData$chEsm[[paste0('cmip', cmip)]]

}
