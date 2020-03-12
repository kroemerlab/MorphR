#' Sigmoidal image normalization
#'
#' Flatten image extrema by sigmoidal transformation
#'
#' @param x a 2D matrix
#' @param lambda smoothness of transformation
#' @param z0 center of sigmoidal transformation
#' @param scaled are input values scaled ?
#'
#' @return Normalized 2D matrix, with values ranging from 0 to 1
#'
#' @author Allan Sauvat, \email{allan.sauvat@gustaveroussy.fr}
#'
#' @export
#'

sigmoNormalize = function(x, lambda=2, z0=1.5, scaled=F){
  y = c(x);if(!scaled){y = scale(y)}
  y = 1/(1+exp(-lambda*(y-z0)))
  return(matrix(y,ncol=ncol(x)))
}