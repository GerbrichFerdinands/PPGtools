#' PPGtools
#'
#' This package provides a toolbox for analyzing noisy photoplethysmography (PPG)
#' raw_signal. The package contains functions for filtering, visualization and feature
#' extraction from noisy PPG raw_signal. The package contains example PPG raw_signal
#' (\code{\link{rec}}).
#'
#' @section Core function:
#' The core function can be found at \code{\link{smoothWE}}
#'
#' @author Gerbrich Ferdinands (\email{gerbrichferdinands@@gmail.com})
#'
#' Maintainer: Gerbrich Ferdinands (\email{gerbrichferdinands@@gmail.com})
#'
#' @docType package
#' @name PPGtools
#' @keywords PPG
#'
#' @import spam
#' @import ggplot2
#' @import dplyr
#' @importFrom magrittr "%>%"
NULL

#-------------------------------------------------------------------------------

#' smoothWE
#'
#' This function ..
#'
#' @param raw_signal a list (resulting from \code{\link{prepInput}}), containing:
#' \itemize{
#' \item Y - a 2 column matrix containing the time steps in the signal ("time") and the noisy series y to be smoothed;
#' \item channel - Red, Green or Blue;
#' \item time - the start and end point from the noisy series.
#' }
#' @param lambda tuning parameter(s), numeric or ?:1 matrix.
#' @param d order of differences (d = 1, 2, 3, ...)
#' @param uni logical indicator of if we equal timesteps are assumed.
#'
#' @return a list containing
#' \itemize{
#' \item{z}{- matrix of smooth series z (fit to y)}
#' \item{lambda}
#' \item{difference}
#' \item{uni}
#' \item{raw_signal}
#' }
#'
#' @examples
#'
#'
#' @export
smoothWE <- function(raw_signal, lambda, d = 2, uni = TRUE){

    # check if input is correct
    if (is.null(dim(lambda))) lambda <- matrix(lambda)
    if (ncol(lambda) != 1) stop("dimensions of 'lambda' are incorrect \n")

    y <- raw_signal[,2]
    t <- raw_signal$time


    # check distribution of timesteps
    steps <- diff(t)
    tol <- 0.03
    if(abs(max(steps) - min(steps)) > tol) warning("Timesteps are unequal")

  

    # E = identity matrix
    m <- length(y)
    E <- diag.spam(m)

    # D = differences matrix, differences indicating the order of difference
    if(uni){
        D <- diff(E, differences = d)
    } else {
        D <- ddMat(x = t, d = d)
    }

    P <- t(D) %*% D

    # (E + lambda D' D)z = y, so:
    # z = solve(E = lambda D' D, y)

    # smooth raw raw_signal y with given lambda tuning parameter(s)
    z <- apply(lambda, 1, function(x) solve(E + x * P, y))
    colnames(z) <- paste0('l', 1:length(lambda))
    # prepare output
    return(z)
}
NULL
# todo: z in sparse notation ( C = chol())
# todo(optimal smoothings)
