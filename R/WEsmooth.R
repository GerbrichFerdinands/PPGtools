#' PPGtools
#'
#' This package provides a toolbox for analyzing noisy photoplethysmography (PPG)
#' data. The package contains functions for filtering, visualization and feature
#' extraction from noisy PPG data. The package contains example PPG data
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
#' @import spam
#' @import ggplot2
#' @import gridExtra
#' @importFrom magrittr "%>%"
NULL

#-------------------------------------------------------------------------------

#' smoothWE
#'
#' This function ..
#'
#' @param data a list (resulting from \code{\link{prepInput}}), containing:
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
#' \item{data}
#' }
#'
#' @examples
#'
#'
#' @export
smoothWE <- function(data, lambda, d = 2, uni = TRUE){
# maybe default for t = 1:length(y) (is this possible in r function input?)
    # check if input is correct
    if (is.null(dim(lambda))) lambda <- matrix(lambda)
    if (ncol(lambda) != 1) stop("dimensions of 'lambda' are incorrect \n")

    # extracting input from data list
    channel <- data$channel
    y <- data$Y[,channel]
    t <- data$Y[,"time"]


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

    # smooth raw data y with given lambda tuning parameter(s)
    z <- apply(lambda, 1, function(x) solve(E + x * P, y))
    names(z) <- names(lambda)

    # prepare output
    output <- list(z = z, lambda = lambda, difference = d, uni = uni)
    output <- c(data, output)

    return(output)
}
NULL
# todo: z in sparse notation ( C = chol())
# todo(optimal smoothings)

