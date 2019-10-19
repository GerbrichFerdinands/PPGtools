#' PPGtools
#'
#' This package extract pulse waves from noisy PPG recordings
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
#' @importFrom magrittr "%>%"
NULL


#' smoothWE
#'
#' This function ..
#'
#' @param x time steps (column vector)
#' @param y noisy series to be smoothed (column vector)
#' @param lambda tuning parameter(s), numeric or ?:1 matrix.
#' @param d order of differences (d = 1, 2, 3, ...)
#'
#' @return a smooth series z (fit to y)
#'
#' @details .
#'
#' @section Warning:
#' You can add sections if you like
#'
#' @seealso \code{\link{Reduce}}
#'
#' @examples

#'
#' \dontrun{fancySum("A", list(c=51))}
#'
#' @export
smoothWE <- function(x, y, lambda, d = 2){
    # check if input is correct
    if (is.null(dim(lambda))) lambda <- matrix(lambda)
    if (ncol(lambda) != 1) stop("dimensions of 'lambda' are incorrect \n")

    m <- length(x)
    # E = identity matrix
    E <- diag.spam(m)
    # D = differences matrix, differences indicating the order of difference

    #D <- ddMat(E, differences = d)
    D <- diff(E, differences = d)
    P <- t(D) %*% D
    # (E + lambda D' D)z = y, so:
    z <- apply(lambda, 1, function(x) solve(E + x * P, y))
    names(z) <- names(lambda)
    return(z)
}

# todo: z in sparse notation ( C = chol())
# todo: unequal timesteps WE
# todo(optimal smoothings)
# done(column names for z)

