#' ddMat
#'
#' matrix D, that constructs divided differences of order d oof y when Dy.
#'
#' @param x time steps
#' @param d order of differences
#' @return the matrix D, divided differences. D*Y = divided differences of order d.
#'
#' @details
#'
#' @section Warning:
#' You can add sections if you like
#'
#'
#' @examples

#'
#' @export

ddMat <- function(x, d){
    m <- length(x)
    # differences matrix D
    E <- diag.spam(m)
    D <- diff(E, differences = d)

    #if(nuni){
        # delta x
        dx <- diff(x, differences = d)
        # V = m-1 by m-1 matrix with 1/delta x on its diagonal.
        V <- diag.spam(1/dx)
        # matrix multiplication VD
        D <- V%*%D
    #}

    return(D)
}

