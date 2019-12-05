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
    E <- diag.spam(m)
    D <- diff(E, differences = d)
    #if(sum(V) == 0){
    #if(d == 0){
        # differences matrix D
        # E <- diag.spam(m)
        # D <- diff(E, differences = d)
        #D <- diag.spam(m)
        #return(D)
    #} else {
        # V = m-1 by m-1 matrix with 1/delta x on its diagonal.
        V <- diag.spam(m-d)
        diag(V) <- 1/diff(x, differences = d)
        # matrix multiplication VD
        D <- V %*% D
        #D <- V %*% diff(ddMat(x, d-1))
        return(D)
    #}
}

