#' ddMat
#'
#'
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

    if(d == 0){
        D <- diag.spam(m)
    } else {
        # delta x
        dx <- diff(x, d)
        # V = m-1 by m-1 matrix with 1/delta x on its diagonal.
        V <- diag.spam(1/dx)
        #diag(V) <- 1/dx
        # todo make recursive
        #D <- diff(diag.spam(m), differences = d)
        D <- V %*% diff(ddMat(x = x, d = d-1))
    }

    # todo(check for other order functions: diff function maybe not suitable?)


    return(D)
}

