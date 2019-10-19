#' prepInput
#'
#' .
#'
#' @param data
#' @param channel
#' @param tstart
#' @param tstop
#'
#' @return
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

prepInput <- function(data, channel = c("Red", "Green", "Blue"),
                      tstart = 20, tstop = 40) {
    # select time frame (default 20-40 secs)
    t <- data[, 1]
    sel <- t > tstart & t < tstop

    # select rows and columns
    output <- data[sel, c("time", channel)]
}

