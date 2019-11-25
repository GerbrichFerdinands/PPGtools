

#' prepInput
#'
#' .
#'
#' @param raw_data
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

prepInput <- function(raw_data, channel = c("Red", "Green", "Blue"),
                      tstart = 20, tstop = 40) {
    # select time frame (default 20-40 secs)
    t <- raw_data[, 1]
    sel <- t > tstart & t < tstop

    output <- data.frame(raw_data[sel, c("time", channel)])
    return(output)
}

NULL


