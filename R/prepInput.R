

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

    output <- list(Y = data[sel, c("time", channel)],
                   channel = channel,
                   time = c(tstart, tstop))

    return(output)
}

NULL


