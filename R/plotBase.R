#' plotBase
#'
#' This function returns lambda input
#'
#' @param pdat
#' @return foundation of a plot
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

plotBase <- function(pdat){
    # todo: add input characteristics
    p <- ggplot(pdat, aes(x = time)) +
        labs(title = "Smoothing raw PPG data, d = ...",
             x = "Time (s)",
             y = "signal") +
        theme_classic()

    return(p)
}

# test branch editeing

