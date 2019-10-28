
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

NULL

################################################################################

#' plotY
#'
#' This function returns lambda input
#'
#' @param p plot
#' @param pdat plotdata
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

plotY <- function(p, pdat){
    p <- p + geom_point(aes(y=y), size = 0.5)
    return(p)
}

NULL

################################################################################

#' plotZ
#'
#' This function plots the smoothed values z.
#'
#' @param p
#' @return pdat
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

plotZ <- function(p, pdat){

    # todo: use apply
    l1 <- geom_line(aes(y = l1, colour = "l1"), size = 0.5)
    l2 <- geom_line(aes(y = l2, colour = "l2"), size = 0.5)
    l3 <-  geom_line(aes(y = l3, colour = "l3"), size = 0.5)
    l4 <-  geom_line(aes(y = l4, colour = "l4"), size = 0.5)

    view <-
        p + scale_colour_manual("",
                                breaks = colnames(z),
                                values = c("l1" = "green", "l2" = "orange",
                                           "l3" = "purple", "l4" = "blue")) +
        scale_fill_discrete(name = "lambda",
                            labels = names(lambda))

    output <- list(base = view, l1=l1, l2=l2, l3=l3, l4=l4)
    return(output)
}

NULL

################################################################################
#' plotLambda
#'
#' This function plots the smoothed values z.
#'
#' @param time
#' @param y
#' @param z
#'
#' @return plot
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
plotLambda <- function(time, y, z){ # todo change output WEsmooth
    pdat <- cbind(time, y, z)
    pdat <- data.frame(pdat)

    p <- plotBase(pdat)
    p1 <- plotY(p, pdat)
    p2 <- plotZ(p1, pdat)

    with(p2,
         base + l1 + l2 + l3 + l4)
    # todo make apply
}

NULL
