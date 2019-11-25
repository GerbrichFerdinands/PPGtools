
#' plotBase
#'
#' This function creates a blank canvas for plotting a PPG signal.
#'
#' @param pdat a list containing data, and other plot characteristics
#' @return a ggplot object
#'
#' @details
#'
#'
#' @examples

#'
#' @export

plotBase <- function(pdat){
    # todo: add input characteristics
    p <- ggplot(pdat, aes(x = time)) +
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
#'
#' @return p
#'
#' @details
#'
#' @section
#'
#' @examples

#'
#' @export

plotY <- function(p, pdat){

    p <- p + geom_point(aes(y=pdat$Green), size = 0.5)
    return(p)
}

NULL

################################################################################

#' plotZ
#'
#' This function plots the smoothed values z.
#'
#' @param p
#' @param pdat
#'
#' @return list of plots
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
    l1 <- geom_line(aes(y = pdat$lambda_1, colour = "l1"), size = 0.5)
    l2 <- geom_line(aes(y = pdat$lambda_2, colour = "l2"), size = 0.5)
    l3 <-  geom_line(aes(y = pdat$lambda_3, colour = "l3"), size = 0.5)
    l4 <-  geom_line(aes(y = pdat$lambda_4, colour = "l4"), size = 0.5)

    view <-
        p + scale_colour_manual("",
                                breaks = colnames(pdat[,3:6]),
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
#' @param raw_signal
#' @param z
#' @param title
#'
#' @return plot
#'
#' @details todo
#'
#' @section Warning:
#' You can add sections if you like
#'
#'
#' @examples

#'
#' @export
plotLambda <- function(raw_signal, z, title){
    pdat <- cbind(raw_signal , z)

    p <- plotBase(pdat)
    p1 <- plotY(p, pdat)
    p2 <- plotZ(p1, pdat)

    lambdaplot <- with(p2,
                        base + l1 + l2 + l3 + l4) +
        # xlim(min(time)-0.2, max(time)+0.2) +
        # ylim(min(y)-0.002, max(y)+0.002) +
        labs(title = title,
             x = "Time (s)",
             y = "green signal")

    # todo make apply
    return(lambdaplot)
}

NULL

################################################################################
