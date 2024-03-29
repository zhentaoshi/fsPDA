#' Plot Raw Data, Fitting, and Counterfactual Prediction
#'
#' The generic \code{plot} method for objects of the "fsPDA" class
#' It draws the observed time series of the treated unit in a red solid line.
#' It uses the fsPDA method to fit the pre-treatment sample (represented by a green dash line),
#' and then to predict the counterfactual in the post-treatment sample (represented by a blue dash line).
#'
#'
#' @param x An object of the "fsPDA" class.
#' @param tlab The label of the time (horizontal) axis.
#' @param ylab The label of the value (vertical) axis.
#' @param title The text for the title.
#' @param point Logical. Should a layer of points be included in the plot?
#' @param legend.position The position of legends ("none", "left", "right", "bottom", "top").
#'
#' @note
#' "ggplot2" package must be installed as dependency.
#'
#' @seealso
#' \link{est.fsPDA} for examples.
#'
#' @export
#'



plot.fsPDA <- function(x, tlab = NULL, ylab = NULL, title = NULL, point = TRUE, legend.position = "bottom") {
  suppressMessages(library(ggplot2))

  ggdata <- rbind(
    data.frame(date = x$in_sample$date, value = x$in_sample$observation, type = "observation"),
    data.frame(date = x$out_of_sample$date, value = x$out_of_sample$observation, type = "observation"),
    data.frame(
      date = c(x$in_sample$date, x$out_of_sample$date[1]),
      value = c(x$in_sample$fit, x$out_of_sample$counterfactual[1]), type = "in-sample fit"
    ),
    data.frame(date = x$out_of_sample$date, value = x$out_of_sample$counterfactual, type = "counterfactual")
  )
  ggdata$type <- factor(ggdata$type, levels = c("observation", "in-sample fit", "counterfactual"))

  plot <- ggplot(ggdata, aes(x = date, y = value, color = type, linetype = type)) +
    geom_vline(xintercept = x$out_of_sample$date[1]) +
    geom_line() +
    scale_linetype_manual(values = c("solid", "dashed", "dashed")) +
    theme_bw() +
    theme(legend.title = element_blank(), legend.position = legend.position) +
    labs(title = title) +
    xlab(tlab) +
    ylab(ylab)
  if (point) {
    plot <- plot + geom_point()
  }

  print(plot)
}
