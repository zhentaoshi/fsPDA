% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plot.fsPDA.R
\name{plot.fsPDA}
\alias{plot.fsPDA}
\title{Plot Raw Data, Fitting, and Counterfactual Prediction}
\usage{
\method{plot}{fsPDA}(
  x,
  tlab = NULL,
  ylab = NULL,
  title = NULL,
  point = TRUE,
  legend.position = "bottom"
)
}
\arguments{
\item{x}{An object of the "fsPDA" class.}

\item{tlab}{The label of the time (horizontal) axis.}

\item{ylab}{The label of the value (vertical) axis.}

\item{title}{The text for the title.}

\item{point}{Logical. Should a layer of points be included in the plot?}

\item{legend.position}{The position of legends ("none", "left", "right", "bottom", "top").}
}
\description{
The generic \code{plot} method for objects of the "fsPDA" class
It draws the observed time series of the treated unit in a red solid line.
It uses the fsPDA method to fit the pre-treatment sample (represented by a green dash line),
and then to predict the counterfactual in the post-treatment sample (represented by a blue dash line).
}
\note{
"ggplot2" package must be installed as dependency.
}
\seealso{
\link{est.fsPDA} for examples.
}
