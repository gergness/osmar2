\name{plot.osmar}
\alias{plot.osmar}
\alias{plot_nodes}
\alias{plot_ways}
\title{Plot osmar object}
\usage{
\method{plot}{osmar}(x, way_args = list(col = gray(0.7)),
  node_args = list(pch = 19, cex = 0.1, col = gray(0.3)), ...)

plot_nodes(x, add = FALSE, ...)

plot_ways(x, add = FALSE, xlab = "lon", ylab = "lat", ...)
}
\arguments{
  \item{x}{An \code{\link{osmar}} object}

  \item{way_args}{A list of parameters for plotting ways}

  \item{node_args}{A list of parameters for plotting nodes}

  \item{...}{Ignored}

  \item{add}{New plot device or plot on existing onde}

  \item{xlab}{A x-axis label}

  \item{ylab}{A y-axis label}
}
\description{
Simple plotting functions to visualize \code{\link{osmar}}
objects. Note that for more complex plots, we suggest to
convert the objects into \code{sp} and use their plotting
functionality.
}

