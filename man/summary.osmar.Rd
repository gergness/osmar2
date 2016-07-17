\name{summary.osmar}
\alias{print.summary.nodes}
\alias{print.summary.osmar}
\alias{print.summary.relations}
\alias{print.summary.ways}
\alias{summary.nodes}
\alias{summary.osmar}
\alias{summary.relations}
\alias{summary.ways}
\title{Summarize osmar objects}
\usage{
\method{summary}{osmar}(object, ...)

\method{print}{summary.osmar}(x, max.print = 3, nchar.value = 20, ...)

\method{summary}{nodes}(object, ...)

\method{print}{summary.nodes}(x, max.print = 10, nchar.value = 20, ...)

\method{summary}{ways}(object, ...)

\method{print}{summary.ways}(x, max.print = 10, nchar.value = 20, ...)

\method{summary}{relations}(object, ...)

\method{print}{summary.relations}(x, max.print = 10, nchar.value = 20, ...)
}
\arguments{
  \item{object}{An object (\code{osmar}, \code{nodes},
  \code{ways}, or \code{relations} for which a summary is
  desired}

  \item{...}{Ignored}

  \item{x}{The computed summary object to print}

  \item{max.print}{Maximum number of shown tags}

  \item{nchar.value}{Number of shown characters of the
  value column}
}
\value{
\code{summary.osmar} returns a list with the summaries for
nodes, ways, and relations.

\code{summary.nodes}, \code{summary.ways},
\code{summary.relations} all return a list with

\describe{

\item{\code{key}}{A contingency table of the counts of each
key label; in descending order}

\item{\code{val}}{A contingency table of the counts of each
value label; in descending order}

\item{\code{keyval}}{A contingency table of the counts
greater zero of each combination of key and value labels;
in descending order}

}
}
\description{
Summaries of \code{osmar}, \code{nodes}, \code{ways}, and
\code{relations} objects. Use these methods to get an
overview of the content.
}
\seealso{
\code{\link{osmar}}
}

