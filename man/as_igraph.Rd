% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/as-igraph.R
\name{as_igraph}
\alias{as_igraph}
\title{Convert osmar object to igraph}
\usage{
as_igraph(obj)
}
\arguments{
\item{obj}{An \code{\link{osmar}} object}
}
\value{
An igraph-package \code{graph} object
}
\description{
Convert an osmar object to an igraph (see
igraph-package).
}
\examples{
file <- system.file("extdata", "kaufstr.xml", package = "osmar2")
raw <- readr::read_file(file)
kaufstr <- as_osmar(read_xml(raw))
kaufstrGraph <- as_igraph(kaufstr)

}

