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
Convert an osmar object to an igraph (see igraph-package).
}
\examples{
file <- system.file("extdata", "kaufstr.xml", package = "osmar")
raw <- readLines(file)
kaufstr <- as_osmar(xmlParse(raw))
kaufstrGraph <- as_igraph(kaufstr)
}

