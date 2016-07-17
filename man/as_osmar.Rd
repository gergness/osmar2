\name{as_osmar}
\alias{as_osmar}
\alias{osmar}
\title{Convert OSM-XML to an osmar object}
\usage{
as_osmar(xml)
}
\arguments{
  \item{xml}{An OSM-XML object}
}
\value{
A list (with class attribute \code{osmar}) with three
elements:

\describe{

\item{\code{nodes}}{A list with two data frames containing
the attributes and tags of the nodes.}

\item{\code{ways}}{A list with three data frames containing
the attributes, tags, and references of the ways.}

\item{\code{relations}}{A list with three data frames
containing the attributes, tags, and references of the
relations.}

}
}
\description{
Convert a given OSM-XML object (as parsed by
\code{\link[XML]{xmlParse}}) to an osmar object.
}
\examples{
file <- system.file("extdata", "kaufstr.xml", package = "osmar")
  raw <- readLines(file)
  kaufstr <- as_osmar(xmlParse(raw))
}

