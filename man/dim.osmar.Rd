\name{dim.osmar}
\alias{dim.osmar}
\title{Dimension of osmar objects}
\usage{
\method{dim}{osmar}(x)
}
\arguments{
  \item{x}{An \code{\link{osmar}} object}
}
\value{
A named vector with the number of nodes, ways and
relations.
}
\description{
Dimension of osmar objects
}
\examples{
\dontrun{
    muc <- get_osm(center_bbox(11.575278, 48.137222, 200, 200))
    dim(muc)
  }
}

