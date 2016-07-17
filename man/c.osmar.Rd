\name{c.osmar}
\alias{c.osmar}
\title{Combine osmar objects}
\usage{
\method{c}{osmar}(...)
}
\arguments{
  \item{...}{\code{\link{osmar}} objects to be
  concatenated}
}
\value{
An \code{\link{osmar}} object based on the provided objects
}
\description{
Combine two or more \code{\link{osmar}} objects.
}
\examples{
\dontrun{
    muc <- get_osm(center_bbox(11.575278, 48.137222, 200, 200))
    o1 <- subset(muc, node_ids = find(muc, node(tags(v == "Marienplatz"))))
    o2 <- subset(muc, ids = find_down(muc, way(c(96619179, 105071000))))

    o1
    o2
    c(o1, o2)
  }
}

