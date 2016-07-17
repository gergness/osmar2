\name{subset.osmar}
\alias{subset.osmar}
\title{Subset an osmar object}
\usage{
\method{subset}{osmar}(x, node_ids = NULL, way_ids = NULL,
  relation_ids = NULL, ids = list(node_ids = node_ids, way_ids = way_ids,
  relation_ids = relation_ids), ...)
}
\arguments{
  \item{x}{An \code{\link{osmar}} object}

  \item{node_ids}{Node ID vector}

  \item{way_ids}{Way ID vector}

  \item{relation_ids}{Relation ID vector}

  \item{ids}{A list composed of \code{node_ids},
  \code{way_ids}, \code{relation_ids}; for easier usage
  with results from \code{\link{find_up}} and
  \code{\link{find_down}}}

  \item{...}{Ignored}
}
\value{
An \code{\link{osmar}} object containing the specified
elements
}
\description{
Subset an osmar object
}
\examples{
data("muc", package = "osmar")
  id <- find(muc, node(tags(v == "Marienplatz")))

  subset(muc, node_ids = id)

  subset(muc, ids = find_up(muc, node(id)))
}

