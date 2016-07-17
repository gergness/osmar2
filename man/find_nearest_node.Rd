\name{find_nearest_node}
\alias{find_nearest_node}
\title{Find nearest node with given conditions}
\usage{
find_nearest_node(object, id, condition)
}
\arguments{
  \item{object}{An \code{\link{osmar}} object}

  \item{id}{An node ID}

  \item{condition}{Condition for the element to find; see
  \code{\link{find}}}
}
\value{
A node ID or \code{NA}
}
\description{
For a given ID, find nearest node (geographical distance)
with given conditions.
}
\examples{
data("muc", package = "osmar")
  id <- find(muc, node(tags(v == "Marienplatz")))[1]

  find_nearest_node(muc, id, way(tags(k == "highway" & v == "pedestrian")))
}
\seealso{
Other finding: \code{\link{find_down}},
\code{\link{find_up}}; \code{\link{find}}
}

