\name{find_down}
\alias{find_down}
\alias{find_up}
\title{Find all elements related to an ID}
\usage{
find_down(object, ids)

find_up(object, ids)
}
\arguments{
  \item{object}{An \code{\link{osmar}} object}

  \item{ids}{A vector of IDs tagged whether they are
  \code{node}, \code{way}, or \code{relation}}
}
\value{
A list with the three elements \code{node_ids},
\code{way_ids}, \code{relation_ids}
}
\description{
For a given ID these functions return all IDs of related
elements.
}
\details{
\code{find_down} finds all elements downwards the
hierarchy:

\tabular{rrr}{

node \tab -> \tab node\cr

way \tab -> \tab way + node\cr

relation \tab -> \tab relation + way + node\cr

}

\code{find_up} finds all elements upwards the hierarchy:

\tabular{rrr}{

node \tab -> \tab node + way + relation\cr

way \tab -> \tab way + relation\cr

relation \tab -> \tab relation\cr

}
}
\examples{
data("muc", package = "osmar")
  o1 <- find(muc, way(tags(k == "highway" & v == "pedestrian")))

  find_down(muc, way(o1))
  find_up(muc, way(o1))
}
\seealso{
Other finding: \code{\link{find_nearest_node}};
\code{\link{find}}
}

