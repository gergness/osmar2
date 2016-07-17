\name{find}
\alias{find}
\title{Find element for a given condition}
\usage{
find(object, condition)
}
\arguments{
  \item{object}{An \code{\link{osmar}} object}

  \item{condition}{A condition for the element to find; see
  details section.}
}
\value{
The ID of the the element
}
\description{
Find element for a given condition
}
\details{
The basis of an \code{\link{osmar}} object are
\code{data.frame}s; therefore the \code{condition}
principally follows the rules for
\code{\link[base]{subset}}: logical expression indicating
elements or rows to keep.

Furthermore, one has to define on which element and which
data of the \code{\link{osmar}} object the condition
applies: \code{element(data(condition))}, see
\code{\link{osm_descriptors}}.
}
\examples{
data("muc", package = "osmar")
  find(muc, node(tags(v == "Marienplatz")))
  find(muc, node(tags(v \%agrep\% "marienplatz")))
  find(muc, node(attrs(id == 19475890)))
  find(muc, way(tags(k == "highway" & v == "pedestrian")))
}
\seealso{
binary_grep

Other finding: \code{\link{find_down}},
\code{\link{find_up}}; \code{\link{find_nearest_node}}
}

