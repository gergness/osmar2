\name{as_osmar_bbox.Spatial}
\alias{as_osmar_bbox.Spatial}
\title{Convert sp object to an osmar object}
\usage{
\method{as_osmar_bbox}{Spatial}(obj, ...)
}
\arguments{
  \item{obj}{A \code{\link[sp]{Spatial}} object}

  \item{...}{Ignored}
}
\value{
A \code{\link{bbox}} object
}
\description{
Functions to convert a given \code{\link[sp]{sp}} object to
osmar infrastructure and objects.
}
\examples{
data("muc", package = "osmar")
  muc_points <- as_sp(muc, "points")
  bbox(muc_points)           # sp::bbox object
  as_osmar_bbox(muc_points)  # osmar::bbox object
}
\seealso{
Other as_osmar_bbox: \code{\link{as_osmar_bbox}};
\code{\link{bbox}}, \code{\link{center_bbox}},
\code{\link{corner_bbox}}
}

