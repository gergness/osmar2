\name{as_osmar_bbox}
\alias{as_osmar_bbox}
\title{Bounding box converter generic}
\usage{
as_osmar_bbox(obj, ...)
}
\arguments{
  \item{obj}{Object to compute osmar \code{\link{bbox}}}

  \item{...}{Additional parameters for underlying
  functions}
}
\description{
Generic function for implementing converters from various
objects (e.g., \link[sp]{sp} \code{\link[sp]{Spatial}}
objects) to osmar \code{\link{bbox}} objects.
}
\seealso{
Other as_osmar_bbox: \code{\link{as_osmar_bbox.Spatial}};
\code{\link{bbox}}, \code{\link{center_bbox}},
\code{\link{corner_bbox}}
}

