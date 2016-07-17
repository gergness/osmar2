\name{as_sp}
\alias{as_sp}
\title{Convert osmar object to sp object}
\usage{
as_sp(obj, what = c("points", "lines", "polygons"), crs = osm_crs(),
  simplify = TRUE)
}
\arguments{
  \item{obj}{An \code{\link{osmar}} object}

  \item{what}{A string describing the sp-object; see
  Details section}

  \item{crs}{A valid \code{\link[sp]{CRS}} object; default
  value is given by \code{\link{osm_crs}}-function}

  \item{simplify}{Should the result list be simplified to
  one element if possible?}
}
\value{
A list of one or more \link[sp]{sp} objects; see Details
section.
}
\description{
Convert an osmar object to a \link[sp]{sp} object.
}
\details{
Depending on the strings given in \code{what} the
\code{\link{osmar}} object will be converted into in a list
of objects given by the \link[sp]{sp}-package:

\describe{

\item{\code{what = "points"}}{the object will be converted
in a \code{\link[sp]{SpatialPointsDataFrame}}. The data
slot is filled with the attrs slot of \code{obj$nodes}.}

\item{\code{what = "lines"}}{the object will be converted
in a \code{\link[sp]{SpatialLinesDataFrame}}. It is build
with all possible elements which are in \code{obj$ways}
\code{obj$relations}. The data slot is filled with elements
of both.}

\item{\code{what = "polygons"}}{the object will be
converted in a \code{\link[sp]{SpatialPolygonsDataFrame}}.
It consists of elements which are in \code{obj$ways} slot.}

}

Every conversion needs at least a non-empty
\code{obj$nodes$attrs}-slot because spatial information are
stored in there.
}
\examples{
data("muc", package = "osmar")
  muc_points <- as_sp(muc, "points")
  muc_lines <- as_sp(muc, "lines")
  muc_polygons <- as_sp(muc, "polygons")

  bbox(muc_points)
}

