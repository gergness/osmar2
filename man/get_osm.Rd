\name{get_osm}
\alias{get_osm}
\title{Get OSM data}
\usage{
get_osm(x, source = osmsource_api(), ...)
}
\arguments{
  \item{x}{Data identifier, e.g., bounding box or specific
  element; see the help page of the used OSM source for a
  detailed list on the supported identifiers}

  \item{source}{OSM source, e.g.,
  \code{\link{osmsource_api}}}

  \item{...}{Additional arguments suppported by the
  specific OSM source; see corresponding source help page
  for a detailed list}
}
\value{
An \code{\link{osmar}} object
}
\description{
Get OSM data as \code{\link{osmar}} object from different
sources by providing a bounding box.
}
\examples{
\dontrun{
  api <- osmsource_api()

  box <- corner_bbox(11.579341, 48.15102, 11.582852, 48.1530)
  gschw <- get_osm(box, source = api)

  kaufstr <- get_osm(way(3810479))
  kaufstr_full <- get_osm(way(3810479), full = TRUE)
  }
}
\seealso{
\code{\link{bbox}}, \code{\link{osm_descriptors}},
\code{\link{osmsource_api}},
\code{\link{osmsource_osmosis}}
}

