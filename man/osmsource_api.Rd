\name{osmsource_api}
\alias{osmsource_api}
\title{API OSM data source}
\usage{
osmsource_api(url = "http://api.openstreetmap.org/api/0.6/",
  encoding = "UTF-8")
}
\arguments{
  \item{url}{URL of the API}

  \item{encoding}{encoding specified as in \code{.encoding}
  parameter in \code{\link[RCurl]{getURL}}. Default is
  \code{"UTF-8"}.}
}
\description{
OSM API version 0.6 data source; see
\url{http://wiki.openstreetmap.org/wiki/API_v0.6}.
}
\details{
#'
}
\section{Supported request elements}{
  \describe{ \item{Bounding box:}{

  Use \code{\link{corner_bbox}} or
  \code{\link{center_bbox}} to retrieve:

  \itemize{

  \item all nodes that are inside a given bounding box and
  any relations that reference them;

  \item all ways that reference at least one node that is
  inside a given bounding box, any relations that reference
  them [the ways], and any nodes outside the bounding box
  that the ways may reference;

  \item all relations that reference one of the nodes or
  ways included due to the above rules (does not apply
  recursively); } }

  \item{Basic request elements:}{ Use
  \code{\link[=osm_descriptors]{node}},
  \code{\link[=osm_descriptors]{way}},
  \code{\link[=osm_descriptors]{relation}} to retrieve an
  element by its ID.

  Use \code{full = TRUE} as additional argument to the
  \code{\link{get_osm}} function. This means that all
  members of the specified elements are retrieved as well:

  \itemize{

  \item For a way, it will return the way specified plus
  all nodes referenced by the way.

  \item For a relation, it will return: (1) the relation
  itself; (2) all nodes, ways, and relations that are
  members of the relation; and (3) all nodes used by ways
  from the previous step.

  } }

  }
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
\references{
\url{http://wiki.openstreetmap.org/wiki/API_v0.6}
}
\seealso{
\code{\link{get_osm}}, \code{\link{bbox}},
\code{\link{osm_descriptors}}

Other osmsource: \code{\link{complete_file}},
\code{\link{osmsource_file}};
\code{\link{osmsource_osmosis}}
}

