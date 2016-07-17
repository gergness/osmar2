\name{osmsource_osmosis}
\alias{osmsource_osmosis}
\title{Osmosis OSM data source}
\usage{
osmsource_osmosis(file, osmosis = "osmosis")
}
\arguments{
  \item{file}{The file name (and path) of the planet dump}

  \item{osmosis}{The path to the osmosis application}
}
\description{
Planet dumps as OSM data source through the osmosis command
line Java application.
}
\details{
Osmosis is a command line Java application for processing
OSM data. It allows, among other things, to extract data
inside a bounding box or polygon from so called planet
dumps. The usage of this source requires an installed
osmosis; see
\url{http://wiki.openstreetmap.org/wiki/Osmosis}.
}
\section{Supported request elements}{
  \describe{

  \item{Bounding box:}{

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
  recursively); }

  }

  }
}
\examples{
\dontrun{
    ## Download and extract a planet file:
    download.file("http://osmar.r-forge.r-project.org/",
                  "muenchen.osm.gz")
    system("gzip -d muenchen.osm.gz")

    ## Define osmosis source; note that we assume that
    ## osmosis is in our path environment variable (if
    ## not, set osmosis argument to the executable):
    src <- osmsource_osmosis(file = "muenchen.osm")

    ## Get the center of Munich:
    muc_bbox <- center_bbox(11.575278, 48.137222,
                            3000, 3000)
    muc <- get_osm(muc_bbox, src)
    muc
  }
}
\references{
\url{http://wiki.openstreetmap.org/wiki/Osmosis}
}
\seealso{
\code{\link{get_osm}}, \code{\link{bbox}},
\code{\link{osm_descriptors}}

Other osmsource: \code{\link{complete_file}},
\code{\link{osmsource_file}}; \code{\link{osmsource_api}}
}

