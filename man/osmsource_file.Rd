\name{osmsource_file}
\alias{complete_file}
\alias{osmsource_file}
\title{OSM file data source}
\usage{
osmsource_file(file)
}
\arguments{
  \item{file}{The file name (and path) of the osm file}
}
\description{
Imports the complete OSM file.
}
\section{Supported request elements}{
  \describe{

  \item{Dummy request element:}{Use the function
  \code{compete_file} as dummy description for all elements
  }

  }
}
\examples{
\dontrun{
    get_osm(complete_file(), source = osmsource_file("muc.osm"))
  }
}
\seealso{
\code{\link{get_osm}}, \code{\link{bbox}},
\code{\link{osm_descriptors}}

Other osmsource: \code{\link{osmsource_api}};
\code{\link{osmsource_osmosis}}
}

