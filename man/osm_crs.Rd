\name{osm_crs}
\alias{osm_crs}
\title{CRS for OpenStreetMap}
\usage{

  osm_crs(crs = "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0")
}
\arguments{
  \item{crs}{A valid proj4 string}
}
\value{
A \code{\link[sp]{CRS}} object
}
\description{
Coordinate Reference System used in OpenStreetMap.
}
\details{
The default value is the WGS84 Ellipsoid which is used in
GPS, therefore it is used in OpenStreetMap.
}
\examples{
osm_crs()
  class(osm_crs())
}

