\name{node}
\alias{attrs}
\alias{node}
\alias{node.condition}
\alias{node.default}
\alias{osm_descriptors}
\alias{refs}
\alias{relation}
\alias{relation.condition}
\alias{relation.default}
\alias{tags}
\alias{way}
\alias{way.condition}
\alias{way.default}
\title{Element descriptors}
\usage{
node(object)

way(object)

relation(object)

\method{node}{default}(object)

\method{way}{default}(object)

\method{relation}{default}(object)

attrs(condition)

tags(condition)

refs(condition)

\method{relation}{condition}(object)

\method{relation}{condition}(object)

\method{relation}{condition}(object)
}
\arguments{
  \item{object}{The descriptor; see details}

  \item{condition}{Condition to describe the object}
}
\description{
For getting OSM data and finding elements in an
\code{\link{osmar}} object one needs to describe the
data---here we provide a simple description language.
}
\examples{
## Description by ID (*.default):
  node(1)
way(1)
relation(1)
## Description by condition (*.condition):
  node(tags(v == "Marienplatz"))
## Description by condition (*.condition):
  way(attrs(id == 17458))
}
\seealso{
\code{\link{bbox}}
}

