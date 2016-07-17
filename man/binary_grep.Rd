\name{\%grep\%}
\alias{\%agrep\%}
\alias{\%grep\%}
\title{Binary operators for grep-like functions}
\usage{
x %grep% y

x %agrep% y
}
\description{
Binary operators for grep-like functions to use in
conditions similar to the "==" operator.
}
\details{
\code{%grep%} is currently implemented as \code{grepl(y, x,
ignore.case = TRUE)}.

\code{%agrep%} is currently implemented as \code{agrep(y,
x, ignore.case = TRUE)} and converts the index result into
a logical vector.
}

