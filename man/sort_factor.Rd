% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/factors.R
\name{sort_factor}
\alias{sort_factor}
\title{Create Sorted Factor}
\usage{
sort_factor(var, base.level = NULL, ...)
}
\arguments{
\item{var}{Vector to convert to factor.}

\item{base.level}{Reference level as a character string. Optional argument.}

\item{...}{Additional arguments to pass to \code{\link[=factor]{factor()}}}
}
\value{
A factor with levels in ascending order and a potential user-defined
reference level.
}
\description{
Set factor with sorted ascending unique values as levels. Also allows the
user to explicitly set the reference level.
}
\examples{
no_ref <- sort_factor(c("e", "f", "z", "d", "a"))
with_ref <- sort_factor(c("e", "f", "z", "d", "a"), base.level = "z")

levels(no_ref)
levels(with_ref)

}
