% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/objects.R
\docType{data}
\name{raampStates}
\alias{raampStates}
\alias{tool.sectors}
\alias{new.varnames}
\title{Builtin Objects For JHPIEGO GBV Projects}
\format{
An object of class \code{character} of length 4.

An object of class \code{Tools} of length 7.

An object of class \code{character} of length 347.
}
\usage{
raampStates

tool.sectors

new.varnames
}
\description{
Builtin Objects For JHPIEGO GBV Projects
}
\note{
\code{new.varnames} is a named vector of new variable names that will
be applied to the imported raw data. It is named to allow for indexing.
These names are equally used in the database, so the actual values are SQL
compliant, while the names are more R-focused.

Not all the values are necessarily named; just those that are actually
in use at the project level. The unnamed ones are usually extracted using
regular expressions, especially for multi-response questions.
}
\keyword{datasets}
