% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/data.R
\name{make_date}
\alias{make_date}
\title{Manipulate A Vector For Getting Correct Date-Time Types}
\usage{
make_date(x, date.string = TRUE)
}
\arguments{
\item{x}{The vector to be modified.}

\item{date.string}{Whether to return a date(-time) sting or and object.}
}
\value{
The modified vector by default (when \code{date.string} is
\code{TRUE}); otherwise an object of class \code{Date} or \code{POSIXct}.
}
\description{
Manipulate A Vector For Getting Correct Date-Time Types
}
