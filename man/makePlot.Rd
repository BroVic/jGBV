% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/capneed-funs.R
\name{makePlot}
\alias{makePlot}
\title{Draw Plot of Service Provider Capacity}
\usage{
makePlot(data, labs, colour, annot = waiver(), table = FALSE)
}
\arguments{
\item{data}{A data frame of a service category}

\item{labs}{The labels to be used for the plot}

\item{colour}{The fill of the bars}

\item{annot}{The annotation}

\item{table}{logical; Whether to return the data for tabulation instead}
}
\description{
Draws the plot for a given service category; thus, 'data' is
a prefiltered data frame
}
