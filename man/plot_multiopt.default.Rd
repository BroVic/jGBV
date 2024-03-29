% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plots.R
\name{plot_multiopt.default}
\alias{plot_multiopt.default}
\title{Draw a plot of multi-response questions}
\usage{
\method{plot_multiopt}{default}(x, ...)
}
\arguments{
\item{x}{An expression being captured code from a valid call to
\code{table_multiplot}.}

\item{...}{arguments to control axis label text via
\code{\link[ggplot2]{element_text}}.}
}
\description{
Draw a plot of multi-response questions using the code for creating
the complimentary tables. This construct is used to avoid
repetitions.
}
