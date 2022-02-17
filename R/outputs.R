# Source file: outputs.R
#
# License: MIT
#
# Copyright (c) 2022 Victor Ordu

#' Produces dual outputs (table & plot) for multi-response variables
#' @param dat The data frame
#' @param indices Columns to be used. Could be an integer vector (indices of
#' the columns) or a character vector of length 1 - a regular expression for
#' the columns to be selected.
#' @param ... Arguments passed for internal use.
#' @param size The font size for the axis labels.
#' @param na.rm Logical; whether to keep NAs or not.
#'
#' @export
dual_multiopts <-
  function(dat,
           indices,
           ...,
           size = getOption("jgbv.axis.text.size"),
           na.rm = TRUE) {
    if (!is.numeric(indices)) {
      if (is.character(indices)) {
        if (length(indices) > 1L)
          stop("'indices' cannot be a character vector longer than 1L")
      indices <- grep(indices, names(dat))
      }
      else
        stop("'indices' cannot be of type ", typeof(indices))
    }
    dat <- applyFilter(dat, ...)
    ft <- table_multiopt(dat, indices = indices)
    pp <- show_output(dat, indices, size = size)
    list(table = ft, plot = pp)
  }








#' Makes outputs like above but for single response questions
#'
#' @param dat The data frame
#' @param x,y An integer or character vector of length \code{1L} for selecting
#' a column from \code{.data}.
#' @param ... Arguments passed on to \code{applyFilter}.
#' @param na.rm Logical
#'
#' @import ggplot2
#' @importFrom flextable flextable
#' @importFrom flextable theme_box
#' @importFrom flextable autofit
#' @importFrom magrittr %>%
#'
#' @export
dual_singleopts <- function(dat, x, y, ..., na.rm = TRUE) {
  stopifnot(is.numeric(x))
  if (!missing(y))
    stopifnot(is.numeric(y))
  dat <- applyFilter(dat, ...)
  tb <- table_singleopt(dat, x, y, data.only = TRUE)
  gg <- if (missing(y)) {
    ggplot(tb, aes(Variable, Freq))
  }
  else
    ggplot(tb, aes(Variable, y))
  gg <- gg +
    theme(axis.text = element_text(size = getOption("jgbv.axis.text.size")))
  ft <- tb %>%
    flextable() %>%
    # theme_box() %>%
    autofit()
  list(table = ft, plot = gg + geom_col())
}







applyFilter <-
  function(data,
           filter = NULL,
           na.rm = TRUE,
           opt.col = "type_of_service")
  {
    vars <- colnames(data)
    colsOfInterst <-
      grep(opt.col,
           vars,
           value = TRUE,
           ignore.case = TRUE)
    if (is.null(filter))
      return(data)
    choiceVal <-
      grep(filter,
           colsOfInterst,
           value = TRUE,
           ignore.case = TRUE)
    choiceIndex <- match(choiceVal, vars)
    data <- data[data[[choiceIndex]],]
    if (na.rm)
      data <- data[!is.na(data[[choiceIndex]]),]
    data
  }
