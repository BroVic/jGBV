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
#' @param use.table Logical. Whether a table object is created internally or
#' not for the handling of bivariate data.
#'
#' @import ggplot2
#' @importFrom dplyr group_by
#' @importFrom dplyr select
#' @importFrom dplyr summarise
#' @importFrom flextable flextable
#' @importFrom flextable theme_box
#' @importFrom flextable autofit
#' @importFrom magrittr %>%
#' @importFrom tidyr pivot_wider
#'
#' @export
dual_singleopts <-
  function(dat, x, y = NULL, ..., na.rm = TRUE, use.table = FALSE)
{
  x <- .assertNumAndChar(x)

  if (!is.null(y))
    y <- .assertNumAndChar(y)

  dat <- applyFilter(dat, ...)

  tb <-
    table_singleopt(dat, x, y, data.only = !use.table, table.only = use.table)
  # TODO: Consider putting this block into the preceding function
  if (inherits(tb, 'table')) {
    tb <- tb %>%
      as.data.frame() %>%
      group_by(x, y) %>%
      summarise(Freq = sum(Freq)) %>%
      pivot_wider(names_from = x, values_from = Freq)

    no.col.rgx <- "^no"

    if (any(grepl(no.col.rgx, names(tb), ignore.case = TRUE))) {
      names(tb) <- tools::toTitleCase(names(tb))
      opts <- c("Yes", "No")
      tb <- tb %>%
        mutate(
          Percent = round(
            (.data[[opts[[1]]]] / (.data[[opts[[1]]]] + .data[[opts[[2]]]])) * 100,
            1)
        ) %>%
        select(!matches(no.col.rgx)) %>%
        rename(Freq = Yes)
    }
    tb <- rename(tb, Variable = y)
  }

  gg <- if (is.null(y))
    ggplot(tb, aes(Variable, Freq))
  else
    ggplot(tb, aes(Variable, y))

  gg <- gg  +
    geom_col() +
    theme(axis.text = element_text(size = getOption("jgbv.axis.text.size")))

  list(table = flextable(tb), plot = gg)
}








.assertNumAndChar <- function(arg) {
  if (!is.numeric(arg) && !is.character(arg))
    stop(sprintf(
      "'%s' should be either a character or numeric atomic vector",
      deparse(substitute(arg))
    ))
  if (length(arg) > 1L) {
    arg <- arg[1]
    warning(sprintf(
      "'%s' must have only one element. %s was taken and others discarded",
      deparse(substitute(arg)),
      sQuote(arg)
    ))
  }
  arg
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






#' Filter Service Data
#'
#' Focus the data on a given GBV service category or sector
#'
#' @param d The data frame
#' @param group.regex A regular expression that exactly matches the
#' prefix used for all the service categories.
#' @param srvtype The service type; also an exact match
#'
#' @return The modified data frame
#'
#' @note The function will preserve variable labels created with
#' \code{labelled::var_label}.
#'
#' @importFrom labelled var_label
#'
#' @export
filterData <- function(d, group.regex, srvtype)
{
  stopifnot(is.data.frame(d))
  if (!is.character(group.regex) || !is.character(srvtype))
    stop("Both arg 2 and 3 must be strings")
  if (inherits(d, "labelled"))
    ll <- labelled::var_label(d, unlist = TRUE)
  cname <- paste0(group.regex, srvtype)
  col <- d[[cname]]
  bool.col <- if (is.numeric(col) && all(0:1 %in% col))
    as.logical(col)
  else if (is.logical(col))
    col
  else
    stop(
      "Filtering is only supported for logical and binary numeric vectors"
    )
  d <- d[bool.col, ]
  if (inherits(d, "labelled"))
    labelled::var_label(d) <- ll
  d
}
