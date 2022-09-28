# Source file: outputs.R
#
# License: MIT
#
# Copyright (c) 2022 Victor Ordu

globalVariables(
  c(
    "No", "Yes", "Percent",              # dual_singleopts
    "LGA", "allempty",                   # multiresponse_by_lga
    "xVar"                               # plot_services_by_lga
  )
)

#' Produces dual outputs (table & plot) for multi-response variables
#'
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
  went.table.route <- inherits(tb, 'table')
  if (went.table.route) {
    tb <- tb %>%
      as.data.frame() %>%
      group_by(x, y) %>%
      summarise(Freq = sum(Freq)) %>%
      pivot_wider(names_from = x, values_from = Freq) %>%
      rowwise(y) %>%
      mutate(Total = sum(c_across()))

    no.col.rgx <- "^(no|yes)"

    if (any(grepl(no.col.rgx, names(tb), ignore.case = TRUE))) {
      names(tb) <- tools::toTitleCase(names(tb))
      tb <- tb %>%
        relocate(No, .after = y) %>%
        relocate(Yes, .after = y)
      if (ncol(tb) <= 4L) {
        tb <- tb %>%
          select(-No) %>%
          rename(Freq = Yes)
      }
    }
    tb <- tb %>%
      rename(Variable = y) %>%
      .addPercentColumn()
  }

  gg <- if (is.null(y))
    ggplot(tb, aes(Variable, Freq))
  else {
    if (went.table.route)
      ggplot(tb, aes(Variable, Percent))
    else
      ggplot(tb, aes(Variable, y))
  }

  gg <- gg  +
    geom_col() +
    theme(axis.text = element_text(size = getOption("jgbv.axis.text.size")))

  list(table = flextable(tb), plot = gg)
}





.addPercentColumn <- function(d) {
  stopifnot(is.data.frame(d))
  .checkCol <- function(x) any(grepl(x, names(d), ignore.case = TRUE))
  hasPercCol <- .checkCol("^Perc")
  hasFreqCol <- .checkCol("^(Freq|No\\.)")

  # When there is no column specifying the number of observations
  # by rows, there is no need to try to compute the percentages
  if (hasPercCol || !hasFreqCol)
    return(d)

  d$Percent <- round((d$Freq / d$Total) * 100, 1)
  d
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






#' Make Multiple Response Tabulation Disaggregated By Local Government Area
#'
#' @param data The data frame
#' @param indices A numeric vector with the indices of the columns with
#' the requested variable.
#' @param text.removed A string. Portion of text that is to be removed from
#' the variable elements (optional).
#'
#' @return An object of class \code{data.frame}.
#'
#' @importFrom dplyr %>%
#' @importFrom dplyr select
#' @importFrom dplyr rename_with
#' @importFrom dplyr mutate
#' @importFrom dplyr arrange
#' @importFrom dplyr summarise
#' @importFrom dplyr group_by
#' @importFrom dplyr rowwise
#' @importFrom dplyr filter
#' @importFrom dplyr pull
#' @importFrom dplyr c_across
#' @importFrom dplyr all_of
#' @importFrom dplyr last_col
#' @importFrom dplyr everything
#' @importFrom stringr str_replace_all
#' @importFrom stringr str_remove
#' @importFrom stringr str_to_title
#'
#' @export
multiresponse_by_lga <-
  function(data,
           indices,
           text.removed = NULL) {
    stopifnot({
      is.data.frame(data)
      is.numeric(indices)
    })
    supp <- data %>%
      select(LGA, all_of(indices))

    # Tidy up the header by removing extraneous,
    # repetitive text
    if (!is.null(text.removed))
      supp <- supp %>%
      rename_with(~ str_remove(.x, text.removed))

    df <- supp %>%
      rename_with(~ str_replace_all(.x, "_", " ")) %>%
      rename_with(str_to_title, .cols = 2:last_col()) %>%
      mutate(across(2:last_col(), as.numeric)) %>%
      arrange(LGA) %>%
      group_by(LGA) %>%
      summarise(across(everything(), sum))

    # Establish which rows are totally empty
    df <- df %>%
      rowwise(LGA) %>%
      mutate(allempty = all(is.na(c_across())))

    ## Attach metadata on any data-empty LGAs
    attr(df, which = "emptyLGAs") <- df %>%
      filter(allempty) %>%
      pull(LGA)

    ## Remove empty LGAs from final object
    df <- df %>%
      filter(!allempty) %>%
      select(-allempty)

    if (!nrow(df))
      return()

    df
  }




#' Draw A Plot of Service Types By LGA
#'
#' @param dat A data frame; most appropriately one generated by
#' \code{link{multiresponse_by_lga}}
#' @param labs The labels representing each service category
#'
#' @details To use the labels correctly, one may first run the function with
#' the default values and see if there is a need to fine-tune.
#'
#' @return An object of class \code{ggplot}.
#'
#' @import dplyr
#' @import ggplot2
#'
#' @export
plot_services_by_lga <- function(dat, labs = NULL) {
  if (!is.data.frame(dat))
    stop("'dat' is not a data frame")

  if (is.null(labs))
    labs <-
      c("Economic", "Health", "Legal", "Social", "Security", "Shelter")

  dat %>%
    pivot_longer(2:last_col(), names_to = "xVar", values_to = "Freq") %>%
    mutate(xVar = factor(xVar, labels = labs)) %>%
    ggplot(aes(xVar, Freq, fill = xVar)) +
    geom_col() +
    scale_color_brewer(palette = "Set1") +
    scale_fill_discrete(name = "Type of Service") +
    labs(y = "No. of facilities") +
    theme(
      axis.text.x = element_blank(),
      legend.position = "right",
      axis.title.x = element_blank(),
      axis.ticks.x = element_blank()
    ) +
    facet_wrap(~ LGA)
}
