globalVariables(c(
  "Open_date",
  "GBV_date",
  "GBV_period",
  "Open_period",
  "Period",
  "Type of Period"
))

#' Ouputs on Opening and Commencement of GBV Services for a State
#'
#' @import dplyr
#' @import ggplot2
#' @importFrom flextable flextable
#' @importFrom tidyr pivot_longer
#'
#' @param .data The data frame
#' @param open The column with the opening data (an integer).
#' @param gbv The column with the date when GBV service was commenced
#' (an integer).
#' @param format The format to be used in extracting the date as in
#' \code{base::format}.
#'
#' @return A list with 2 elements: An object of class \code{flextable} and
#' another of class \code{ggplot}, respectively called \code{table} and
#' \code{plot}.
#'
#' @export
outputs_commencement <- function(.data, open, gbv, format = "%Y") {
  minmax <- .data[[open]] %>%
    format(format) %>%
    as.numeric() %>%
    range(na.rm = TRUE)
  earliest <- minmax[1]
  bin <- 10L
  if ((rem <- earliest %% bin) != 0L)
    earliest <- earliest - rem
  latest <- minmax[2]
  year.cats <- seq(earliest, latest + (bin - (latest %% bin)), bin)
  year.labs <- paste0(year.cats, "-", year.cats + (bin - 1L))
  year.labs <- year.labs[-length(year.labs)]
  if (latest >= 2020L)
    year.labs[length(year.labs)] <- "2020+"

  createYearGroups <- function(x) {
    stopifnot(is.numeric(x))
    cut(x, year.cats, year.labs, ordered_result = TRUE)
  }
  thisyr <- as.integer(format(Sys.Date(), "%Y"))
  period.data <- .data %>%
    select(all_of(c(open, gbv))) %>%
    mutate(across(everything(), ~ format(.x, format))) %>%
    mutate(across(everything(), ~ as.integer(.x))) %>%
    setNames(c("Open_date", "GBV_date")) %>%  # reset for convenience
    arrange(Open_date) %>%
    mutate(Open_date = ifelse(Open_date > thisyr, NA_integer_, Open_date)) %>%
    mutate(Open_period = createYearGroups(Open_date)) %>%
    mutate(GBV_period = createYearGroups(GBV_date)) %>%
    # select(contains('period')) %>%
    drop_na()

  gbv <- period.data %>%
    group_by(GBV_period, .drop = FALSE) %>%
    count() %>%
    ungroup()

  open <- period.data %>%
    group_by(Open_period, .drop = FALSE) %>%
    tally() %>%
    ungroup() %>%
    bind_cols(gbv) %>%
    setNames(c(
      "Year Open",
      "Number of facilities opened",
      "Year GBV Services",
      "Number of facilities providing GBV services"
    ))

  ## Table
  ft <- flextable(open, cwidth = c(2, 1, 2, 1))

  ## Plot
  gg <- period.data %>%
    select(!contains('date')) %>%   # Remember the names were reset internally
    pivot_longer(everything(), names_to = "Type of Period", values_to = "Period") %>%
    ggplot(aes(Period, fill = `Type of Period`)) +
    geom_bar(position = 'dodge') +
    coord_flip()

  list(table = ft, plot = gg)
}
