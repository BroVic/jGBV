#' Make a frequency tabulation of multi-response questions
#'
#' @rdname tables
#'
#' @param data The data
#' @param dictionary The data dictionary
#' @param indices Numeric vector representing indices of columns chosen
#' @param use.regex Logical; whether to use label patterns to determine
#' what the options should be for the responses
#' @param data.only Logical; whether to return a \code{data.frame} (\code{TRUE})
#' or a formatted object for printing in the output document, in this case an
#' object of class \code{flextable}
#' @param ... Arguments passed from higher level wrapper functions. In this case
#' the argument expected is \code{caption}, which is a character vector of length
#' 1.
#'
#'
#' @import dplyr
#' @import stringr
#' @importFrom flextable set_caption
#' @importFrom userfriendlyscience multiresponse
#'
#' @export
table_multiopt <-
  function(data,
           dictionary,
           indices,
           use.regex = TRUE,
           data.only = FALSE,
           ...) {
    stopifnot(nrow(dictionary) == ncol(data))

    opts <- get_value_labels(dictionary, indices, use.regex)

    mult <- data %>%
      select(all_of(indices)) %>%
      mutate_all(~ ifelse(is.na(.), 0L, 1L)) %>%
      multiResponse() %>%          # from userfriendlyscience package
      as_tibble

    # Separate the totals for later use when drawing tables
    tot <- slice_tail(mult) %>%
      select(!last_col()) %>%
      as.character

    # Make a function for creating abridged options
    .abridgeOptions <- function(x) {
      x %>%
        str_remove_all("(or|to|of|in|the|a|by)(\\s)") %>%
        sub("(([[:alpha:]]+\\s){3})(.+)", "\\1", .) %>%
        str_replace('and', '&') %>%
        str_trim %>%
        str_squish %>%
        str_to_title
    }

    mult <- mult %>%
      filter(Option != "Total") %>%
      mutate(Option = opts) %>%
      filter(Option != "None") %>%
      mutate(Variable = .abridgeOptions(Option)) %>%
      relocate(Variable, .after = Option) %>%
      mutate(Variable = factor(Variable) %>%
               fct_reorder(Frequency, .desc = TRUE)) %>%
      arrange(desc(Frequency)) %>%
      select(!last_col(offset = 1L))

    if (data.only)
      return(mult)

    ft <- mult %>%
      myFlextable(values = tot)
    argslist <- list(...)
    if ('caption' %in% names(argslist))
      ft <- set_caption(ft, caption = argslist$caption)
    ft
  }





#' Make a frequency tabulation that for variables with Yes/No responses
#'
#' @rdname tables
#'
#' @param data The data.
#' @param col The column with the responses
#' @param data.only Return only the \code{data.frame}?
#' @param ... Additional response options, if they exist.
#'
#' @import dplyr
#' @import tidyr
#' @importFrom rlang enquo
#'
#' @export
table_yesno <- function(data, col, data.only = FALSE, ...) {
  column <- rlang::enquo(col)

  modified <- data %>%
    select(name_of_lga, !!column) %>%
    mutate(!!column := unclass(!!column)) %>%
    drop_na() %>%
    mutate(name = ifelse(!!column == 1, "Yes", "No")) %>%
    pivot_wider(names_from = name,
                values_from = !!column,
                values_fn = length,
                values_fill = 0L) %>%
    summarise_at(c("Yes", "No"), sum)

  if (data.only)
    return(modified)

  myFlextable(modified, ...)
  # %>% set_header_labels(name_of_lga = "LGA")
}


#' @importFrom dplyr %>%
#' @importFrom dplyr select
#' @importFrom dplyr rename
#' @importFrom flextable flextable
#' @importFrom flextable theme_box
myFlextable <- function(data, ...) {
  if (all(!grepl("^(Yes|No)$", colnames(data)))) {
    data <- data %>%
      select(-Variable) %>%
      rename(Variable = Option)  # TODO: Use variable name instead??
  }
  ft <- flextable(data)

  if (...length() > 0L) {
    ft <- ft %>%
      width(j = 1, width = 2)
  }
  ft %>%
    theme_box
}
