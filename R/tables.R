# Source file: tables.R
#
# MIT License
#
# Copyright (c) 2019 Victor Ordu

globalVariables(c(
  "Variable",
  "Option",
  "Frequency",
  "name_of_lga",
  "Freq"
))


#' Make a frequency tabulation
#'
#' @rdname tables
#'
#' @note To control what happens via the \code{use.regex} argument, the
#' project has to set this option, ideally in the local \code{.Rprofile} to
#' a desired value. For instance, to set a project-wide default value of
#' \code{FALSE} for this argument, set \code{options(use.regex = FALSE)}, and
#' that way, there is no need to set the argument, each time the function
#' is called.
#'
#' @param data The data
#' @param dictionary The data dictionary
#' @param indices Numeric vector representing indices of columns chosen
#' @param use.regex Logical; whether to use label patterns to determine
#' what the options should be for the responses
#' @param data.only Logical; whether to return a \code{data.frame} (\code{TRUE})
#' or a formatted object for printing in the output document, in this case an
#' object of class \code{flextable}
#' @param redcap Logical. Is the project where the function is being used one
#' for which the data were stored on REDCap?
#' @param ... Arguments passed from higher level wrapper functions. In this case
#' the argument expected is \code{caption}, which is a character vector of length
#' 1.
#'
#'
#' @importFrom dplyr as_tibble
#' @importFrom flextable set_caption
#' @importFrom purrr map_dfc
#'
#' @export
table_multiopt <-
  function(data,
           dictionary = NULL,
           indices,
           use.regex = getOption("use.regex"),
           data.only = FALSE,
           redcap = getOption("data.on.redcap"),
           ...) {
    if (is.null(dictionary)) {
      dictionary <- makeDictionary(data)
    }
    if (nrow(dictionary) != ncol(data))
      stop(
        "'data' and 'dictionary' are incompatible:
             `nrow(dictionary)` is not equal to `ncol(data)`"
      )
    if (is.null(redcap)) redcap <- TRUE  # for backward compatibility
    opts <- get_value_labels(dictionary, indices, use.regex, redcap = redcap)

    mult <- data %>%
      select(all_of(indices)) %>%
      map_dfc( ~ ifelse(is.na(.x), 0L, .x)) %>%
      ufs::multiResponse() %>%
      as_tibble

    # Separate the totals for later use when drawing tables
    tot <- slice_tail(mult) %>%
      select(!last_col()) %>%
      as.character

    mult <- mult %>%
      filter(Option != "Total") %>%
      mutate(Option = opts) %>%
      filter(Option != "None") %>%
      mutate(Variable = Option) %>%
      relocate(Variable, .after = Option) %>%
      mutate(Variable = factor(Variable) %>%
               forcats::fct_reorder(Frequency, .desc = TRUE)) %>%
      arrange(desc(Frequency)) %>%
      select(!last_col(offset = 1L)) %>%
      mutate(across(last_col(), ~ round(.x, digits = 1)))

    if (data.only)
      return(mult)

    argslist <- list(...)
    addedArgs <- names(argslist)
    if (!('caption' %in% addedArgs))
      myFlextable(mult, ...)
    else
      myFlextable(mult) %>%
      set_caption(caption = argslist$caption)
  }







#' @rdname tables
#'
#' @import dplyr
#' @import flextable
#' @importFrom labelled var_label
#'
#' @param data The data
#' @param x,y An integer or character vector of length \code{1L} for selecting
#' a column from \code{data}.
#' @param table.only Logical
#' @param data.only Logical
#'
#' @return An object of class \code{flextable} (or when \code{table.only} is
#' \code{TRUE} an object of class \code{table}).
#'
#' @export
table_singleopt <-
  function(data,
           x,
           y = NULL,
           table.only = FALSE,
           data.only = FALSE) {
    if (!is.data.frame(data))
      stop("'.data' should be a data frame")
    if (table.only && data.only)
      stop("'table.only' and 'data.only' cannot both be TRUE at the same time")
    x <- data[[x]]
    if (!is.null(y))
      y <- data[[y]]
    lbs <- labelled::var_label(x)

    t <- if (is.null(y))
      table(x)
    else
      table(x, y)

    if (table.only)
      return(t)

    d <- t %>%
      as.data.frame() %>%
      arrange(desc(Freq)) %>%
      mutate(Percentage = round(Freq/sum(Freq)*100, 1)) %>%
      rename(Variable = x)

    if (data.only)
      return(d)

    flextable(d) %>%
      set_caption(caption = lbs) %>%
      theme_box %>%
      autofit()
  }



#
# ..makePercentage <- function(ref, rounding = 1L) {
#   if (is.symbol(ref))
#     ref <- enquo(ref)
#   else
#     stopifnot(is.character(ref))
#   stopifnot(is.numeric(rounding))
#
#   round(!!ref/sum(ref)*100, 1)
# }




# Make a function for creating abridged options
#' @import stringr
.abridgeOptions <- function(x, redcap = getOption('data.on.redcap')) {
  stopifnot(is.character(x))
  if (is.null(redcap))
    redcap <- TRUE
  nx <- x %>%
    str_remove_all("(\\s)(or|to|of|in|the|a|by)")
  if (redcap)
    nx <- str_replace(nx, "(([[:alpha:]]+\\s){3})(.+)", "\\1")
  nx %>%
    str_replace('and', '&') %>%
    str_remove(".+\\s?/\\s?") %>%
    str_trim %>%
    str_squish %>%
    str_to_title
}








#' Make a frequency tabulation that for variables with Yes/No responses
#'
#' @rdname tables
#'
#' @param data The data
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


#' @importFrom dplyr select
#' @importFrom flextable flextable
myFlextable <- function(data, ...) {
  stopifnot(is.data.frame(data))
  if (all(!grepl("^(Yes|No)$", colnames(data))))
    data <- select(data, -Option)
  if (...length() > 0L)
    olddef <- set_flextable_defaults(...)
  flextable(data)
}


# Generates a data dictionary
# This function is defined to avoid repetitious calls to `generate_dictonary`
# and to make room for the eventual implementation of a caching mechanism
#' @importFrom labelled generate_dictionary
makeDictionary <- function(x)
{
  stopifnot(is.data.frame(x))
  labelled::generate_dictionary(x)
}



#' Retrieve the labels from the dictionary
#'
#' @param dictionary A data dictionary
#' @param indices Numeric vector; indices of the variables whose labels' values
#' will be used
#' @param use.regex Whether regular expressions will be used to extract the
#' value(s)
#' @param ... Arguments passed to internal functions. These include
#' \code{redcap}, a logical vector indicating whether the project data are on
#' REDCap or not. Others are \code{multiresponse} ("are the labels for
#' multiresponse questions?") and \code{ignore.case} - used as in
#' \code{base::grep}.
#'
#' @details This function is used differently across the various GBV projects.
#' Specifically, the patterns used in the projects that had data hosted on
#' REDCap are peculiar. For other projects, we either set
#' \code{getOption('use.regex')} to \code{FALSE} or specify a pattern for
#' extraction.
#'
#' @return A character vector of the labels, possibly modified internally.
#'
#'
#' @export
get_value_labels <-
  function(dictionary,
           indices,
           use.regex = getOption("use.regex"),
           ...) {
    stopifnot(is.data.frame(dictionary))
    lbls <- dictionary$label[indices]
    if (!use.regex)
      return(lbls)
    .extractComponent(lbls, 'value', ...)
  }





## TODO: Deprecate
#' Get Variable Labels
#'
#' Retrieve all the labels of designated columns of a data frame
#'
#' @param data A data frame
#' @param ind A numeric or character vector representing columns. When no
#' argument is supplied, all column indices are used for retrieval.
#'
#' @importFrom labelled var_label
#' @importFrom purrr map_chr
#'
#' @return A character vector of label names
#'
#' @export
get_var_labels <- function(data, ind = NA_integer_) {
  if (!is.data.frame(data))
    stop("'data' should be of class data.frame")

  if (!is.numeric(ind) && !is.character(ind))
    stop(paste("The data cannot be subset with an object of type", typeof(ind)))

  if (is.character(ind)) {
    nm <- names(data)

    if (!all(ind %in% nm))
      stop("Any strings must be valid variable names")

    ind <- match(ind, nm)
  }

  dfi <- seq_along(data)

  if (all(is.na(ind)))
    ind <- dfi

  if (any(!ind %in% dfi))
    stop("Out-of-bounds or missing index")

  purrr::map_chr(ind, ~ var_label(data[[.x]]))
}







# Extracts various components from a character vector and these components
# are defined by regular expressions
.extractComponent <-
  function(label,
           component = c('number', 'question', 'value'),
           ignore.case = TRUE,
           multiresponse = TRUE,
           redcap = TRUE) {
    last <- if (redcap) "\\4" else "\\2"
    component <- match.arg(component)
    plc <- switch(component,
                  number = '\\1',
                  question = '\\2',
                  value = last)
    rgx <- .multichoiceRegex()
    if (!redcap)
      return(sub(rgx, plc, label))
    if (multiresponse) {
      if (!any(grepl(rgx, label, ignore.case = ignore.case)))
        stop(
          sQuote(label),
          'is not a valid string with multi-response labels used on REDCap'
        )
    }
    else
      rgx <- substr(rgx, 1, gregexpr("\\(", rgx)[[1]][3] -1)

    sub(rgx, plc, label, ignore.case)
  }






# Generates the regular expression patterns that are used to
# identify/extract labels applicable to the options of
# multi-response questions
## So far, guaranteed only to apply to the projects that stored
## the data on the REDCap servers
.multichoiceRegex <- function(redcap = getOption("data.on.redcap")) {
  val <- '(.+)$'
  if (redcap || is.null(redcap)) {
    lead <- "(^q\\d{1,2}[a-b]?\\.?|^Organization|^Facility'?\\.?s)"
    mid <- '(.+)(.choice.)'
    return(paste0(lead, mid, val))
  }
 paste0("(.+\\s/\\s)", val)
}






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





#' Generate Auto-numbers for Tables
#'
#' @param bookmark A string to represent the bookmark used in a given document
#' @param ... Other arguments passed on to \code{\link[officer]{run_autonum}}.
#'
#' @return See the documentation for \code{officer::run_autonum}.
#'
#' @export
my_autonum <- function(bookmark = NULL, ...) {
  if (is.null(bookmark))
    bookmark <- "iufmp"
  if (!is.character(bookmark) || length(bookmark) != 1L)
    stop("'bookmark' should be a valid string")
  if (is.na(bookmark))
    stop("Cannot use a missing value to create a bookmark")
  officer::run_autonum('tab', bkm = bookmark)
}
