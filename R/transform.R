# Source file: transform.R
#
# MIT License
#
# Copyright (c) 2019 Victor Ordu

#' Manipulate Data with Multiple Choice Questions
#'
#' There were multiple-choice questions wherein more than one
#' option was permissible i.e. check-list kind of questions. These questions
#' needed to be brought together into a single variable at some point in the
#' analysis. After this was done, these options were brought together into a
#' list column. This function will expand that list column, creating new
#' columns - one for each response.
#'
#' @param data A data frame that contains the column to be collapsed
#' @param variable A character vector of length 1 representing the
#' name of the column to be extended
#' @param options The would-be levels of the resulting factor column i.e. the
#' question's values as they are to appear in a new factor.
#' @param multisectoral Logical. Whether work is on a combined data frame i.e.
#' the question cuts across various sectors of the analysis e.g. health, legal
#' aid, etc.
#' @param notReferral Logical. Whether not Referral Directory data
#'
#'
#' @importFrom stats reorder
#' @importFrom tidyr pivot_longer
#'
#' @return  A modified data frame with the choices now appearing as
#' levels of a factor.
#'
#' @export
prepare_extended_col <-
  function(data,
           variable,
           options,
           multisectoral = TRUE,
           notReferral = multisectoral) {
    selected <-
      c(lgas = "lgas",
        sector = "sector",
        variable = variable)

    if (isFALSE(multisectoral))
      selected <- selected[-2]     # Test this!

    start <- if (isFALSE(notReferral))
      2L
    else
      3L

    # handle exception for when sector column is not appropriate
    data <- select(data,!!selected)

    for (i in seq_along(options)) {
      data <- extend_single_listcol(data, variable, options[i])
    }

    data <- select(data, -variable)
    data <- pivot_longer(data, start:ncol(data))
    data <- mutate(data, name = factor(name))
    data <- mutate(data, name = reorder(name, desc(value)))
    data <- filter(data, value)
  }











#' collect Name of RAAMP State
#'
#' Collect the name of a given State for the purpose of using it to build a
#' given part of the project that is state-dependent.
#'
#' @details This function will collect input both from the shell and during
#' interactive R sessions. Also, if a State's name is supplied as an argument
#' to \code{Rscript}, it will read it and apply it as required.
#'
#' @export
input_state <- function() {
  prompt <- "Enter State: "

  state <- if (!interactive()) {
    args <- commandArgs(trailingOnly = TRUE)

    if (identical(args, character(0))) {
      cat(prompt)
      readLines("stdin", n = 1L)
    }
    else
      args
  } else
    readline(prompt = prompt)

  paste(state, collapse = " ")
}






#' Transform the data for a given sector
#'
#' @param data The data frame with the data for a given sector
#' @param regexes Regular expression defining patterns for manipulating
#' columns for multiple choice questions
#' @param options The choices
#'
#' @export
transform_sector_data <- function(data, regexes, options) {
  stopifnot(length(regexes) == length(options))

  map2(regexes, options, colCheck) %>%    # class constructor
    {
      collapse_multichoice(data, .)
    }
}











#' Selectively transform a variable into a factor
#'
#' @import labelled
#'
#' @param x An atomic vector, usually part of a data frame
#'
#' @importFrom labelled val_labels
#' @importFrom labelled var_label
#'
#'
#' @export
transform_to_factor <- function(x) {
  var.name <- var_label(x)
  val.lab.pair <- val_labels(x)

  x <- if (is.integer(x)) {
    if (!grepl("How many cases of GBV.+12 months", var.name, ignore.case = TRUE))
      factor(x, levels = val.lab.pair, labels = names(val.lab.pair))
    else
      x
  }
  else
    x
  var_label(x) <- var.name
  x
}




#' Transform data for "Checked/Unchecked"
#'
#' @param data The data frame
#' @param vec The given vector
#'
#' @importFrom dplyr mutate_at
#'
#' @export
transform_checked <- function(data, vec) {
  stopifnot(ncol(data) == length(vec))

  for(i in seq_along(vec)) {
    .var <- colnames(data)[i]
    data <- mutate_at(data, .var, ~if_else(.x == 1, vec[i], NA_character_))
  }
  data
}








#' Collapse Columns for Multiiple-Choice Questions
#'
#' @param data An object of class \code{data.frame}
#' @param details An object of class \code{colCheck}
#'
#' @return An updated data frame
#' @note All the newly created columns will be pushed to the right end of the
#' table.
#'
#' @export
collapse_multichoice <- function(data, details) {
  data %>%
    as_tibble %>%
    {
      for (i in seq_along(details)) {
        . <- pickout_cols_and_transform(., details[[i]])
      }
      invisible(.)
    }
}








## TODO: Write unit tests
#' Pick out a Colum and Transform it
#'
#' @param data A data frame
#' @param chklist An object of class \code{colCheck}
#'
#' @import dplyr
#' @importFrom rlang :=
#' @importFrom rlang !!
#' @importFrom rlang quo_name
#' @importFrom tidyr unite
#'
#' @export
pickout_cols_and_transform <- function(data, chklist) {
  columnnames <- colnames(data)
  varsIndex <- grep(chklist$regex, columnnames)
  if (length(varsIndex) != length(chklist$opts)) {
    stop("The indices of the data frame are ",
         sQuote(paste(varsIndex, collapse = " ")),
         " while the options for matching are ",
         sQuote(chklist$opts))
  }

  ## Fetch the name and label of variable #1
  firstIndex <- varsIndex[1]
  the.label <- var_label(data[[firstIndex]])
  new.column <- columnnames[firstIndex]
  var.name <- quo_name(new.column)

  edit <- data %>%
    select(varsIndex) %>%
    transform_checked(chklist$opts) %>%
    unite(new, na.rm = TRUE) %>%
    select(new) %>%
    {
      var_label(.$new) <- the.label
      .
    } %>%
    rename(!!var.name := new)

  # if (is.null(var_label(edit[[new.column]])))
  #   warning("Variable labels have been stripped")
  #
  data %>%
    select(-varsIndex) %>%
    bind_cols(edit)
}
