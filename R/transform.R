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
