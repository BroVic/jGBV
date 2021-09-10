#' Modify Strings
#'
#' Creates spaces for underscores and changes text to title case
#'
#' @param str A character vector of strings that will be modified.
#'
#' @import stringr
#'
#' @return A character vector of the same length as \code{str}
#'
#' @export
spaceAndTitle <- function(str) {
  stopifnot(is.character(str))
  str %>%
    str_replace_all("_", " ") %>%
    str_to_title %>%
    str_trim
}
