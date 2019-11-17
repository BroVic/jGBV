#' Extracting code from REDCap Script  File
#'
#'
#' @name extract
NULL

#' @rdname extract
#'
#' @param rgx A reguar expression
#' @param txt A portion of text, supplied as a character vector
#'
#' @import stringr
#'
#' @return A character vector with the extracted code
#'
#' @export
extract_code <- function(rgx, txt) {
  stopifnot(is.character(rgx))
  stopifnot(is.character(txt))

  txt %>%
    str_subset(rgx) %>%
    str_replace(rgx, "\\2")
}



#' @rdname extract
#'
#' @param str A character string containing the code
#'
#' @return A vector from the evaluated code
#'
#' @note This is an internal function but is exported to enable use in
#' the main RAAMP_GBV project.
#'
#' @export
extract_vector <- function(str) {
  stopifnot(exprs =
              {
                is.character(str)
                isFALSE(anyNA(str))
              })
  str <- eval(parse(text = str))
  gsub("\\s?", "", str)
}
