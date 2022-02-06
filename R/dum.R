#' Make a dummy table
#'
#' @param dic A \code{data.frame} containing the data dictionary, as generated
#' by \code{\link[labelled]{generate_dictionary}}.
#' @param rows Numeric vector of the rows of interest in the dictionary
#' @param cols Character vector of columns that we want to use as focus
#' @param top.header Additional header row
#' @param multiresponse Boolean; whether table is for multi-response questions
#' @param caption The caption of the table
#' @param ... Other parameters for internal use by the internal function
#' \code{make_table}
#'
#' @import stringr
#' @import flextable
#'
#' @export
build_dummy_flextable <-
  function(dic = NULL,
           rows = NULL,
           cols,
           top.header,
           multiresponse = T,
           caption = NULL,
           ...) {
    if (is.null(dic)) {
      hdrs <- cols
    }
    else {
      stopifnot(is.data.frame(dic))
      if (multiresponse && is.numeric(rows)) {
        hdrs <- get_hdrs(dic, rows)
        capPat <- "^.+/"
        caption <- hdrs[1] %>%
          str_extract(capPat) %>%
          str_remove("/$")
        hdrs <- hdrs %>%
          str_remove(capPat) %>%
          str_trim()
      }
    }
    df <- if (is.character(rows))
      make_table_df(cols, rows, ...)
    else if (multiresponse)
      make_multiresp(hdrs)
    else
      make_table_df(hdrs, ...)

    ft <- flextable(df) %>%
      theme_box()

    if (!is.null(caption))
      ft <- set_caption(ft, caption = caption)

    if (missing(top.header))
      return(ft)

    add_header_lines(ft, top.header)
  }





# Get column headers
get_hdrs <- function(dic, rows, use.regex = TRUE)
{
  stopifnot(is.data.frame(dic))
  vec <- dic[['label']][rows]
  if (use.regex)
    .extractComponent(vec, 'value')
  else
    vec
}






# Make table base data frame
#' @import dplyr
#' @importFrom rlang ensym
make_table_df <- function(cols, rows, name = NULL)
{
  stopifnot(is.character(cols))

  df <- matrix('', ncol = length(cols), nrow = length(rows)) %>%
    data.frame %>%
    structure(names = cols) %>%
    bind_cols(as_tibble(rows)) %>%
    relocate(value)

  if (is.null(name)) {
    name <- if (all(naijR::is_lga(rows))) {
      'LGA'
    }
    else {
      warning("'name' was not supplied, so an arbitrary term was used",
              call. = FALSE)
      "Variable"
    }
  }
  nm <- rlang::ensym(name)
  rename(df, !!nm := value)
}






#' Make a multi-response dummy table using established headers
#'
#' @param hdrs Character vector with the names of the labels for each response
#'
#' @return A data frame with summary of multiple responses
#'
#' @export
make_multiresp <- function(hdrs) {
  dd <- as.data.frame(as.list(rep(NA, length(hdrs))))
  names(dd) <- hdrs
  ufs::multiResponse(dd)
}












#' Make a kable table
#'
#' Create a table formatted for markdown
#'
#' @param row A vector for the rows.
#' @param column A character vector of the column names
#' @param opt The options for the dummy table
#'
#' @importFrom knitr kable
#'
#' @export
make_kable <- function(row, column, opt = NULL) {
  if (inherits(row, "Tools"))
    row <- row[!row == "Referral"]
  if (is.null(opt))
    opt <- c(" (N) ", " (%) ")
  else
    opt <- opt
  len.column <- length(column)
  newCol <- rep(opt, len.column)
  ncols <- length(newCol)
  nrows <- length(row)
  df <-
    as.data.frame(matrix(
      data = rep("", nrows * ncols),
      nrow = nrows,
      dimnames = list(row, newCol)
    ),
    stringsAsFactors = FALSE)
  grpSz <- length(opt)
  grps <- structure(rep(grpSz, len.column), names = column)

  kable(df) %>%
    kableExtra::kable_styling("striped") %>%
    kableExtra::add_header_above(c("", grps))
}







#' Make a Yes/No Dummy Table
#'
#' @param columns The colums to be used
#' @param all.lgas The LGAs in the State
#'
#' @export
make_just_yesno <- function(columns, all.lgas) {
  make_kable(all.lgas, columns , opt = c("Yes", "No"))
}
