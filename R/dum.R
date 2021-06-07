#' Make a dummy table
#'
#' @param dic A \code{data.frame} containing the data dictionary
#' @param rows Numeric vector of the rows of interest in the dictionary
#' @param cols Character vector of columns that we want to use as focus
#' @param top.header Additional header row
#' @param multiresponse Boolean; whether table is for multi-response questions
#' @param caption The caption of the table
#' @param ... Other parameters for internal use by the internal function
#' \code{make_table}
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
      make_table_df(hdrs, rows, ...)
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
