# Source file:  capneed-funs.R
#
# License: MIT
#
# Copyright (c) 2022 Victor Ordu

## Helper functions for use in the script that has externalized R code
## that is put into play for the creation of various output documents
## based on R Markdown sources.

# ===
## The Functions:
# ===
# Gets the index of the column for 'Type of Service' and that of the first
# column of the changeable portion of the respective tables

globalVariables("allcap")


.tableBookmarks <- function(data) {
  stopifnot(is.data.frame(data))
  cols <- names(data)
  c(type.index = grep("type", cols, ignore.case = TRUE)[1])
}


#' Draw Plot of Service Provider Capacity
#'
#' Draws the plot for a given service category; thus, 'data' is
#' a prefiltered data frame
#'
#' @param data A data frame of a service category
#' @param labs The labels to be used for the plot
#' @param colour The fill of the bars
#' @param annot The annotation
#' @param table logical; Whether to return the data for tabulation instead
#'
#' @import dplyr
#' @import ggplot2
#' @import ggthemes
#' @importFrom tidyr pivot_longer
#'
#' @export
makePlot <- function(data, labs, colour, annot = waiver(), table = FALSE) {
  ## last column of the fixed part of the table
  fixed <- .tableBookmarks(data)["type.index"]
  d <- data %>%
    select(-c(seq_len(fixed))) %>%
    select(seq_len(length(.) - 3)) %>%
    mutate(across(everything(), ~ ifelse(.x == "-", NA_character_, .x))) %>%
    pivot_longer(everything()) %>%
    mutate(value = ifelse(value == "No", 0L, 1L))


  ii <- getTrainingsIndex(data)
  dt <- d %>%
    mutate(name = factor(name, labels = labs)) %>%
    filter(value != 0) %>%
    mutate(name = forcats::fct_infreq(name)) %>%
    group_by(name) %>%
    summarise(n = n()) %>%
    mutate(perc = round(n / nrow(data) * 100, 1))

  if (table)
    return(dt)

  dt %>%
    ggplot(aes(name, perc)) +
    geom_col(fill = colour) +
    labs(
      x = "Trainings Done",
      y = sprintf("Respondents (N = %d)", nrow(data)),
      caption = annot
    ) +
    scale_y_continuous(labels = scales::percent_format(accuracy = 1, scale = 1)) +
    coord_flip() +
    ggthemes::theme_gdocs() +
    theme(axis.title = element_text(face = "bold.italic"),
          axis.title.x = element_text(vjust = 0),
          axis.title.y = element_text(vjust = 1),
          plot.margin = unit(c(1, 1, 1.5, 1.2), "cm"),
          plot.caption = element_text(face = 'italic', size = 7))
}



#' Make a table of Capacity (Training) Needs
#'
#' @param service A string for the type of service provider
#' @param caption A caption for the table
#'
#' @importFrom magrittr %>%
#' @importFrom scales col_factor
#' @import flextable
#' @import dplyr
#'
#' @note This function depends on a global variable \code{allcap}, which is
#' found in the workspace of the project and activated by the appropropriate
#' script. Hence it is for now unfortunately tied to is.
#'
#' @export
makeTable <- function(service, caption = NULL) {
  stopifnot(is.character(service))
  # 'allcap' is in the parent environment
  dt <- allcap[[service]]
  train.cols <- getTrainingsIndex(dt)
  scales <-
    scales::col_factor(palette = c("darkgreen", "red"),
                       levels = c("Yes", "No"))
  dt %>%
    select(-10) %>%   # remove the 'type of service' column
    flextable %>%
    add_header_row(
      values = c("Facility Info", "Areas of Training", "Coordination"),
      colwidths = c(.tableBookmarks(dt) - 1L, length(train.cols), 3L)
    ) %>%
    theme_box %>%
    set_caption(caption) %>%
    align(1L, align = 'center', part = 'header') %>%
    bold(1L, part = 'header') %>%
    fontsize(size = 7, part= "all") %>%
    color(color = scales, j = train.cols - 1L) %>% # needed to shift indices by 1
    bold(j = train.cols) %>%
    set_formatter_type
}






getTrainingsIndex <- function(dt) {
  stopifnot(is.data.frame(dt))
  sentinel <- .tableBookmarks(dt)
  len <- ncol(dt) - (sentinel + 3L)
  seq(sentinel + 1L, length.out = len)
}






#' Filter the Data and Select Only Columns Relevant to Capacity Assessment
#'
#'
#' @import stringr
#' @import dplyr
#'
#' @param df The data
#' @param service A string representing the service being filtered.
#'
#' @export
filterAndSelect <- function(df, service) {
  stopifnot(is.data.frame(df), is.character(service))
  cols <- names(df)
  t <- .tableBookmarks(df)['type.index']
  df <- df %>%
    filter(grepl(service, .data[[cols[[t]]]], ignore.case = TRUE)) %>%
    select(!matches("train") | matches(service))
  # %>%
  #   select(!where( ~ all(.x == "-")))   # NAs were earlier converted to '-'
  #
  # Carry out this step here because when the entire complement of
  # columns are available, transformation is impossible due to
  # the duplication of column names.
  colnames(df) %<>%    # pipe assignment
    {
      .[[1]] <- "LGA"
      .[[2]] <- "Name of Facility"
      .[[4]] <- "GBV Focal Person?"
      .[[5]] <- "Designation"
      .[[6]] <- "Age Range"
      .[[8]] <- "Qualifications"
      .[[9]] <- "Phone"
      .[[10]] <- "Type of Organization"
      .
    } %>%
    str_remove("\\.+\\d{1,2}$") %>%
    str_remove("^Q\\d\\.") %>%
    str_remove("^Responding/") %>%
    str_remove("approach$") %>%
    str_replace("Sex/Gender", "Gender") %>%
    str_trim %>%
    str_squish
  df
}







#' Generate Inline Statisics for the Report
#'
#' @param df The data
#' @param stat The statistic to be used
#' @param var The variable of interest.
#'
#' @export
statGBV <- function(df, stat = c("sum", "perc", 'total'), var = character()) {
  stopifnot(is.data.frame(df))
  stat <- match.arg(stat)
  i <- if (is.character(var)) {
    var <- match.arg(var, c('gbv', 'gender'))
    grep(var, names(df), ignore.case = TRUE)[1]
  } else
    var
  stopifnot(is.numeric(i))
  if (length(i) == 0L)
    stop("grep: No index found", call. = TRUE)
  col <- df[[i]]
  v <- tolower(col) == "yes"
  if (var == 'gender')
    v <- col == "Female"

  switch(stat,
         sum = sum(v),
         perc = paste0(round(mean(v) * 100, 1), '%'),
         total = nrow(df))
}







#' Fix offsets for some States
#'
#' @param x A vector
#' @param ... Other arguments for methods
#' @param df A data frame
#'
#' @export
fixOffset <- function(x, ...)
  UseMethod("fixOffset")

#' @rdname fixOffset
#'
#' @export
fixOffset.numeric <- function(x) {
  .tableBookmarks(capdata)['type.index'] + x
}

#' @rdname fixOffset
#'
#' @export
fixOffset.character <- function(x, df) {
  stopifnot(is.data.frame(df))
  grep(x, names(df), ignore.case = TRUE)
}

#' @rdname fixOffset
#'
#' @export
fixOffset.default <- function(x) {"Unsupported type"}




#' Make a Table Specific to the Capacity Assessment Report
#'
#' @param data A data frame with the required data
#'
#' @import flextable
#'
#' @export
flex_datatable <- function(data) {
  stopifnot(is.data.frame(data))
  flextable(data) %>%
    set_header_labels(name = "Training Received", n = "Respondents", perc = "Perc.") %>%
    theme_box %>%
    width(width = c(2, 1, 1))
}





