# Source file:  capneed-funs.R
#
# License: MIT
#
# Copyright (c) 2022 Victor Ordu

globalVariables("perc")

## Helper functions for use in the script that has externalized R code
## that is put into play for the creation of various output documents
## based on R Markdown sources.

# ===
## The Functions:
# ===
# Gets the index of the column for 'Type of Service' and that of the first
# column of the changeable portion of the respective tables


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
  if (!is.data.frame(data))
    stop("'data' is not a data frame")
  if (!is.character(labs) && is.character(colour))
    stop("'labs' and 'colour' must be of type 'character'")
  if (!is.logical(table) && length(table == 1L))
    stop("'table' must be a logical vector or length 1")
  if (!nrow(data))
    return()
  ## last column of the fixed part of the table
  bkm <- .tableBookmarks(data)
  fixed <- bkm["type.index"]
  coord.start <- bkm['coord.index']
  d <- data %>%
    select(-c(coord.start:last_col())) %>%
    select(-c(seq_len(fixed))) %>%
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
#' @param dt A data frame for the type of service provider
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
makeTable <- function(dt, caption = character(1)) {
  if (!is.data.frame(dt))
    stop("'dt' is not a data frame")
  if (!is.character(caption) || length(caption) > 1L)
    stop("'caption' must be a character vector of length 1")
  if (!nrow(dt))
    return()
  train.cols <- getTrainingsIndex(dt)
  scales <-
    scales::col_factor(palette = c("darkgreen", "red"),
                       levels = c("Yes", "No"))
  bkm <- .tableBookmarks(dt)
  dt %>%
    select(-10) %>%   # remove the 'type of service' column
    flextable %>%
    add_header_row(
      values = c("Facility Info", "Areas of Training", "Coordination"),
      colwidths = c(
        bkm['type.index'] - 1L,
        length(train.cols),
        length(dt) - bkm['coord.index'] + 1)
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


.tableBookmarks <- function(data) {
  stopifnot(is.data.frame(data))
  cols <- names(data)
  c(type.index = grep("type", cols, ignore.case = TRUE)[1],
    coord.index = grep("coordination meeting", cols, ignore.case = TRUE)[1])
}







getTrainingsIndex <- function(dt) {
  stopifnot(is.data.frame(dt))
  sentinels <- .tableBookmarks(dt)
  iTrainFirst <- sentinels['type.index'] + 1
  numTrainings <- sentinels['coord.index'] - iTrainFirst
  seq(iTrainFirst, length.out = numTrainings)
}






#' Filter the Data and Select Only Columns Relevant to Capacity Assessment
#'
#' @import dplyr
#' @importFrom magrittr %<>%
#' @importFrom purrr map_lgl
#' @importFrom stringr str_remove
#' @importFrom stringr str_replace
#' @importFrom stringr str_squish
#' @importFrom stringr str_trim
#'
#' @param df The data
#' @param service A string representing the service being filtered.
#'
#' @return A data frame with data related to a particular service category.
#'
#' @export
filterAndSelect <- function(df, service) {
  stopifnot(is.data.frame(df), is.character(service))

  # Identifies columns that are empty
  .allEmpty <- function(x) {
    if (is.character(x))
      all(x == "-")
    else if (is.logical(x))
      all(is.na(x))
    else FALSE
  }

  cols <- names(df)
  t <- .tableBookmarks(df)['type.index']
  df <- df %>%
    filter(grepl(service, .data[[cols[[t]]]], ignore.case = TRUE)) %>%
    # select(!matches("train") | matches(service)) %>%   # applied to NEDC. Review!
    select(which(!map_lgl(., .allEmpty)))  # most NAs were earlier converted to '-'

  # Carry out this step here because when the entire complement of
  # columns are available, transformation is more challenging due to
  # the duplication of column names.
  colnames(df) %<>%    # pipe assignment
    {
      try(.[[grep("LGA", .)]] <- "LGA")
      try(.[[grep("Name.+(Facility|Organization)", ., ignore.case = TRUE)]] <- "Name of Facility")
      try(.[[grep("focal", .)]] <- "GBV Focal Person?")
      try(.[[grep("Position", .)]] <- "Designation")
      try(.[[grep("^Age", .)]] <- "Age Range")
      # try(.[[grep("Qualification", .)]] <- "Qualifications")
      try(.[[grep("Phone", .)]] <- "Phone")
      try(.[[t]] <- "Type of Organization")
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
  if (!is.data.frame(df))
    stop("'df' is not a data frame")
  if (!nrow(df))
    return()
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
#'
#' @export
fixOffset <- function(x, ...)
  UseMethod("fixOffset")

#' @rdname fixOffset
#'
#' @export
fixOffset.numeric <- function(x, ...) {
  df <- list(...)$df
  stopifnot(is.data.frame(df))
  .tableBookmarks(df)['type.index'] + x
}

#' @rdname fixOffset
#'
#' @export
fixOffset.character <- function(x, ...) {
  df <- list(...)$df
  stopifnot(is.data.frame(df))
  grep(x, names(df), ignore.case = TRUE)
}

#' @rdname fixOffset
#'
#' @export
fixOffset.default <- function(x, ...) {"Unsupported type"}




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





