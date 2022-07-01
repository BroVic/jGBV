# Source file: data.R
#
# MIT License
#
# Copyright (c) 2022 Victor Ordu

#' Read Data From A Database
#'
#' Reads data from the database and may carry out some minor transformations
#' in the process.
#'
#' @param db The path to the database file.
#' @param tbl The name of the table to be accessed.
#' @param ... Additional arguments passed on to \code{DBI::dbConnect}.
#'
#' @importFrom RSQLite dbConnect
#' @importFrom RSQLite dbDisconnect
#' @importFrom RSQLite dbReadTable
#'
#' @export
read_from_db <- function(db, tbl, ...) {
  if (!file.exists(db))
    stop("The database file does not exist")
  if (!is.character(tbl))
    stop("'tbl' must be a string")
  if (length(tbl) > 1L) {
    tbl <- tbl[1]
    warning("Only the first element of 'tbl' was used")
  }
  con <- dbConnect(SQLite(), db, ...)
  on.exit(dbDisconnect(con))
  dbReadTable(con, tbl)
}






#' Loads data from the project database(s)
#'
#' @param path The path to the data(base) file.
#' @param state The State for which the data are retrieved.
#' @param type The kind of data being retrieved (see \italics{Details})
#' @param ... Arguments for methods
#'
#' @details There are two possible values for the \code{type} argument:
#' \code{services} for retrieving data on GBV services mapping and
#' \code{capacity} for data on capacity assessment.
#'
#' @import RSQLite
#' @importFrom labelled var_label
#'
#' @export
load_data <- function(path, state, type = c("services", "capacity")) {
  state <- match.arg(state, getOption("jgbv.project.states"))
  type <- match.arg(type)
  con <- dbConnect(SQLite(), path)
  on.exit(dbDisconnect(con))
  tbl.main <- paste(state, type, "cleaned", sep = '_')
  tbl.lbls <- paste(state, type, "labels", sep = "_")
  df <- dbReadTable(con, tbl.main, check.names = FALSE)
  if (type == 'services') {
    df <- suppressWarnings(.processDateTime(df, ...))
    df <- .setFactors(df)
  }
  qry <- sprintf("SELECT label FROM %s;", tbl.lbls)
  labs <- unlist(dbGetQuery(con, qry))
  var_label(df) <- labs
  df
}




#' @import dplyr
#' @importFrom stringr str_remove
.processDateTime <- function(data, vrs) {
  stopifnot(is.data.frame(data), is.list(vrs))
  data %>%
    mutate(across(
      all_of(c(vrs$start, vrs$end)),
      ~ as.POSIXct(strptime(.x, format = "%Y-%m-%dT%H:%M:%OS"))
    )) %>%
    mutate(across(
      all_of(c(vrs$open.time, vrs$close.time)),
      ~ str_remove(.x, "\\:00\\.000\\+01:00$")
    )) %>%
    mutate(across(all_of(c(vrs$today, vrs$opstart, vrs$gbvstart)), as.Date)) %>%
    mutate(across(contains(vrs$close.time), .dealWithBadPmEntries))
}





#' @importFrom  dplyr mutate
.setFactors <- function(data) {
  stopifnot(is.data.frame(data))
  data %>%
    mutate(refdir_update = factor(
      refdir_update,
      levels = c(
        "Every six months or less",
        "Every year",
        "More than a year",
        "It has never been updated"
      ),
      ordered = TRUE
    )) %>%
    mutate(age_grp_served = factor(
      age_grp_served,
      levels = c("Adults and children", "Only adults ", "Only children ")
    ))
}
