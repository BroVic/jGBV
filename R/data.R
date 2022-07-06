# Source file: data.R
#
# MIT License
#
# Copyright (c) 2022 Victor Ordu

globalVariables(c("refdir_update", "age_grp_served"))

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






#' Save/Load Data To Or From The Project Database(s)
#'
#' @param path The path to the data(base) file.
#' @param df A project data frame that is to be saved.
#' @param state The State for which the data are retrieved.
#' @param type The kind of data being retrieved (see \emph{Details})
#'
#' @return For \code{load_data},aA data frame with individual columns of
#' class "labelled".
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
  .assertStateAndDbpath(state, path)
  type <- match.arg(type)
  con <- dbConnect(SQLite(), path)
  on.exit(dbDisconnect(con))
  df <-
    dbReadTable(con, .tblName(state, type, "cleaned"), check.names = FALSE)
  if (type == 'services') {
    varlist <- as.list(getOption("jgbv.new.varnames"))
    df <- suppressWarnings(.processDateTime(df, varlist))
    df <- .setFactors(df)
  }
  qry <- sprintf("SELECT label FROM %s;", .tblName(state, type, "labels"))
  labs <- unlist(dbGetQuery(con, qry))
  var_label(df) <- labs
  df
}







#' @rdname load_data
#'
#' @import RSQLite
#' @importFrom labelled generate_dictionary
#'
#' @export
save_table <- function(df, state, type = c("services", "capacity"), path) {
  if(!is.data.frame(df))
    stop("Expected 'data' to be a data frame")
  .assertStateAndDbpath(state, path)
  type <- match.arg(type)
  state <- tolower(state)

  tryCatch({
    cat("Saving to database... ")
    con <- dbConnect(SQLite(), path)
    dbWriteTable(con, .tblName(state, type, "cleaned"), df, overwrite = TRUE)

    dic <- generate_dictionary(df)
    dic <- as.data.frame(dic[, 2:3])  # TODO: Consider expanding the table
    dbWriteTable(con, .tblName(state, type, "labels"), dic, overwrite = TRUE)
    cat("Done\n")
  },
  error = function(e) {
    cat("Failed\n")
    warning(e)
  },
  finally = dbDisconnect(con)
  )
}




#' @importFrom naijR is_state
.assertStateAndDbpath <- function(s, db) {
  if (!is_state(s))
    stop("'state' is not a valid State")

  if (!file.exists(db))
    stop("The file", sQuote(db, q = FALSE), "does not exist")
}


# Constructs the name of a database table
.tblName <- function(state, type, str) {
  paste(state, type, str, sep = '_')
}


#' @import dplyr
#' @importFrom stringr str_remove
.processDateTime <- function(data, vars) {
  stopifnot(is.data.frame(data), is.list(vars))
  data %>%
    mutate(across(
      all_of(c(vars$start, vars$end)),
      ~ as.POSIXct(strptime(.x, format = "%Y-%m-%dT%H:%M:%OS"))
    )) %>%
    mutate(across(
      all_of(c(vars$open.time, vars$close.time)),
      ~ str_remove(.x, "\\:00\\.000\\+01:00$")
    )) %>%
    mutate(across(all_of(c(vars$today, vars$opstart, vars$gbvstart)), as.Date)) %>%
    mutate(across(contains(vars$close.time), .dealWithBadPmEntries))
}




.dealWithBadPmEntries <- function(x) {
  stopifnot(is.character(x))
  .f <- function(str) {
    t <- strptime(str, format = "%H:%M")
    if (is.na(t) || as.numeric(format(as.POSIXct(t), "%H")) > 11)
      return(str)
    t <- t + (12 * 60 ^ 2)
    strftime(t, "%H:%M")
  }
  vapply(x, .f, character(1), USE.NAMES = FALSE)
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
