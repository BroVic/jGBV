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
#' @param vars A vector of variable names.Defaults to names provided via
#' \code{options}.
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
load_data <- function(path,
                      state,
                      type = c("services", "capacity"),
                      vars = getOption("jgbv.new.varnames")) {
  .assertStateAndDbpath(state, path)
  type <- match.arg(type)
  if (!is.character(vars))
    stop("'vars' must be a character vector")
  con <- dbConnect(SQLite(), path)
  on.exit(dbDisconnect(con))
  df <-
    dbReadTable(con, .tblName(state, type, "cleaned"), check.names = FALSE)

  if (type == 'services') {
    if (!matchDfWithVarsLength(df, vars))
      stop("Length of 'df' and 'vars' do not match")
    df <- suppressWarnings(.processDateTime(df, as.list(vars)))
    df <- .setFactors(df)
  }
  else
    stop("No implementation for ", sQuote(type))
  qry <-
    sprintf("SELECT label FROM %s;", .tblName(state, type, "labels"))
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

  if (!file.exists(path)) {

    message("The database file ",
            sQuote(path),
            " does not exist.",
            "Will attempt to create one.")

    tryCatch({
      # create bank database
      con <- RSQLite::dbConnect(RSQLite::SQLite(), path)
      RSQLite::dbDisconnect(con)
    },
    error = function(e)
      warning(e, call. = FALSE))
  }

  tryCatch({
    message("Saving to database... ", appendLF = FALSE)
    con <- dbConnect(SQLite(), path)
    dbWriteTable(con, .tblName(state, type, "cleaned"), df, overwrite = TRUE)

    dic <- generate_dictionary(df)
    dic <-
      as.data.frame(dic[, 2:3])  # TODO: Consider expanding the table
    dbWriteTable(con, .tblName(state, type, "labels"), dic, overwrite = TRUE)
    message("Done")
  },
  error = function(e) {
    message("Failed")
    warning(e)
  },
  finally = dbDisconnect(con))
}




#' Manipulate A Vector For Getting Correct Date-Time Types
#'
#' @param x The vector to be modified.
#' @param date.string Whether to return a date(-time) sting or and object.
#'
#' @return The modified vector by default (when \code{date.string} is
#' \code{TRUE}); otherwise an object of class \code{Date} or \code{POSIXct}.
#'
#' @importFrom stringr str_trim
#' @export
make_date <- function(x, date.string = TRUE) {
  if (!is.character(x) && !is.numeric(x))
    stop("'x' must be either of type 'character' or 'numeric'")

  # Converts objects to character vectors, if requested
  .ds <- function(.x) {
    if (date.string)
      .x <- as.character(.x)
    .x
  }
  cent.origin <- "1899-12-30"
  epoch.unix <- "1970-01-01"

  if (is.character(x)) {
    x <- stringr::str_trim(x)
    hasIsoDate <- grepl("^\\d{4}(\\-\\d{2}){2}$", na.exclude(x))
    if (all(hasIsoDate))
      return(.ds(as.Date(x)))
    if (any(hasIsoDate)) {
      if (all(grepl("^\\d{4,}$", x[!hasIsoDate])))
        x <-
          ifelse(!hasIsoDate,
                 as.Date(as.numeric(x), origin = cent.origin),
                 x)
      return(.ds(x))
    }
    x <- as.numeric(x)
  }
  if (all(x %/% 1e4 > 3))
    return(.ds(as.POSIXct(x, origin = epoch.unix)))
  .ds(as.Date(x, origin = cent.origin))
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
#' @importFrom stats na.exclude
#' @importFrom stringr str_remove
.processDateTime <- function(data, vars) {
  stopifnot(is.data.frame(data), is.list(vars))

  df <- data %>%
    mutate(across(
      all_of(c(vars$start, vars$end)),
      ~ as.POSIXct(strptime(.x, format = "%Y-%m-%dT%H:%M:%OS"))
    )) %>%
    mutate(across(
      all_of(c(vars$open.time, vars$close.time)),
      ~ str_remove(.x, "\\:00\\.000\\+01:00$")
    )) %>%
    mutate(
      across(all_of(c(vars$today, vars$opstart, vars$gbvstart)), make_date)
    ) %>%
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
