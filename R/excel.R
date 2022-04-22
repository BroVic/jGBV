globalVariables(c("days_open", "intervention", "services"))

#' Make the Referral Directory
#'
#' Creates a data frame of the referral directory of the State, which can be
#' used to create other more sylistically appealing tables
#'
#' @param df The cleaned data from the State.
#' @param indices A numeric vector of all the column indices that will be used to
#' create the tabulation.
#' @param serv.cols The columns that contain the interventions provided. Used either
#' as an atomic vector with indices, a regular expression or the actual column names.
#' by each category of service provider.
#' @param servtyp.pattern,day.pattern A regular expression that represents the
#' \code{colnames} of all those that represent the service category of a facility
#' and the day it operates, respectively.
#' @param namelist A named list with column names as follows:
#' \itemize{
#'   \item \strong{orgname} - Name of the organization.
#'   \item \strong{orgphone} - Phone number of the organization.
#'   \item \strong{gbvcontact} - Contact details of the GBV focal person.
#' }
#' In all instances, the value is \emph{the column name} for that variable.
#'
#' @return A modified data frame containing the information required of the
#' State's referral directory.
#'
#' @importFrom dplyr select
#' @importFrom dplyr rename
#' @importFrom dplyr mutate
#' @importFrom dplyr arrange
#' @importFrom dplyr relocate
#' @importFrom dplyr quo
#' @importFrom tidyr unite
#' @importFrom stats setNames
#'
#' @export
prep_ref_directory <-
  function (df,
            indices,
            serv.cols,
            servtyp.pattern,
            day.pattern,
            namelist)
  {
    stopifnot(is.data.frame(df))
    nms <- names(df)
    serv.names <- if (is.numeric(serv.cols))
      nms[serv.cols]
    else if (is.character(serv.cols)) {
      if (length(serv.cols) == 1L)
        grep(serv.cols, nms, value = TRUE)
      else
        serv.cols
    }
    else
      stop(quote(serv.cols), " cannot be of type ", typeof(serv.cols))
    df %>%
      select(all_of(indices)) %>%
      rename(name = !!quo(namelist$orgname)) %>%
      rename(phone = !!quo(namelist$orgphone)) %>%
      rename(contact_gbv = !!quo(namelist$gbvcontact)) %>%
      mutate(across(matches(servtyp.pattern), set_logical_with_label)) %>%
      rowwise() %>%
      mutate(num.intervention = sum(c_across(all_of(serv.names)), na.rm = TRUE)) %>%
      mutate(across(all_of(serv.names), set_logical_with_label)) %>%
      mutate(across(matches(day.pattern), set_logical_with_label)) %>%
      unite("intervention",
            all_of(serv.names),
            sep = ", \n",
            na.rm = TRUE) %>%
      mutate(intervention = gsub("\\n", "", intervention)) %>%
      unite("services",
            matches(servtyp.pattern),
            sep = ", ",
            na.rm = TRUE) %>%
      unite("days_open",
            matches(day.pattern),
            sep = ", ",
            na.rm = TRUE) %>%
      mutate(days_open = ifelse(
        grepl("Yes", !!quo(namelist$openaccess), ignore.case = TRUE),
        "-",
        days_open
      )) %>%
      arrange(!!quo(namelist$lga), !!quo(namelist$ward)) %>%
      relocate(intervention, .after = last_col()) %>%
      relocate(num.intervention, .after = last_col()) %>%
      relocate(services, .before = last_col())
  }




#' Replaces the logical TRUE in the column with its label. This
#' features prominently in those columns that have to do with
#' the type of services/interventions carried out by facilities
#'
#' @param column A column of the data frame that inherits from class
#' \code{labelled}.
#'
#' @return The transformed column, now of type \code{character}.
#'
#' @importFrom labelled var_label
#' @export
set_logical_with_label <- function(column) {
  if (!is.logical(column)) {
    booleans <- sum(c("TRUE", "FALSE") %in% column)
    if (booleans > 0 && booleans <= 2)
      column <- as.logical(column)
  }
  label <- labelled::var_label(column)
  ifelse(column, label, NA_character_)
}






.changeToUpper <- function(rgx, x) {
  stopifnot(is.character(rgx), is.character(x))
  stopifnot(length(rgx) == 1)
  sub(rgx, toupper(rgx), x, ignore.case = TRUE)
}








#' Generate File Names for Excel Outputs
#'
#' @param state The State of interest.
#' @param type The type of output one of \code{refdir} (for Referral Directory)
#' or \code{capneed} (for Capacity Needs Assessment).
#'
#' @export
generate_filename <- function(state, type) {
  stopifnot(state %in% getOption("jgbv.project.states"))
  type <- match.arg(type, c("refdir", "capneed"))
  sprintf("%s_tables_%s.xlsx", type, state)
}





#' Check Input of States
#'
#' @param state The project State.
#'
#' @export
assertStateInput <- function(state) {
  rgx <- paste(getOption("jgbv.project.states"), collapse = "|")
  if (!length(state) == 1L || isFALSE(all(grepl(rgx, state))))
    stop("Input must be one of the project States")
}







#' Gets or sets worksheet names
#'
#' A convenience function for consistent naming of spreadsheets for the
#' relevant outputs.
#'
#' @param type Either \code{capneeds} or \code{refdir}. Partial matching is
#' allowed.
#'
#' @return A character vector of length 1L, naming the sheet.
#'
#' @export
SheetName <- function(type = c("capneeds", "refdir")) {
  type <- match.arg(type)
  switch(type, capneeds = "CapNeedsAssmt", refdir = "ReferralDirectory")
}






#' Write an Excel Sheet for Project Outputs
#'
#' This function actually finalises the process of getting an Excel
#' worksheet ready for the Referral Direcories and Capacity Needs
#' Assessment tables.
#'
#' @param data The data frame made with preparatory functions
#' @param path The path of the Excel file to be created.
#' @param sheet The name of the worksheet.
#' @param header.fill The colour for the header.
#' @param header.font.colour The colour of the header text.
#' @param na.string How to represent missing values
#'
#' @return No return value. Used for its side effects.
#'
#' @export
writeFormattedExcelSheet <- function(data, path, sheet, header.fill,
                                     header.font.colour, na.string = "") {
  stopifnot(length(path) == 1L)
  data <- as.data.frame(data)
  wkbk <- if (file.exists(path))
    xlsx::loadWorkbook(path)
  else
    xlsx::createWorkbook()
  mysheets <- xlsx::getSheets(wkbk)
  if (sheet %in% names(mysheets))
    xlsx::removeSheet(wkbk, sheet)
  sheetObj <- xlsx::createSheet(wkbk, sheet)
  hdrStyle <- xlsx::CellStyle(wkbk) +
    xlsx::Fill(foregroundColor = header.fill) +
    xlsx::Font(wkbk, color = header.font.colour, isBold = TRUE) +
    xlsx::Border()
  xlsx::addDataFrame(
    data,
    sheetObj,
    colnamesStyle = hdrStyle,
    characterNA = na.string,
    row.names = FALSE
  )

  cat(sprintf("Saving workbook for %s State ... ", basename(dirname(path))))
  xlsx::saveWorkbook(wkbk, path)
  cat("Done\n")
}



