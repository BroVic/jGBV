# Source file: start.R
#
# MIT License
#
# Copyright (c) 2022 Victor Ordu


## Functions for collecting and cleaning up data

globalVariables(c("orgtype", "orgname"))

#' Import Raw Data Into the Project
#'
#' @param dir A directory used for the downloads, which usually has
#' subdirectories - one for each project State.
#' @param db Path to an SQLite database. If non-existent, will attempt to
#' create one. If \code{NULL}, the data are returned without being saved.
#' @param modlist An optional list of objects of class \code{VarModifier}.
#' @param state The project state.
#' @param filetype The kind of data being imported. Should be one of
#' \emph{Capacity} or \emph{Services} for data on capacity assessment and for
#' GBV service mapping, respectively.
#' @param drop.c,drop.v Character or integer vector of columns or variables to
#' be dropped from the data table or list of variable names, respectively.
#' @param nvars The number of columns in the final data frame.
#' @param ... Arguments passed to \code{read_in_excel_data}.
#' @param na.strings Strings in the Excel sheet to be interpreted as \code{MA}.
#'
#' @return Both functions return a \code{data.frame} with labelled variables.
#' \code{import_data} does so invisibly.
#'
#' @note \code{import_data} wraps \code{read_in_excel_data}, as it carries
#' out additional processing of the data read from the spreadsheets.
#' \code{nvars} is usually set for the capacity assessment data, which tend
#' to have superfluous columns.
#'
#' @importFrom RSQLite dbConnect
#' @importFrom RSQLite dbDisconnect
#' @importFrom dplyr filter
#' @importFrom dplyr %>%
#'
#' @export
import_data <-
  function(dir, db = NULL, modlist = NULL, state, filetype, ..., na.strings)
{
  # Validate input
  if (!is.null(modlist)) {
    if (!is.vector(modlist, mode = "list"))
      stop("'modlist' must be a vector of type 'list'")
  }
  state <- match.arg(state, getOption("jgbv.project.states"))
  srv <- "Services"; cap <- "Capacity"
  filetype <- match.arg(filetype, c(srv, cap))
  if (missing(na.strings))
    na.strings <- ""

  ## Collect data as well as modified variable names
  ## The vector with the modified names is supplied as
  ## a named vector via `options`, and the vector is named
  ## to make for easy indexing. The vector used varies depending
  ## on whether we are working with data on service mapping or
  ## for capacity assessment.
  dat <- read_in_excel_data(dir, state, filetype, ..., na.strings = na.strings)
  newvars <- .chooseNewVars(filetype)

  ## Make sure that only values from the state of interest
  ## are used. This is made necessary by the experience we
  ## had where data from 2 States were inadvertently joined
  ## together in a single Excel sheet. We don't want to have
  ## check this manually again. This applies only to the
  ## capacity assessment data!!!
  if (filetype == cap) {
    lgavarname <- paste0("lga.", tolower(state))
    lgavar <- newvars[lgavarname]
    dat <- dplyr::filter(dat, !is.na(.data[[lgavar]]))
  }

  # Fine-tune the variable labels
  # First do the generic one that works the same regardless of the
  # type of data we're dealing with, and then the one specific
  # to each type of dataset.
  var_label(dat) <- dat %>%
    var_label(unlist = TRUE) %>%
    unname %>%
    .cleanLblGeneral() %>%
    {
      if (filetype == cap)
        .cleanLblCapacity(.)
      else if (filetype == srv)
        .cleanLblServices(.)
    }

  ## This block is only run for the data for service mapping
  ## Essentially, we are cleaning up selected columns.
  ## TODO: This operation may not be wholly necessary at this
  ## stage since we are saving to a database, which does not
  ## preserve all the base R types/classes used here.
  if (filetype == srv) {
    if (!is.null(modlist)) {
      for (x in modlist)
        try(
          dat <-
            .modifyAndPreserveLabels(
              dat, newvars[x$vars], x$func, x$nestfunc, x$args)
        )
    }
    dat <- transform_bool_to_logical(dat)

    # In some of the State datasets, the number of staff/beneficiaries and costs
    # are represented as character vectors, so we perform a check/fix for this.
    for (col in grep("num_|_num|fee_", newvars, value = T)) {
      if (is.character(dat[[col]]))
        try(dat <- .modifyAndPreserveLabels(dat, col, as.numeric))
    }

    ## Conversely, some of the columns that ought to contain strings are
    ## found to be numeric in some States' datasets
    for (col in grep("describe|simserial", newvars, value = TRUE))
      try(dat <- .modifyAndPreserveLabels(dat, col, as.character))

    dat <- dat |>
      .modifyAcrossPatterns("^(yes|no|sometimes|never|always)$", str_to_title) |>
      .modifyAcrossPatterns("^free$", str_to_title) |>
      fix_factors(newvars)

    ## Some of the LGAs in States are misspelt. We have prepared a function
    ## that is customized to fix this given the spelling mistakes identified
    ## and varies depending on the State
    dat[[newvars['lga']]] <-
      repair_lga_spelling(dat[[newvars['lga']]], state)
  }
  else if (filetype == cap) {
    ## In some cases, there are missing values where the type of
    ## organization ought to be specified. In this instance, we try
    ## to deduce what kind of organization it is by examining the name
    ## of the organization, which is extracted with regular expressions.
    ## When found, the type is identified and filled in.
    rgxes <- c(health = "(hf|health|hospital|phc|dispensary)\\s?",
               law = "law|nscdc")
    is.missing <- sapply(rgxes, function(rgx) {
      dat %>%
        with(is.na(orgtype) &
               grepl(rgx, orgname, ignore.case = TRUE)) %>%
        which()
    }, simplify = FALSE)

    fac.string <-
      sapply(
        names(rgxes),
        function(nm) {
          unique(grep(nm, x = dat$orgtype, value = TRUE, ignore.case = TRUE))
        },
        USE.NAMES = TRUE
      )

    for (i in names(is.missing)) {
      indic <- is.missing[[i]]
      dat$orgtype[indic] <- fac.string[[i]]
    }
  }

  if (!is.null(db))
    save_table(dat, state, type = tolower(filetype), db)

  invisible(dat)
}




.chooseNewVars <- function(filetype) {
  stopifnot(is.character(filetype))
  ft <- tolower(filetype)

  if (ft == 'services')
    return(new.varnames)

  if(ft == 'capacity')
    return(getOption('jgbv.capnames'))

  stop("No avaiable action for file type ", sQuote(filetype))
}


#
# make_filename_regex <- function(services, capacity) {
#   if (is.character(services) && is.character(capacity))
#     return(c(Services = services, Capacity = capacity))
#   stop("Both 'services' and 'capacity' must be character vectors")
# }



# The next set of modifications are based on the actual elements
# of the variable. They are identified with regular expressions.
# This function uses those patterns to carry out the modifications.
.modifyAcrossPatterns <- function(df, pattern, f) {
  func <- substitute(f)
  stopifnot({
    is.data.frame(df)
    is.character(pattern)
    # is.function(eval(func))
  })
  patternFound <-
    function(column) any(grepl(pattern, column))
  indices <- which(vapply(df, patternFound, logical(1)))
  if (!indices[1])
    stop("The regular expression ",
         sQuote(pattern),
         "did not match column values")
  for (i in indices)
    try(df <- .modifyAndPreserveLabels(df, i, deparse(func)))
  df
}




# Generic cleaning of variable labels
#' @importFrom stringr str_remove
#' @importFrom stringr str_replace
#' @importFrom stringr str_replace_all
#' @importFrom stringr str_trim
#' @importFrom stringr str_squish
.cleanLblGeneral <- function(labels) {
  stopifnot(is.character(labels))

  labels %>%
    str_remove("^_+") %>%
    str_replace_all("\\s/\\s", "/") %>%
    str_replace("\\s/\\s", "/") %>%
    str_remove("\\.{3}\\d+$") %>%
    str_trim %>%
    str_squish
}



#' @importFrom stringr str_remove
#' @importFrom stringr str_remove_all
#' @importFrom stringr str_replace
#' @importFrom stringr str_trim
#' @importFrom stringr str_squish
.cleanLblCapacity <- function(labels) {
  stopifnot(is.character(labels))

  labels %>%
    str_replace("^(.+)(GPS coordinates)(.+_)(.+)$", "\\2 (\\4)") %>%
    str_remove(regex("(Please)? (specify|state)", ignore_case = TRUE)) %>%
    str_remove("^\\d{1,2}\\)") %>%
    str_remove_all("Have you participated in any training on the provision of")  %>%
    str_remove("Handling of GBV case\\(s\\)") %>%
    str_trim %>%
    str_remove("\\,$") %>%
    str_replace("^e", "E") %>%
    str_squish
}


#' @importFrom stringr str_remove
#' @importFrom stringr str_replace
#' @importFrom stringr str_trim
#' @importFrom stringr str_squish
.cleanLblServices <- function(labels) {
  stopifnot(is.character(labels))

  labels %>%
    str_replace("lgaorigin", "LGA") %>%
    str_replace("stateorigin", "State") %>%
    str_replace("\\(select all that apply\\)", "") %>%
    str_remove("^Sources of funding/") %>%
    str_replace("\\((please|to)?\\s?describe\\)", " ") %>%
    str_replace("(Other )(\\((to )?specify\\))", "\\1") %>%
    str_replace("The qualification/capacity of the staff", "Staff qualification/capacity") %>%
    str_replace("(Nothing, facility is w)(ell equipped)", "W\\2") %>%
    str_remove("\\((please )?specify\\)") %>%
    str_remove("^Opening days/") %>%
    str_remove("^Which forms of GBV does this facility address\\?/") %>%
    str_remove("^Is the facility.+survivors of GBV?/") %>%
    str_remove("^Please.+provided by this organization:/") %>%
    str_remove("^Please select the specific forms of GBV.+ protocols.+survivors, such as :/") %>%
    str_remove("^What services.+provide to survivors.+at this facility\\?/") %>%
    str_remove(
      "^Is the facility missing anything that is necessary to provide quality care to survivors of GBV\\?/"
    ) %>%
    str_remove(
      regex(
        "^Are the following MEDICINES AND ESSENTIAL SUPPLIES\\s*available\\?/",
        igore_case = TRUE
      )
    ) %>%
    str_remove("^Are the following elements available\\?/") %>%
    str_remove("\\(funds for school fees\\, food\\, etc\\.\\)") %>%
    str_remove("\\(Please specify the length of time survivors are allowed to stay\\)") %>%
    str_remove("\\(funds for school fees\\, food\\, etc\\.\\)") %>%
    str_remove("\\(referral to other structures\\)") %>%
    str_remove("^Refer to") %>%
    str_remove("\\(including completion of medical certificates or other medico-legal forms\\)") %>%
    str_remove("\\(i\\.e\\. first aid kits\\, feminine hygiene products\\)") %>%
    str_remove("\\(survivor-centered approaches and trauma-informed interview skills\\)") %>%
    str_remove("\\(i\\.e\\. private or hidden from other rooms/areas\\)") %>%
    str_remove("^Which type of standard health forms\\?/") %>%
    str_remove("^How many cases were reported in the last 6 months for\\s") %>%
    str_replace("^rape", "Rape") %>%
    str_remove("\\(rape, sexual assault.*\\)$") %>%
    str_remove("/domestic violence$") %>%
    str_remove("\\(including physical.* or sexual abuse of children\\)") %>%
    str_remove("^Does this facility.+trained and experienced in\\:?/") %>%
    str_remove("^What resources are available for investigation and follow-up\\?/") %>%
    str_remove(
      "^What precautions are taken to ensure the safety of survivors of GBV and to protect their privacy\\?/"
    ) %>%
    str_remove("^Does this organization offer the following amenities\\?/") %>%
    str_remove("^What services are provided\\?/") %>%
    str_remove("for temporary storage of forensic evidence$") %>%
    str_remove(regex("^what is the price of the", ignore_case = TRUE)) %>%
    str_remove("service in NGN\\?$") %>%
    {
      cap <- "MEDICINES AND ESSENTIAL SUPPLIES"
      str_replace(., cap, str_to_lower(cap))
    } %>%
    str_trim %>%
    str_squish
}



#' @rdname import_data
#'
#' @export
read_in_excel_data <-
  function(dir,
           state,
           filetype,
           drop.c = NULL,
           drop.v = NULL,
           nvars = NULL,
           na.strings = "") {
    if (!dir.exists(dir))
      stop("The directory 'path' does not exist")
    state <- match.arg(state, getOption("jgbv.project.states"))
    filetype <- match.arg(filetype, c("Services", "Capacity"))
    force(drop.c)
    force(drop.v)
    force(nvars)

    # Select a pattern (if applicable for extracting the Excel file)
    xl.rgx <- NULL
    rgx <- getOption("jgbv.excelfile.regex")
    if (!is.null(rgx))
      xl.rgx <- rgx[filetype]

    dir <- paste(dir, state, sep = '/')
    xlf <- list.files(dir, xl.rgx, ignore.case = TRUE)
    if (length(xlf) > 1L)
      stop("More than one Excel file selected")
    xlpath <- file.path(dir, xlf)
    newcolnames <- .chooseNewVars(filetype)
    .readRawAndLabel(xlpath, newcolnames, filetype, state, drop.c, drop.v, nvars, na = na.strings)
  }




# Reads in the raw data from Excel and also labels the new data frame
#' @importFrom dplyr select
#' @importFrom labelled var_label
#' @importFrom readxl read_xlsx
#' @importFrom purrr map_df
.readRawAndLabel <-
  function(file,
           new.var,
           ftype,
           state,
           drop.col,
           drop.var,
           numvar,
           na) {
    stopifnot({
      file.exists(file)
      is.character(ftype)
    })

    df <- readxl::read_xlsx(file, na = na)
    if (!is.null(drop.col))
      for (col in drop.col)
        df[[col]] <- NULL

    if (!is.null(numvar))
      df <- df[seq_len(numvar)]

    labelled::var_label(df) <- names(df)

    # For situations where there are missing variables from the
    # imported data, we will add new variables that have only
    # missing values.
    if (!is.null(new.var)) {
      missingVars <- isFALSE(is.null(drop.var))
      if (missingVars) {
        add.var <- new.var[drop.var]
        new.var <- new.var[-drop.var]
      }
      names(df) <- new.var
      if (!matchDfWithVarsLength(df, new.var))
        stop("'new.var' must have as many elements as there are columns")
      if (missingVars)
        df[, add.var] <- NA
    }
    df
  }








#' Cleanup Categorical Variables
#'
#' Converts relevant character vectors in the dataset into factors.
#'
#' @param data The data frame
#' @param newvars A named vector with the variable names.
#'
#' @return The modified data frame (if applicable).
#'
#' @export
fix_factors <- function(data, newvars) {

  if (!is.data.frame(data))
    stop("'data' should be of class 'data.frame'")

  if (missing(newvars))
    newvars <- new.varnames

  if (!is.character(newvars))
    stop("'newvars' should be atomic and of type 'character'")

  data <-
    .factorizeAndPreserveLabels(data, newvars[.yesNoVarnames()], c("Yes", "No"))
  data <-
    .factorizeAndPreserveLabels(data,
                                newvars[.yesNoVarnames("ynd")],
                                c("Yes", "No", "Don't know"))
  data <- .factorizeAndPreserveLabels(data,
                                      newvars[.yesNoVarnames("ynsd")],
                                      c("Yes", "No", "Sometimes", "Don't know"))
  .factorizeAndPreserveLabels(
    data,
    newvars["hf.type"],
    c(
      "Primary health care facility",
      "Secondary health care facility",
      "Tertiary health care facility",
      "Other"
    )
  )
}







# Note that the value returned by this function is a vector of, not the
# the variable names themselves but the arbitrary names created as attributes
# or metadata to allow for subsequent variable names changes.
.yesNoVarnames <- function(type = c("yn", "ynd", "ynsd")) {
  type <- match.arg(type)
  switch(
    type,
    yn = c(
      "serve.disabled",
      "disabled.special",
      "coc.copies",
      "coc.confidentiality",
      "coc.equity",
      "has.focalperson",
      "has.gbv.trained",
      "has.refdir",
      "choose.referral",
      "coordination",
      "support.for.court",
      "police.followup",
      "shelter.famfriendly",
      "shelter.kidfriendly",
      "shelter.support",
      "shelter.new.support",
      "child.docs"
    ),
    ynd = c("standard.forms",
            "data.is.stored",
            "coc.signed"),
    ynsd = c("computer.secured")
  )
}




.factorizeAndPreserveLabels <- function(df, colnames, new.levels) {
  stopifnot({
    is.data.frame(df)
    is.character(colnames)
    is.character(new.levels)
  })
  for (fac in colnames) {
    l <- var_label(df[[fac]])
    df[[fac]] <- factor(df[[fac]], levels = new.levels)
    var_label(df[[fac]]) <- l
  }
  df
}















#' Collect User Input for Data Importation
#'
#' Collects input from the user related to data for the
#' projects.
#'
#' @details Two variables are collected from the user: the state of interest
#' and the kind of data i.e. services or capacity assessment. The input can
#' be collected interactively or passed as a command line argument.
#'
#' @note When passing input via the shell or a script, \code{state} should
#' precede \code{filetype} in the argument list.
#'
#' @return A named list with 2 elements - one for each value collected.
#'
#' @importFrom tools toTitleCase
#'
#' @export
collect_input <- function() {
  inputerr <- "Illegal input"
  if (interactive()) {
    state <- pick_one_state()
    ftype <- c("Services", "Capacity")
    ftypeIndex <- menu(ftype, TRUE, "Select one")

    if (!ftypeIndex)
      stop(inputerr)

    filetype <- ftype[ftypeIndex]
  }
  else {
    arglist <- commandArgs(trailingOnly = TRUE)
    allstates <- getOption('jgbv.project.states')
    res <- match(arglist, allstates)
    stateIndex <- which(!is.na(res))
    state <- arglist[stateIndex]

    if (!(state %in% allstates))
      stop(inputerr)

    filetype <- arglist[1:2 != stateIndex] %>%
      tools::toTitleCase()
  }
  list(state = state, filetype = filetype)
}


#' Repair Poorly Spelt Nigeria Local Government Areas in the Project
#'
#' A wrapper for \code{naijR::fix_region} for dealing with misspelt
#' Local Government Areas.
#'
#' @param x The character vector to be checked.
#' @param state The state whose LGAs will be checked.
#'
#' @importFrom naijR fix_region
#' @importFrom naijR fix_region_manual
#' @importFrom naijR is_lga
#' @importFrom naijR lgas
#'
#' @export
repair_lga_spelling <- function(x, state) {
  stopifnot(state %in% getOption('jgbv.project.states'))

  if (all(is_lga(x)))
    return(x)

  if (is.character(x))
    x <- lgas(x, warn = FALSE)

  x <- fix_region(x, quietly = TRUE)

  if (state == "Taraba")
    fix_region_manual(x, "Kurmi", "Kumi")
  else if (state == "Niger") {
    moya <- "Moya"
    x %>%
      fix_region_manual("Muya", moya) %>%
      fix_region_manual("Muye", moya)
  }
  else x
}



#' Change Binary Values to Logical
#'
#' Set the Columns that have 1/0 values to T/F instead, notably those
#' for multiple response variables
#'
#' @param data The data frame
#'
#' @return The data frame, now modified (if applicable).
#'
#' @export
transform_bool_to_logical <- function(data) {
  if (!is.data.frame(data))
    stop("'data' must be an object of class 'data.frame'")
  for (k in getOption("jgbv.multiresponse.regex")) {
    colnams <- grep(k, names(data), value = TRUE)
    for (col in colnams) {
      if (is.character(data[[col]]))
        data <- .modifyAndPreserveLabels(data, col, as.integer)
    }
    data <- .modifyAndPreserveLabels(data, colnams, as.logical)
  }
  data
}


# We need this function when modifying the variables because some
# of the functions in use do strip the labels
#' @importFrom labelled var_label
.modifyAndPreserveLabels <- function(df, x, f, nest = NULL, ...) {
  stopifnot(is.data.frame(df))
  local({
    f <- if (is.character(f))
      as.name(f)
    else
      substitute(f)
    arglist <- list()
    columnnames <-  names(df)
    for (i in x) {
      if (is.character(i) && (!i %in% columnnames)) {
        message("No variable named ", i)
        next
      }
      l <- var_label(df[[i]])
      cc <- as.call(list(f, quote(df[[i]])))
      cl <- c(as.list(cc), ...)
      cc <- as.call(cl)
      if (!is.null(nest)) {
        nestfun <- if (is.character(nest))
          as.name(nest)
        else
          substitute(nest)
        cc <- as.call(list(nestfun, cc))
      }
      if (is.numeric(i))
        i <- columnnames[i]
      message("Modifying ", i)
      df[[i]] <<- eval(cc)
      var_label(df[[i]]) <<- l
    }
  })
  return(df)
}


