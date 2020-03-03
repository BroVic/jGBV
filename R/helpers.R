# Source file: helpers.R
#
# MIT License
#
# Copyright (c) 2019 Victor Ordu

globalVariables(c(".",
                  "name",
                  "value",
                  "new"))



#' Creates value-label pairing for a given vector
#'
#' @param variable The data frame variable
#' @param val.lab.obj The object with the label:value pairs
#'
#' @importFrom labelled var_label
#' @importFrom labelled val_labels
#'
#' @export
create_value_label_pairs <- function(variable, val.lab.obj)
{
  stopifnot(is.atomic(variable))
  stopifnot(is.character(val.lab.obj))

  if (!inherits(variable, "labelled"))
    warning("Expected an object of class 'labelled'")

  if (inherits(variable, "integer"))
    class(variable) <- "numeric"  # preserve 'label' attribute vs. as.numeric()

  vals <- create_named_vector(val.lab.obj)

  # Take care of instances where the number of values supplied
  # by the imported script exceeds those in the actual dataset
  cats <- unique(variable)
  if (!identical(length(cats), length(vals))) {
    isPresent <- vals %in% cats
    vals <- vals[isPresent]
  }

  val_labels(variable) <- vals
  variable
}





#' Creates sequences of odd or even value
#'
#' @param vec The vector to be checked
#' @param type Whether to follow an odd or even sequence
#'
#' @export
odd_even_seq <- function(vec, type = c("odd", "even")) {
  stopifnot(is.vector(vec))

  type <- match.arg(type)
  from <- switch(type,
                 odd = 1L,
                 even = 2L)
  seq(from, length(vec), by = 2)
}





#' creates a named vector from single string
#'
#' @param vec.list A vector that contains a list of items for creating
#' a named vector
#'
#' @import stringr
#'
#' @export
create_named_vector <- function(vec.list) {
  stopifnot(is.character(vec.list))

  vec <- vec.list[[1]]
  if (length(vec) > 1L)
    stop("Expected a vector of length 1L as input")

  if (str_detect(vec, "^\\d{1}\\s+")) {
    ## We use this construct in this branch because when we try to split up the
    ## object, some of the would-be labels have multiple words and thus cannot
    ## be implemented by purely splitting on the basis of whitespace.
    vals <- vec %>%
      str_extract_all("\\d+") %>%
      unlist %>%
      as.numeric

    lbls <- vec %>%
      str_replace_all("\\d+\\s", ",") %>%
      str_replace(",", "") %>%
      str_split(" ,") %>%
      unlist %>%
      str_squish
  }
  else if (str_detect(vec, "^\\w")) {
    vectorVersion <- vec %>%
      str_split(" ") %>%
      unlist
    if (length(vectorVersion) < 2)
      stop("Object of length < 2 cannot produce a label-value pair")
    odd.num <- odd_even_seq(vectorVersion, 'odd')
    even.num <- odd_even_seq(vectorVersion, 'even')

    vals <- vectorVersion[odd.num]
    lbls <- vectorVersion[even.num]
  }

  if (!identical(length(vals), length(lbls)))
    stop("Length of values vs labels are unequal")

  names(vals) <- lbls
  vals
}









#' Imports CSV downloaded from REDCap into R session
#'
#' @param path.list A character vector of filepaths
#' @param pattern A regular expression for searching for a given CSV file
#'
#' @importFrom utils read.csv
#'
#' @return A dataframe; \code{stringsAsFactors} is set to \code{FALSE}
#'
#' @export
import_redcap_csv <- function(path.list, pattern) {
  stopifnot(exprs = {
    is.character(path.list)
    is.character(pattern)
  })
  path <- select_file(path.list, pattern, "csv")
  read.csv(path, stringsAsFactors = FALSE)
}










#' Make a kable table
#'
#' Create a table formatted for markdown
#'
#' @param row A vector for the rows.
#' @param column A character vector of the column names
#' @param opt The options for the dummy table
#'
#' @importFrom kableExtra add_header_above
#' @importFrom kableExtra kable_styling
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
    kable_styling("striped") %>%
    add_header_above(c("", grps))
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

#
#
#
#
#
#
#
#' Display current RQDA codes for the project
#'
#' @param dir Path to the directory to which to save the persistent data.
#' Defaults to the folder containing the RQDA project file.
#'
#' @export
view_rqdacodes <- function(dir) {
  codes <- readRDS(file.path(dir, "CODES.rds"))
  cat(codes, sep = "\n")
}
#
#
#
#
#
#
#
#' Selectively transform a variable into a factor
#'
#' @import labelled
#'
#' @param x An atomic vector, usually part of a data frame
#'
#' @importFrom labelled val_labels
#' @importFrom labelled var_label
#'
#'
#' @export
transform_to_factor <- function(x) {
  var.name <- var_label(x)
  val.lab.pair <- val_labels(x)

  x <- if (is.integer(x)) {
    if (!grepl("How many cases of GBV.+12 months", var.name, ignore.case = TRUE))
      factor(x, levels = val.lab.pair, labels = names(val.lab.pair))
    else
      x
  }
  else
    x
  var_label(x) <- var.name
  x
}







#' Check which columns are multiple choice
#'
#' @param x A column, represented as a vector
#'
#' @import stringr
#' @importFrom labelled val_labels
#' @importFrom stats na.omit
#'
#' @export
not_multichoice <- function(x) {
  x <- names(val_labels(x))
  str_detect(x, "^Checked|Unchecked$", negate = TRUE) %>%
    na.omit %>%
    all
}




#' Transform data for "Checked/Unchecked"
#'
#' @param data The data frame
#' @param vec The given vector
#'
#' @importFrom dplyr mutate_at
#'
#' @export
transform_checked <- function(data, vec) {
  stopifnot(ncol(data) == length(vec))

  for(i in seq_along(vec)) {
    .var <- colnames(data)[i]
    data <- mutate_at(data, .var, ~if_else(.x == 1, vec[i], NA_character_))
  }
  data
}








## TODO: Write unit tests
#' Pick out a Colum and Transform it
#'
#' @param data A data frame
#' @param chklist An object of class \code{colCheck}
#'
#' @import dplyr
#' @importFrom rlang :=
#' @importFrom rlang !!
#' @importFrom rlang quo_name
#' @importFrom tidyr unite
#'
#' @export
pickout_cols_and_transform <- function(data, chklist) {
  columnnames <- colnames(data)
  varsIndex <- grep(chklist$regex, columnnames)
  if (length(varsIndex) != length(chklist$opts)) {
    stop("The indices of the data frame are ",
         sQuote(paste(varsIndex, collapse = " ")),
         " while the options for matching are ",
         sQuote(chklist$opts))
  }

  ## Fetch the name and label of variable #1
  firstIndex <- varsIndex[1]
  the.label <- var_label(data[[firstIndex]])
  new.column <- columnnames[firstIndex]
  var.name <- quo_name(new.column)

  edit <- data %>%
    select(varsIndex) %>%
    transform_checked(chklist$opts) %>%
    unite(new, na.rm = TRUE) %>%
    select(new) %>%
    {
      var_label(.$new) <- the.label
      .
    } %>%
    rename(!!var.name := new)

  # if (is.null(var_label(edit[[new.column]])))
  #   warning("Variable labels have been stripped")
  #
  data %>%
    select(-varsIndex) %>%
    bind_cols(edit)
}




#' Create Object Of Class colCheck
#'
#' Constructor for colCheck objects - a class that conveys information on
#' different multiple choice variables
#'
#' @param regex A regular expression used for identifying the columns
#' @param opts TA list of character vectors, each of which are options for the
#' for the given question.
#'
#' @export
# TODO: Write unit tests
colCheck <- function(regex, opts) {
  structure(
    list(regex = regex,
         opts = opts),
    class = "colCheck"
  )
}








#' Collapse Columns for Multiiple-Choice Questions
#'
#' @param data An object of class \code{data.frame}
#' @param details An object of class \code{colCheck}
#'
#' @return An updated data frame
#' @note All the newly created columns will be pushed to the right end of the
#' table.
#'
#' @export
collapse_multichoice <- function(data, details) {
  data %>%
    as_tibble %>%
    {
      for (i in seq_along(details)) {
        . <- pickout_cols_and_transform(., details[[i]])
      }
      invisible(.)
    }
}








#' Transform the data for a given sector
#'
#' @param data The data frame with the data for a given sector
#' @param regexes Regular expression defining patterns for manipulating
#' columns for multiple choice questions
#' @param options The choices
#'
#' @export
transform_sector_data <- function(data, regexes, options) {
  stopifnot(length(regexes) == length(options))

  map2(regexes, options, colCheck) %>%    # class constructor
    {
      collapse_multichoice(data, .)
    }
}





#' Unnest a List Column
#'
#' 'Open up list columns containing multiple choice responses by creating
#' a column for each option
#'
#' @param data The data frame
#' @param var The variable to be expanded
#' @param chr A character vector, which is the label of the value and which
#' will also be used to develop a new name for the column
#'
#' @importFrom dplyr %>%
#' @importFrom dplyr mutate
#' @importFrom rlang enquo
#' @importFrom rlang quo_name
#' @importFrom rlang !!
#' @importFrom rlang :=
#' @importFrom purrr map_lgl
#' @importFrom stringr str_detect
#'
#' @return A modified data frame
#'
#' @export
extend_single_listcol <- function(data, var, chr) {
 stopifnot(any(grepl(deparse(substitute(var)), colnames(data))))  # TODO: Test!

  var <- enquo(var)
  colname <- chr[1] %>% make.names %>% quo_name

  data %>%
    mutate(
      !!colname := map(!!var, str_detect, chr) %>%
        map_lgl(any)
    )
}






#' Basic bar plot
#'
#' @param data The data frame
#' @param title The title of the plot
#' @param xlab The x-axis label. Defaults to \code{x}
#'
#' @import ggplot2
#'
#' @return A ggplot object
#'
#' @export
basic_bar <- function(data, title = "[Title]", xlab = 'x') {
  ggplot(data) +
    aes(name, fill = name) +
    geom_bar() +
    theme(legend.position = 'none',
          plot.title = element_text(hjust = .5)) +
    ggtitle(title) +
    ylab("No. of respondents") +
    xlab(xlab)
}


#' Get a variable with it's question
#'
#' @param data The data frame
#' @param ques The question found within the variable label
#'
#' @importFrom labelled var_label
#' @importFrom magrittr %>%
#'
#' @export
# TODO: Unit tests and roxygen2 commenting
find_var_with_ques <- function(data, ques) {
  var_label(data) %>%
    str_which(ques)
}






#' Get a Question from the Variable name
#'
#' @param data The data frame
#' @param varname The variable name
#'
#' @importFrom labelled var_label
#'
#' @export
find_ques_with_var <- function(data, varname) {
  stopifnot(is.character(varname))
  names <- colnames(data)
  ind <- match(varname, names)
  if (length(ind) > 1L)
    stop("Expected to have unique column name")
  var_label(data)[ind]
}





#' Get Column from a question
#'
#' @param data The data frame
#' @param ques The question found in the variable label
#'
#' @importFrom magrittr %>%
#'
#' @export
column_from_question <- function(data, ques) {
  ind <- data %>%
    find_var_with_ques(ques)
  colnames(data)[ind]
}





#' Get Data on Services Offered
#'
#' @param data A data frame
#' @param ques The question (found in the variable label itself)
#' @param options The service options for that given question and tool
#'
#' @export
get_service_data <- function(data, ques, options) {
  col <- column_from_question(data, ques)
  prepare_extended_col(data, col, options, multisectoral = FALSE)
}










#' Get the coding table from RQDA projects from collaborators
#'
#' @param path A path to a directory, zip file or RQDA projecct file
#'
#' @description This function is designed to just fetch RQDA projects
#' with as little hassle as possible. So whether it's a directory, or
#' a compressed archive or an RQDA project file itself, the codes will
#' be extracted.
#'
#' @description In the event that there are several RQDA files in the
#' accessed directory, they will all be extracted and merged into one
#' single table. No further transformation is carried out, thereafter.
#'
#' @import DBI
#' @import RSQLite
#' @importFrom RQDA closeProject
#' @importFrom RQDA getCodingTable
#' @importFrom RQDA openProject
#' @importFrom purrr map_df
#' @importFrom utils file_test
#'
#'
#' @return The coding table as an R \code{data.frame}.
#'
#' @export
obtain_rqda_codes <- function(path) {
  stopifnot(is.character(path))
  isZipfile <- endsWith(path, ".zip")
  finalDir <-
    if (file_test("-d", path))
      path
  else if (file_test("-f", path)) {
    if (isZipfile)
      extract_zipfile(path)
    sub("\\.[[:alpha:]]+$", "", path)
  }
  else
    stop("'path' is incorrect or non-existent")

  rqdafile <-
    if (endsWith(path, ".rqda"))
      path
  else
    list_files_pattern(finalDir, "\\.rqda$")

  projectsFound <- length(rqdafile)
  if (projectsFound == 0L) {
    if (isZipfile) {
      unlink(finalDir, recursive = TRUE, force = TRUE)
      warning("Created extraction directory ",
              sQuote(finalDir),
              " was removed")
    }
    stop("RQDA project file not found in ", sQuote(finalDir))
  }
  else if (projectsFound > 1L)
    warning("More than one RQDA project file found")

 map_df(rqdafile, function(proj) {
    tryCatch({
      openProject(proj)
      codes <- getCodingTable()
      on.exit(closeProject())
      codes
    }, error = function(e)
      e)
  })
}



#' Check A Question Across Datasets
#'
#' Uses a question, held in the label to identify variables across different
#' data frames.
#'
#' @param data.list A list of data frames
#' @param ques The label containing the actual question
#'
#' @importFrom labelled var_label
#' @importFrom purrr %>%
#' @importFrom purrr map
#' @importFrom purrr map2
#'
#' @return A list of variables from each dataset containing the question.
#' @export
find_var_across_datasets <- function(data.list, ques) {
  stopifnot(is.list(data.list), is.character(ques))

  map(data.list, find_var_with_ques, ques = ques) %>%
    map2(data.list, function(ind, df)
    var_label(df)[ind])
}





# view_labels <- function(data, columnames) {
#   var_label(data)[colnames(data)]
# }


#' Convert a number to percentage
#'
#' @param num The number to be converted
#'
#' @export
compute_percent <- function(num) {
  stopifnot(is.numeric(num))
  round(num, 4) * 100
}






#' Test For RAAMP-GBV Project State
#'
#' Checks whether a string represents a RAAMP-GBV project state
#'
#' @importFrom naijR states
#'
#' @param str A character vector of length 1.
#'
#' @return A boolean value \code{TRUE} or \code{FALSE}
#'
#' @export
is_project_state <- function(str)
{
  stopifnot(is.character(str))
  allStates <- naijR::states()
  if (!str %in% allStates) {
    if (str %in% tolower(allStates)) {
      warning(sprintf(
        "Possible typo. Did you mean to type %s?",
        sQuote(tools::toTitleCase(str))
      ))
      return(FALSE)
    }
    else
      stop(sQuote(str), " is not a Nigerian State")
  }
  if (!str %in% raampStates)
    return(FALSE)

  TRUE
}




# Bring up an R Markdown for generating a document
# `dir` is the parent directory of the `skeleton` directory
.retrieveDocumentTemplate <- function(dir)
{
  stopifnot(is.character(dir))
  tmpl <- system.file(
    "rmarkdown",
    "templates",
    dir,
    "skeleton",
    "skeleton.Rmd",
    package = "raampGBV"
  )
  if (identical(tmpl, ""))
    stop("Codebook template not found")
  tmpl
}





#' collect Name of RAAMP State
#'
#' Collect the name of a given State for the purpose of using it to build a
#' given part of the project that is state-dependent.
#'
#' @details This function will collect input both from the shell and during
#' interactive R sessions. Also, if a State's name is supplied as an argument
#' to \code{Rscript}, it will read it and apply it as required.
#'
#' @export
input_state <- function() {
  prompt <- "Enter State: "

  state <- if (!interactive()) {
    args <- commandArgs(trailingOnly = TRUE)

    if (identical(args, character(0))) {
      cat(prompt)
      readLines("stdin", n = 1L)
    }
    else
      args
  } else
    readline(prompt = prompt)

  paste(state, collapse = " ")
}
