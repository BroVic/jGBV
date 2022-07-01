# Source file: helpers.R
#
# MIT License
#
# Copyright (c) 2019 Victor Ordu

globalVariables(c(".", "name", "value", "new"))

# The last name change caused a lot a problems with the code
thisPkg <- function()
{
  "jGBV"
}





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







#' Get a variable with it's question
#'
#' @param data The data frame
#' @param ques The question found within the variable label
#'
#' @importFrom labelled var_label
#' @importFrom stringr str_which
#'
#' @export
# TODO: Unit tests and roxygen2 commenting
find_var_with_ques <- function(data, ques) {
  x <- var_label(data)
  str_which(x, ques)
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
#' @export
column_from_question <- function(data, ques) {
  ind <- find_var_with_ques(data, ques)
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




