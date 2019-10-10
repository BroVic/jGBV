globalVariables(c(".",
                  "lgas_nigeria",
                  "name",
                  "value",
                  "new"))


#' Make a Representation of the Project's Standard Directory Tree
#'
#' @importFrom here here
#' @importFrom purrr map
#' @importFrom stats setNames
#'
#' @export
make_dir_tree <- function()
{
  dList <-
    map(c("data", "src", "downloads", "doc", "tools"), here)
  dList <-
    setNames(dList, c("dat", "src", "dwn", "doc", "tls"))

  ql <- file.path(dList$dat, "qual")
  qt <- file.path(dList$dat, "quant")
  cod <- "coding"

  list(
    qual = ql,
    quant = qt,
    data = dList$dat,
    tools = dList$tls,
    downloads = dList$dwn,
    coding = file.path(dList$src, cod),
    clean = file.path(dList$src, "clean"),
    output = file.path(dList$doc, "output"),
    utility = file.path(dList$src, "utility"),
    transcripts = file.path(ql, "transcripts"),
    shinyApp = file.path(dList$src, "shiny", "app"),
    codebooks = file.path(dList$src, cod, "codebooks")
  )
}











#' Retrieve all files of a given type from a directory
#'
#' Lists files in a directory whose names have a certain file extension.
#'
#' @param dir character vector of directory path
#' @param file.ext The file extension; defaults to \emph{R}
#'
#' @return A character vector whose elements are the full paths of the
#' found files. If none is found, \code{character(0)}.
#'
#' @note For the RAAMP-GBV RStudio project, the root directory is set to
#' the project directory.
#'
#'
#' @export
fetch_all_filepaths_named <- function(dir, file.ext = "R") {
  stopifnot(is.character(dir))

  pattern <- sprintf(".+\\.%s$", file.ext)

  if (!dir.exists(dir))
    stop('The directory does not exist')

  the.list <- list_files_pattern(dir, pattern)

  if (identical(the.list, character(0)))
    warning("No filepaths returned")

  the.list
}




#' Fetch All The Data
#'
#' @importFrom glue glue
#' @importFrom purrr map
#' @importFrom stats setNames
#'
#' @param state The RAAMP Assessment State e.g. Ogun
#' @param dir The directory where the data are saved.
#' @param sectors The sectors for which we are retrieving data
#'
#' @export
fetch_all_data <-
  function(state, dir, sectors) {
    dPath <- file.path(dir, state)

    map(sectors, function(sector) {
      regex <- glue("transformed.+{state}.+{sector}")
      rds <- list.files(dPath, pattern = regex, full.names = TRUE)
      if (is.null(rds))
        stop("Data file not found")
      readRDS(rds)
    }) %>%
      setNames(sectors)
  }




#' Extract Code from an R script
#'
#' @param rgx A reguar expression
#' @param txt A portion of text, supplied as a character vector
#'
#' @import stringr
#'
#' @return A character vector with the extracted code
extract_code <- function(rgx, txt) {
  stopifnot(is.character(rgx))
  stopifnot(is.character(txt))

  txt %>%
    str_subset(rgx) %>%
    str_replace(rgx, "\\2")
}



#' Extract Vector Code
#'
#' @param str A string containing the code
#'
#' @return A vector from the evaluated code
#' @export
extract_vector <- function(str)
  eval(parse(text = str))










#' Save an object with specific details
#'
#' This is a custom wrapper for \code{saveRDS}.
#'
#' @param dir The directory to which to save the RDS file
#' @param obj The object to be save.
#' @param filelabel A label to be applied to the filename.
#' @param state The State for whose data we are saving (specific to
#' RAAMP-GBV project)
#' @param ... Additional strings to add to filename
#'
#'
#' @export
save_as_rds <- function(dir, obj, filelabel, ..., state) {
  saveRDS(obj,
          file.path(
            dir,
            state,
            paste0(filelabel, "_", state, "_", ..., ".rds")
          ))
}










## Creates value-label pairing for a given vector
#' @importFrom labelled var_label
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





# Creates sequences of odd or even values
odd_even_seq <- function(vec, type = c("odd", "even")) {
  stopifnot(is.vector(vec))

  type <- match.arg(type)
  from <- switch(type,
                 odd = 1L,
                 even = 2L)
  seq(from, length(vec), by = 2)
}





## creates a named vector from single string
#' @import stringr
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





## Selects a file from a vector of file names based on a regex pattern
select_file <- function(path.list, pattern, type = c("R", 'csv')) {
  if (!all(file.exists(path.list)))
    stop("One or more paths do not exist")
  type <- match.arg(type)
  pattern <- sprintf("%s.*\\.%s$", pattern, type)
  path <- grep(pattern,
               path.list,
               ignore.case = TRUE,
               value = TRUE)
  stopifnot(length(path) == 1L)
  if (!file.exists(path))
    stop("Path ", sQuote(path), " does not exist")
  path
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






#' Conveniently list files in a directory
#'
#' @param dir The path of the directory
#' @param pattern A regular expression for selecting files
#'
#' @return A character vector with all the files whose paths match the
#' \code{pattern}. Otherwise returns \code{character(0)}.
#'
#' @export
list_files_pattern <- function(dir = ".", pattern) {
  stopifnot(is.character(pattern))
  list.files(
    path = dir,
    pattern = pattern,
    full.names = TRUE,
    ignore.case = TRUE
  )
}





#' Generate codebooks
#'
#' Automated generation of ALL the quantitative codebooks
#'
#' @param states A character vectors with names of the Nigerian states of
#' interest as defined by the \code{states} object.
#' @param  sectors A character vector with the labels of the various tools
#' as defined by the \code{tools.sector} object.
#'
#' @importFrom purrr map
#' @importFrom purrr map_chr
#' @importFrom stats setNames
#'
#' @return A named list, with each element of the list being a character vector
#' of the filepaths of individual codebooks generated for a given State.
#'
#' @export
generate_all_codebooks <- function(states, sectors) {
  cat("Generating all codebooks.\nThis will take time. Please wait...\n")

  map(states, function(state) {
    cat(sprintf("Building codebooks for %s State:\n", state))

    map_chr(sectors, function(sector) {
      cat(sprintf("%s sector... ", sector))
      fpath <- build_single_codebook(state, sector)
      cat("Done\n")
      fpath
    })
  }) %>%
    setNames(states)
}








#' Build codebook
#'
#' Render an individual codebook for a given State and Tool
#'
#' @param state The State where the Assessment was conducted
#' @param tool The specific category of tool that was administered
#' @param rmdfile The template Rmarkdown file for building the codebook
#' @param outputdir The output directory
#'
#' @importFrom rmarkdown render
#'
#' @return The path of the rendered Rmarkdown file.
#'
#' @export
build_single_codebook <- function(state, tool, rmdfile, outputdir) {
  render(
    input = rmdfile,
    output_format = "html_document",
    output_dir = outputdir,
    output_file = sprintf("codebook_%s_%s.html", tool, state),
    params = list(state = state, toolType = tool),
    quiet = TRUE
  )
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





#' Extract ZIP file
#'
#' Extract a zip archive for a given state
#'
#' @param zipfile Character vector of the ZIP file name
#' @param state State whose data is being extracted
#'
#' @importFrom utils unzip
#'
#' @return A data frame from  argument
#' set to \code{TRUE}
#'
#' @export
extract_zipfile <- function(zipfile, state = NULL) {
  stateRelated <- isFALSE(is.null(state))

  if (stateRelated)
    message("Extracting files for ", state, " State. Please wait... ")

   all.files <- unzip(zipfile = zipfile,
                     list = TRUE,
                     unzip = "unzip")
  unzip(zipfile, unzip = "unzip", exdir = dirname(zipfile))

  if (stateRelated)
    message("Done")
  all.files
}








#' Convert Word documents to a text format
#'
#' @param docpath Path to an MS Word file
#' @param dest Destination folder
#' @param to_markdown Logical; whether to convert to markdown or not
#'
#' @import stringr
#' @importFrom readtext readtext
#'
#' @return The path to the converted text file.
#'
#' @export
convert_word_to_text <- function(docpath, dest = ".", to_markdown = FALSE)
{
  stopifnot(file.exists(docpath))
  message("Converting ", basename(docpath))
  txtpath <- docpath %>%
    str_replace("(.+)(\\.docx|\\.doc)$", "\\1\\.txt")
  if (to_markdown) {
  tryCatch({
    opts <- c("-f", "docx", "-t", "markdown", "-o")
    system2(command = "pandoc",
            args = c(shQuote(docpath), opts, shQuote(txtpath)))
  },
  error = function(e) {
    e
  },
  warning = function(w) w)
  }
  else {
    readtextObj <- readtext(docpath)
    cat(readtextObj$text, file = txtpath)
  }
  txtpath
}


#' Manually Check File Names
#'
#' @param directory A directory path
#'
#' @export
manually_check_filenames <- function(directory) {
      shell.exec(shQuote(normalizePath(directory)))
      readline(
        "Check the opened File Explorer to manually rename the file.When done, press <ENTER> to continue."
      )
}






#' Retrieve Word Files (containing transcripts)
#'
#' @param dir A diretory path
#'
#' @export
fetch_only_word_files <- function(dir) {
  list.files(
    path = dir,
    pattern = "\\.docx?$",
    recursive = TRUE,
    full.names = TRUE,
    include.dirs = TRUE
  )
}





#' Add some codes to the RQDA project
#'
#' @param codes A character vector of the new codes that are to be added.
#' @param rqdafile An RQDA project file
#'
#' @note The rationale behind creating this function is the fact that merging
#' the binary RQDA in Git version control is not feasible. Thus, codes can be
#' added to the existing ones and represented in the R session for further
#' uses downstream.
#'
#' @importFrom RQDA RQDA
#' @importFrom magrittr %>%
#'
#' @export
add_rqdacodes <- function(codes, rqdafile) {
  openProject(rqdafile)
  on.exit(closeProject())

  cd <- getCodingTable()$codename %>%
    c(codes) %>%
    unique %>%
    sort
  class(cd) <- "LocalCodes"
  cd
}
#
#
#
#
#
#
#
#' Save New Codes To Disk
#'
#' Saves a local representation of the codes of the RQDA project for viewing
#' and easy sharing in version control
#'
#' @param codes An object of class \code{LocalCodes} which are created by the
#' function \code{add_rqdacodes}.
#' @param dir Path to the directory to which to save the persistent data.
#' Defaults to the folder containing the RQDA project file.
#'
#' @export
store_rqdacodes <- function(codes, dir) {
  stopifnot(inherits(codes, "LocalCodes"))
  stopifnot(is.character(codes), dir.exists(dir))
  saveRDS(codes, file = file.path(dir, "CODES.rds"))
  cat(codes, file = file.path(dir, ".CODES"), sep = "\n")
}
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
  stopifnot(length(varsIndex) == length(chklist$opts))

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




#' Check whether a question is found across the sectors
#'
#' @param quest The question asked in the survey (acutally a regular expression)
#' @param table A list where each element is a character vector of all the
#' questions - extracted from the data frame labels.
#'
#' @return A named logical vector for all the sectors checked.
#'
#' @export
question_in_all_sectors <- function(quest, table) {
  map_lgl(table, function(labels) {
    labels %>%
      str_trim %>%
      str_squish %>%
      str_detect(quest) %>%
      any
  })
}








#' Manipulate Data with Multiple Choice Questions
#'
#' @param data A data frame with the collaped column
#' @param variable A character vector of length 1 representing the
#' name of the column to ve extended
#' @param options The would-be levels of the resulting factor column
#' @param multisectoral Logical. Whether work is on combined data frame
#' @param notReferral Logical. Whether not Referral Directory data
#'
#' @importFrom stats reorder
#' @importFrom tidyr pivot_longer
#'
#' @return  A modified data frame with the choices now appearing as
#' levels of a factor.
#'
#' @export
prepare_extended_col <-
  function(data,
           variable,
           options,
           multisectoral = TRUE,
           notReferral = multisectoral) {
    selected <-
      c(lgas = "lgas",
        sector = "sector",
        variable = variable)

    if (isFALSE(multisectoral))
      selected <- selected[-2]     # Test this!

    start <- if (isFALSE(notReferral))
      2L
    else
      3L

    data %>%   # handle exception for when sector column is not appropriate
      select(!!selected) %>%
      {
        for (i in seq_along(options)) {
          . <- extend_single_listcol(., variable, options[i])
        }
        .
      } %>%
      select(-variable) %>%
      pivot_longer(start:ncol(.)) %>%
      mutate(name = factor(name)) %>%
      mutate(name = reorder(name, desc(value))) %>%
      filter(value)
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









#' Plot the Services for a Single Sector Selected
#'
#' @param data The data frame
#'
#' @import ggplot2
#' @importFrom magrittr %>%
#'
#' @export
single_sector_services_plot <- function(data) {
  (data %>%
     ggplot(aes(name, fill = name)) +
     geom_bar() +
     theme(axis.text = element_text(
       angle = 20,
       vjust = 1,
       hjust = 1
     ),
     axis.title.x = element_blank(),
     axis.text.x = element_blank())
  )
}




#' Launch RQDA
#'
#' Attach RQDA to the Workspace and start up the GUI
#'
#' @importFrom RQDA RQDA
#'
#' @export
launch_rqda <- function() {
  RQDA()
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



#' List Local Government Areas
#'
#' @param state State in the Federation of Nigeria
#'
#' @import naijR
#' @export
lgas_ng <- function(state = NULL) {
  if (!is.null(state))
    lgas_nigeria$lga[lgas_nigeria$state == state]
  else
    lgas_nigeria$lga
}

