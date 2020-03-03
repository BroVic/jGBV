# Source file: files.R
#
# MIT License
#
# Copyright (c) 2019 Victor Ordu

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
#
#
#
#
#
#
#
#
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





#' Select a file from a directory
#'
#' Selects a file from a vector of file names based on a regex pattern
#'
#' @param path.list A character vector of paths
#' @param pattern A regular expression
#' @param type The file format. The functions only support \emph{.R} and
#' \emph{CSV} files, with R scripts being the default format.
#'
#' @return If successful, the path of the selected file.
#'
#' @export
select_file <- function(path.list, pattern, type = c("R", 'csv')) {
  if (!all(file.exists(path.list)))
    stop("One or more paths do not exist")
  type <- match.arg(type)
  pattern <- sprintf("%s.*\\.%s$", pattern, type)
  path <- grep(pattern, path.list, ignore.case = TRUE, value = TRUE)
  numfiles <- length(path)

  if (numfiles > 1L)
    stop("Expected to select only 1 file, but now ",
         as.character(numfiles),
         " were selected")

  if (!file.exists(path))
    stop("Path ", sQuote(path), " does not exist")

  path
}


#' Make a Representation of the Project's Standard Directory Tree
#'
#' @importFrom here here
#' @importFrom purrr map
#' @importFrom stats setNames
#'
#' @export
make_dir_tree <- function()
{
  if (!identical(basename(here()), "RAAMP_GBV"))
    stop("You are not in the 'RAAP_GBV' working directory or repository")
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
    codebooks = file.path(dList$src, cod, "codebooks"),
    reports = file.path(dList$src, "rep")
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
  stopifnot(is.character(file.ext))

  if (!dir.exists(dir))
    stop('The directory does not exist')

  pattern <- sub("(\\.?)([[:alnum:]]+$)", "\\1\\2", file.ext)
  the.list <- list_files_pattern(dir, pattern)

  if (identical(the.list, character(0)))
    warning("No filepaths returned")

  the.list
}















#' Save an object with specific details
#'
#' This is a custom wrapper for \code{saveRDS}.
#'
#' @param dir The directory to which to save the RDS file
#' @param obj The object to be saved.
#' @param filelabel A label to be applied to the filename.
#' @param state The State for whose data we are saving (specific to
#' RAAMP-GBV project)
#' @param ... Additional strings to add to filename
#'
#'
#' @export
save_as_rds <- function(dir, obj, filelabel, ..., state) {
  if (!dir.exists(dir))
    stop("No directory called ", sQuote(dir))
  stopifnot(is_project_state(state))
  if (any(grepl("\\s", state)))
    state <- .removeSpaceForFilepath(state)
  pth <- file.path(dir, state, paste0(filelabel, "_", state, "_", ..., ".rds"))
  saveRDS(obj, pth)
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
fetch_all_data <- function(state, dir, sectors)
{
  stopifnot(state %in% raampStates)
  statePath <- .removeSpaceForFilepath(state)
  dPath <- file.path(dir, statePath)

  map(sectors, function(sector) {
    regex <- glue("transformed.+{statePath}.+{sector}")
    rds <- list.files(dPath, pattern = regex, full.names = TRUE)
    if (is.null(rds))
      stop("Data file not found")
    readRDS(rds)
  }) %>%
    setNames(sectors)
}





#' Optimize Filenames Containing States
#'
#' Remove space in a State's name for use with filepaths. This is to improve
#' portability for platforms and utilities e.g. \code{make} that do not work
#' well with filepaths that have spaces in them.
#'
#' @param state A character vector of length 1.
#'
#' @details If \code{length(state) > 1L}, all additional elements are removed
#' silently. Also, \code{state} is expected to be a State in Nigeria; illegal
#' input will produce an error and the function will fail.
#'
#' @return The State with any speaces replaced with a hyphen
#'
#' @examples
#'   .removeSpaceForFilepath("Akwa Ibom")
#'
#'
#' @export
.removeSpaceForFilepath <- function(state)
{
  state <- state[1]
  stopifnot(is_project_state(state))
  sub("\\s", "-", state)
}

