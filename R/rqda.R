# Source file: rqda.R
#
# MIT License
#
# Copyright (c) 2019 Victor Ordu

globalVariables(c("cid", "codecat"))

#' Launch RQDA
#'
#' Attach RQDA to the Workspace and start up the GUI
#'
#' @export
launch_rqda <- function() {
  if (!rqda_is_installed())
    stop("Oops! You have not yet installed 'RQDA'.")

  RQDA::RQDA()
}





#' Get the paths to RQDA projects
#'
#' A convenience function for collecting RQDA projects developed by
#' different collaborators.
#'
#' @param datafolder The directory housing the projects. The default
#' value is internally determined and is a vestige from older projects
#' that is there only for backward compatibility.
#'
#' @return A character vector whose elements are the absolute paths to
#' individual RQDA projects.
#'
#' @export
get_rqda_projs <- function(datafolder = NULL) {
  if (is.null(datafolder))
    datafolder <- here::here("data/qual/rqda") # applies to old project

  pqDir <- sQuote(datafolder, q = FALSE)

  if (!dir.exists(datafolder))
    stop("The directory ", pqDir, " does not exist")

  ff <- list.files(datafolder, '\\.rqda$', full.names = TRUE)

  if (!length(ff))
    warning("No RQDA projects were found in ", pqDir)

  ff
}





#' Data related to codings
#'
#' Gets a data frame of all the codings across one or more RQDA projects.
#'
#' @param proj A character vector of the paths of one or more related
#' RQDA project databases
#' @param query A valid SQLite (i.e. including a terminating semicolon) in
#' the form of a string. Character vectors longer than \code{1L} are
#' truncated.
#'
#'
#' @details By default, \code{query} is used to provide a coding table
#' from the project that has the following relations: rowid, cid, fid,
#' codename, filename, index1, index2, CodingLength, codecat, and coder.
#' For addition details, visit
#' \code{\link[RQDA]{RQDATables}}.
#'
#' @return A data frame containing the data, either singly or stacked
#' when more than one are combined.
#'
#' @importFrom RQDAassist retrieve_codingtable
#' @importFrom purrr map_dfr
#' @importFrom purrr map_lgl
#'
#' @export
get_codings_master_df <- function(proj, query = NULL) {

  res <- map_lgl(proj, function(p) {
    projExists <- file.exists(p)

    if (!projExists)
      warning("No project ",
              sQuote(basename(p), q = FALSE),
              " was found and thus was excluded from the operation")

    projExists
  })

  proj <- proj[res]

  if (length(proj) == 0L)
    stop("No projects files were found for the operation")

  map_dfr(proj, retrieve_codingtable, query = query)  # NB: catid NAs abound!
}









#' Dataframes for specific Code Categories
#'
#'
#' Gives a list of data frames, each for a given code category
#'
#' @param dir A path to the directory housing the individual RQDA projects
#'
#' @return A list of data frames, one for each category
#'
#' @export
get_codecat_dfs <-
  function(dir = here::here("data/qual/rqda")) {

    tryCatch({
      message("Please wait... ")
      projfiles <- get_rqda_projs(dir)


      cdt <- get_codings_master_df(projfiles)

      split(cdt, cdt$codecat)
    })

  }





#' Get Selected Codings
#'
#' Fetches the portions of text that were selected i.e. the codings for a
#' given code in an RQDA project
#'
#' @param codedata A data frame containing the codings, usually as an output
#' of \code{RQDA::getCodingsTable}.
#' @param proj An RQDA project.
#' @param code The code for which the codings are to be retrieved
#'
#' @note For this function to work properly, the RQDA GUI needs to have been
#' opened prior to its being called.
#'
#' @return No value is returned but a window with the codings is opened
#' as a side effect.
#' @export
get_quotes <- function(codedata, proj, code) {
  if (!rqda_is_installed())
    stop("Package 'RQDA' not found")

  if (!is.character(code) || length(code) > 1)
    stop("'code' must be of type character and length 1")

  RQDA::openProject(proj)
  on.exit(RQDA::closeProject())
  code <- unique(codedata$cid[codedata$codename == code])
  len <- length(code)

  if (len > 1L)
    warning(paste("cid is ", code, sep = ", ", collapse = ' '), call. = FALSE)
  else if (len == 0L)
    stop("Requested code does not exist in this project")

  RQDA::getCodingsByOne(code)
}




## Performs a check whether RQDA is installed. This function exists to enable
## conditional imports for functions that require RQDA. This is necessary
## because this package is supposed to help with installing RQDA in the first
## place, as well as use it in some of the functions whenever it is available.
## Otherwise, we can cross the `R CMD check` hurdle.
rqda_is_installed <- function() {
  installed <- requireNamespace("RQDA", quietly = TRUE)

  if (!installed)
    warning("To install RQDA visit https://github.com/BroVic/RQDAassist")

  installed
}








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
#' @importFrom purrr map_df
#' @importFrom utils file_test
#'
#' @return The coding table as an R \code{data.frame}.
#'
#' @export
obtain_rqda_codes <- function(path) {
  if (!rqda_is_installed())
    stop("Required package 'RQDA' is not installed")

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
    stop("'path' is incorrect or non-existent", call. = FALSE)

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

    stop("RQDA project file not found in ", sQuote(finalDir), call. = FALSE)
  }
  else if (projectsFound > 1L)
    warning("More than one RQDA project file found")

  map_df(rqdafile, function(proj) {

    tryCatch({
      RQDA::openProject(proj)
      codes <- RQDA::getCodingTable()
      on.exit(RQDA::closeProject())
      codes
    }, error = function(e) e)

  })
}
