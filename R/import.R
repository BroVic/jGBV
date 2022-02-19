#' Extract interview transcripts for RQDA
#'
#' A script to extract the zip archive containing interview transcripts
#'
#' @details  First we pick the archive interactively and unzip it into the same
#' directory containing the archive. After establishing the directory
#' for the extraction, we modify the names of the files by removing
#' all spaces therein. Then we move them into the State-specific
#' folder. When finalising, we do some checks that everything went well
#' and we remove the directory that was created for the extraction.
#'
#' @param zipfile Path to a ZIP archive containing interview transcripts. When
#' \code{NULL}, a file selection dialog is presented to the user - this only works
#' in interactive sessions.
#' @param patterns A regular expression or matching string for selecting LGAs.
#' @param ignore_case Logical; whether to consider the case when matching.
#' @param no. The number of projects to be created as a numeric vector of length 1.
#'
#' @import utils
#' @importFrom here here
#' @importFrom purrr map_lgl
#' @importFrom purrr walk
#' @importFrom purrr walk2
#'
#' @return For \code{lga_match_function}, a function that takes a list of patterns
#' for different states. No value is returned by \code{import_transcriipts}
#'
#' @export
import_transcripts <-
  function(zipfile = NULL, patterns = NULL, ignore_case = FALSE, no. = NULL)
{
  anykey <- '(Press <ENTER> to continue): '
  if (is.null(zipfile)) {
    if (!interactive())
      stop("A path to a ZIP archive must be provided in non-interactive mode")
    # readline(paste("Please select a zip archive to extract", anykey))
    zipfile <- file.choose()
    if (isFALSE(endsWith(zipfile, ".zip")))  # TODO: Consider a low-level check.
      stop("Selected file is not a .ZIP file")
  }
  if (!is.null(no.) && !is.numeric(no.))
    stop("'no.' must be either be NULL or numeric")

  # New directory tree setup
  dir <- dirname(zipfile)
  zipNoExtension <- zipfile %>%
    basename %>%
    substr(0, regexpr('\\.zip$', .) - 1)
  extractDirpath <- file.path(dir, zipNoExtension)

  # rename all files to safer names i.e. without any spaces
  safeDirpath <- extractDirpath %>%
    basename %>%
    .changenames %>%
    file.path(dirname(extractDirpath), .)
  txtDirpath <- file.path(safeDirpath, 'txt')
  dir.create(txtDirpath, recursive = TRUE)

  # Extract files into the new directory, if not done earlier
  if (!length(list.files(safeDirpath, ".\\docx"))) {
    tryCatch({
      cat("Extracting the ZIP archive... ")
      extracted <- unzip(zipfile, exdir = safeDirpath)
      cat("Done\n")
    }, error = function(e)
      cat("Failed\n"))
    # safeDirpath <- dirname(extracted[1])  # TODO: This is a hack. Make more robust.

    # Treat the filenames here as well by removing spaces, if any.
    # Then conduct a check to see if there are files that were not
    # successfully renamed and list them for the user's attention.
    # This should not affect continued execution of the function.
    result <-
      map_lgl(extracted, function(fn)
        file.rename(fn, .changenames(fn)))
    if (sum(!result)) {
      notchanged <- extracted[!result]
      cat("Filenames not changed:\n", sprintf("* %s\n", notchanged))
    }
  }

  ## Convert the transcripts from MS Word to plain text and store
  ## in the newly created folder named 'txt' at the same level
  walk(list.dirs(safeDirpath), function(dir) {
    docfiles <-
      list.files(dir, "\\.docx?$", ignore.case = ignore_case, full.names = TRUE)
    if (0L == length(docfiles))
      return()
    RQDAassist::read_transcript(
      destdir = txtDirpath,
      docxfiles = docfiles,
      ignore_missing_files = TRUE) # TODO: fine-tune in package
  })

  # Generate new RQDA projects and import text files
  rqdaDirpath <- here("data/rqda")
  dir.create(rqdaDirpath, recursive = TRUE)
  opt <-
    menu(c("Yes", "No"), FALSE, "Do you want to generate a new project?")
  if (opt == 1L) {
    num <- if (is.null(no.)) {
    readline("Enter the number of projects to be created: ") %>%
      as.numeric
    }
    else
      no.
    num <- seq_len(num)
    state <- pick_one_state()
    walk(num, replicate_rqdaproj, dir = rqdaDirpath, state = state)

    ## Note that based on the patterns supplied, the assumption
    ## is that each filename starts with the name of the LGA
    allprojects <-
      list.files(paste(rqdaDirpath, state, sep = '/'), full.names = TRUE)
    walk2(
      allprojects,
      patterns,
      write_files_to_project,
      sourcedir = txtDirpath,
      ignore_case = ignore_case
    )
  }
  else {
    readline(paste("Select an RQDA project", anykey))
    rq <- file.choose()
    readline(paste("Select text files to import", anykey))
    tryCatch({
      cat("File import ")
      RQDA::openProject(rq)
      filelist <- RQDAassist::make_FileList(choose.files())
      RQDA::write.FileList(filelist)
      cat("was successful\n")
    },
    error = function(e)
      cat("failed\n"),
    finally = RQDA::closeProject())
  }
}








#' @rdname import_transcripts
#'
#' @param state The project State for which the matches/patters are to be
#' extracted.
#' @param ... Optional list of States to be manually provided for pattern
#' selection.
#'
#' @return
#'
#' @note The list of project States is set internally by
#' \code{getOption("jgbv.project.states")}. This option is used by the
#' GBV assessment project beginning from 2021, and does not apply to earlier
#' ones.
#'
#' To get the appropriate patterns or matches to be used for the extracting
#' function, there is need to inspect the list of transcript files. There, it
#' will be discovered how they are named and differentiated by LGA.
#'
#' @export
lga_matching_function <- function(state, ...) {
  proj.states <- getOption("jgbv.project.states")
  if (is.null(proj.states))
    proj.states <- c(...)
  tryCatch({
    state <- match.arg(state, proj.states)
  }, error = function(e) {
    warning("'state' is not one of the project States")
    stop(e)
  })
  function(lgalist) {
    names(lgalist) <- proj.states
    lgalist[[state]]
  }
}







#' Creates a new RQDA project from the existing base project
#'
#' @param serialno A serial number for the project.
#' @param dir The directory where the project is to be saved to.
#' @param state A state of Nigeria where the data were collected from.
#'
#' @importFrom here here
#'
#' @return The value of \code{\link[base]{file.copy}}.
#'
#' @export
replicate_rqdaproj <- function(serialno, dir, state)
{
  stopifnot(is.character(state), is.numeric(serialno))
  gbvproj <- getOption("jgbv.project.name")
  base.rqda.proj <- here(dir, paste0(gbvproj, ".rqda"))
  if (!file.exists(base.rqda.proj))
    stop(paste("The base RQDA project could not be found.",
               "Please make sure it is created and placed",
               "in the directory", sQuote("data/rqda")))
  savdir <- here(dir, state)
  if (!dir.exists(savdir)) {
    dir.create(savdir, recursive = TRUE)
    warning("The directory ", sQuote(savdir), " was created")
  }
  newproj <- sprintf("%s-%s-%d.rqda", gbvproj, state, serialno)
  newsave <- file.path(savdir, newproj)
  file.copy(base.rqda.proj, newsave)
}









# Removes spaces in the names i.e. rename the files/directories
#' @importFrom stringr str_replace_all
#' @importFrom stringr str_squish
#' @importFrom stringr str_trim
.changenames <- function(oldname) {
  stopifnot(is.character(oldname))
  oldname %>%
    str_trim() %>%
    str_squish() %>%
    str_replace_all("\\s", "_")
}






# TODO: Send to RQDAassist package
#' Write Text Files to an RQDA Project
#'
#' Selects a number of files to RQDA based on a regular expression
#' matched on the filename.
#'
#' @param proj The path to the RQDA project.
#' @param pattern A regular expression for matching selected files.
#' @param ignore_case Logical; whether to consider the case when matching.
#' @param sourcedir The path to the directory containing the text files.
#'
#' @note The files to be imported are expected to be in plain text format.
#'
#' @return The function has the side-effect of altering the state of the
#' indicated RQDA project.
#'
#' @export
write_files_to_project <- function(proj, pattern = NULL, ignore_case = FALSE, sourcedir = NULL) {
  files <-
    list.files(sourcedir, pattern, ignore.case = ignore_case, full.names = TRUE)

  flist <- RQDAassist::make_FileList(files)
  message("Writing text files to project ", basename(proj))
  RQDA::openProject(proj)
  on.exit(RQDA::closeProject())
  RQDA::write.FileList(flist)
}
