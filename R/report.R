# Source file: reports.R
#
# MIT License
#
# Copyright (c) 2019 Victor Ordu

#' Build A Report
#'
#' Generate a report in MS Word format (\code{.docx} format) from an R Markdown
#' template.
#'
#' @importFrom rmarkdown render
#'
#' @param outdir Destination directory of generated report
#' @param state RAAMP State for which report is generated
#' @param type character - either \code{basic} or \code{findings}
#' @param quietly logical; should output be printed to the console?
#' @param ... Other arguments passed to \code{rmarkdown::render}.
#'
#' @note By default, in interactive mode, the function will print progress
#' messages to the console, while it is quiet during a scripting session.
#'
#' @export
build_report <-
  function(outdir,
           state,
           type = c("findings", "generic"),
           quietly = !interactive(),
           ...)
  {
    stopifnot(state %in% raampStates)
    fnameState <- .removeSpaceForFilepath(state)
    type <- match.arg(type)
    input <- switch(
      type,
      findings = file.path(
        make_dir_tree()$reports,
        paste0("findings_", fnameState, ".Rmd")
      ),
      generic = .retrieveDocumentTemplate("raamp-gbv-report")
    )

    outputFile <- sprintf("%s_%s.docx", type, fnameState)
    tryCatch(
      render(
        input = input,
        output_format = "word_document",
        output_dir = outdir,
        output_file = outputFile,
        params = list(state = state),
        quiet = quietly,
        ...
      ),
      error = function(e) {
        message("Failed")
      }
    )
  }





#' Create A Clone of the Reporting Template
#'
#' @importFrom here here
#' @importFrom utils file.edit
#'
#' @export
clone_template <- function() {
  # TODO: Add logic for writing inserting 'state' into clone
  wd <- getwd()
  new.tmp <- file.path(wd, 'Untitled.Rmd')
  if (isFALSE(grepl(here::here(), wd, fixed = TRUE))) {
    warning("You are not working in the project's directory tree")
    return(NULL)
  }
  file.copy(
    system.file(
      "rmarkdown",
      "templates",
      "raamp-gbv-report",
      "skeleton",
      "skeleton.Rmd",
      package = 'raampGBV',
      mustWork = TRUE
    ),
    new.tmp
  )
  file.edit(new.tmp)
}





#' Generate Auto-numbers for Tables
#'
#' @param bookmark A string to represent the bookmark used in a given document
#'
#' @importFrom officer run_autonum
#'
#' @return See the documentation for \code{\link[officer]{run_autonum}}.
#'
#' @export
my_autonum <- function(bookmark = NULL) {
  if (is.null(bookmark))
    bookmark <- "iufmp"
  officer::run_autonum('tab', bkm = bookmark)
}
