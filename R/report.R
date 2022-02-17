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
      generic = .retrieveDocumentTemplate("gbv-report")
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







#' R Markdown Templates
#'
#' \code{open_template} creates an R Markdown template for various types of
#' intermediate as well as final project outputs such as dummy tables, analysis
#' reports, etc. \code{clone_template} creates a clone of the main reporting
#' template.
#'
#' @param type A character vector of length 1L, representing the type of
#' template to be loaded. One of \emph{dummy}, \emph{report},
#' \emph{codebook}, or \emph{capacity}.
#' @param tmplname A string naming the actual template.
#' @param ... Other arguments passed on to \code{\link[rmarkdown]{draft}},
#' apart from \code{file}, \code{template}, and \code{package}, which are
#' set internally.
#'
#' @importFrom rmarkdown draft
#'
#' @return These functions will create base R Markdown documents that the
#' the user can now modify fo reporting. Every effort is being made to
#' increasingly automating the actual process of building the reports.
#'
#' @export
open_template <-
  function(type = c("dummy", "report", "codebook", "capacity"),
           tmplname,
           ...)
  {
    dir <- if (missing(tmplname)) {
      type <- match.arg(type)
      .Templates(type)
    }
    else tmplname
    if(!nzchar(system.file("rmarkdown/templates", dir, package = thisPkg())))
      stop("There is no template ", sQuote(dir))
    fname <- paste0(dir, ".Rmd")
    rmarkdown::draft(fname, dir, thisPkg(), ...)
  }







# Picks one out of several available R Markdown templates
.Templates <- function(tmpl) {
  dirs <- c(
    "dummy-tables",
    "report-gbv",
    "codebook-survey",
    "capacity-needs"
  )
  ll <- lapply(dirs, function(str) list(dirname = str))
  names(ll) <- sub("(^[[:alpha:]]+)([[:punct:]])(.+)", "\\1", dirs)
  ll[[tmpl]]$dirname
}







#' @rdname open_template
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
      "gbv-report",
      "skeleton",
      "skeleton.Rmd",
      package = thisPkg(),
      mustWork = TRUE
    ),
    new.tmp
  )
  file.edit(new.tmp)
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
    package = thisPkg()
  )
  if (!nzchar(tmpl))
    stop(sprintf("Template %s was not found", sQuote(dir)))
  tmpl
}
