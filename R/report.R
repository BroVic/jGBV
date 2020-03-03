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
