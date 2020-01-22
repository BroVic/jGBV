
#' Build A Report
#'
#' Generate a report in MS Word format (\code{.docx} format) from an R Markdown
#' template.
#'
#' @importFrom rmarkdown render
#'
#' @param outdir Destination directory of generated report
#' @param state RAAMP State for which report is generated
#' @param quietly logical; should output be printed to the console?
#'
#' @note By default, in interactive mode, the function will print progress
#' messages to the console, while it is quiet during a scripting session.
#'
#' @export
build_report <- function(outdir, state, quietly = !interactive())
{
  stopifnot(state %in% raampStates)
  template <- .retrieveDocumentTemplate("raamp-gbv-report")
  filenameForState <- .removeSpaceForFilepath(state)
  outputFile <- sprintf("findings_%s.docx", state)
  tryCatch(
    render(
      input = template,
      output_format = "word_document",
      output_dir = outdir,
      output_file = outputFile,
      params = list(state = state),
      quiet = quietly,
      envir = globalenv()
    ), error = function(e) { message("Failed") })
}
