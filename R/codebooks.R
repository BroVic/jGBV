


# Internal function to generate a codebook's filename
.getCodebookFileName <- function(state, sector) {
  stopifnot(is_project_state(state))
  stopifnot(sector %in% tool.sectors)
  state <- .removeSpaceForFilepath(state)
  sprintf("codebook_%s_%s.html", state, sector)
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
      fpath <- build_codebook(state, sector)
      cat("Done\n")
      fpath
    })
  }) %>%
    setNames(states)
}








#' Build Quantitative Codebook
#'
#' Render an individual codebook for a given State and Tool
#'
#' @param state The State where the Assessment was conducted
#' @param tool The specific category of tool that was administered
#' @param outdir The output directory
#' @param quietly A logical value of \code{TRUE} or \code{FALSE} - whether the
#' output of knitting the documents should be printed out or not.
#'
#' @importFrom here here
#' @importFrom naijR states
#' @importFrom rmarkdown render
#'
#' @return The path of the rendered Rmarkdown file i.e. the return value for
#' \code{rmarkdown::render}. If building the document is interrupted by a
#' condition, the function fails silently and the condition object is returned.
#'
#' @export
build_codebook <- function(state, tool, outdir, quietly = TRUE)
{
  stopifnot(is_project_state(state))
  stopifnot(tool %in% tool.sectors)
  tmpl <- .retrieveDocumentTemplate("survey-template-codebook")

  if (missing(outdir)) {
    if (!identical(basename(here()), "RAAMP_GBV"))
      stop("Wrong project root. Navigate to RAAMP_GBV project directory")
    outdir <- here("doc/output", state)
  }
  message(sprintf("Building %s codebook for %s State... ", tool, state))
  outputFile <- .getCodebookFileName(state, tool)  # also removes any spaces
  tryCatch(
    render(
      input = tmpl,
      output_format = "html_document",
      output_dir = outdir,
      output_file = outputFile,
      params = list(state = state, toolType = tool),
      quiet = quietly
    ), error = function(e) { message("Failed") })
}





#' View a codebook
#'
#' Opens up a quantitative codebook into the default web browser.
#'
#' @param state One of the project states
#' @param sector The sector of interest as defined by \code{tool.sectors}
#'
#' @export
show_codebook <- function(state, sector) {
  stopifnot(is.character(state))
  stopifnot(is.character(sector))
  fname <- .getCodebookFileName(state, sector)
  fpath <- here::here("doc", "output", state, fname)
  if (!file.exists(fpath))
    stop(sprintf("The file '%s' does not exist", fpath))
  shell.exec(fpath)
}
