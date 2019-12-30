

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
#'
#' @importFrom naijR states
#' @importFrom rmarkdown render
#'
#' @return The path of the rendered Rmarkdown file i.e. the return value for
#' \code{rmarkdown::render}. If building the document is interrupted by a
#' condition, the function fails silently and the condition object is returned.
#'
#' @export
build_codebook <- function(state, tool, outdir) {
  stopifnot(is_project_state(state))
  tmpl <-
    system.file(
      "rmarkdown",
      "templates",
      "survey-template-codebook",
      "skeleton",
      "skeleton.Rmd",
      package = "raampGBV"
    )
  if (identical(tmpl, ""))
    stop("Codebook template not found")
  try({
    render(
      input = tmpl,
      output_format = "html_document",
      output_dir = outdir,
      output_file = sprintf("codebook_%s_%s.html", tool, state),
      params = list(state = state, toolType = tool),
      quiet = TRUE
    )
  }, silent = TRUE)
}
