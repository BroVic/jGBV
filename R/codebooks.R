

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
