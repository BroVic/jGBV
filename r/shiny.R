#' Load data for Shiny app
#'
#' Loads data that are specifically tailored for the project's Shiny application.
#'
#' @param state The RAAMP-GBV Assessment State for which data are to be
#' visualised
#'
#' @return A data frame.
#'
#' @export
load_shiny_data <- function(state)
{
  if (missing(state))
    stop("Argument 'state' was not supplied with no default")
  stopifnot(is_project_state(state))

  dir.path <- file.path(make_dir_tree()$quant, state)
  file <- list_files_pattern(dir.path, "common.+\\.rds")
  readRDS(file)
}
