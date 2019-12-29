#' Load data for Shiny app
#'
#' @param state The RAAMP-GBV Assessment State for which data are to be
#' visualised
#'
load_shiny_data <- function(state)
{
  if (length(grep(state, naijR::states())))
    stop(sQuote(state), " is not a Nigerian State")
  stopifnot(state %in% c("Abia", "Akwa Ibom", "Bauchi", "Ogun"))
  dir.path <- file.path(make_dir_tree()$quant, state)
  file <- list_files_pattern(dir.path, "common.+\\.rds")
  readRDS(file)
}
