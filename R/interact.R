# Source file: interact.R
#
# MIT License
#
# Copyright (c) 2019 Victor Ordu

#' collect Name of RAAMP State
#'
#' Collect the name of a given State for the purpose of using it to build a
#' given part of the project that is state-dependent.
#'
#' @details This function will collect input both from the shell and during
#' interactive R sessions. Also, if a State's name is supplied as an argument
#' to \code{Rscript}, it will read it and apply it as required.
#'
#' @export
input_state <- function() {
  prompt <- "Enter State: "

  state <- if (!interactive()) {
    args <- commandArgs(trailingOnly = TRUE)

    if (identical(args, character(0))) {
      cat(prompt)
      readLines("stdin", n = 1L)
    }
    else
      args
  } else
    readline(prompt = prompt)

  paste(state, collapse = " ")
}







confirm_states <- function(x = NULL) {
  all.st <-  getOption("jgbv.project.states")
  if (is.null(x))
    return(all.st)
  x <- stringr::str_to_title(x)
  match.arg(x, all.st)
}





pick_one_state <- function() {
  ss <- confirm_states()
  ss[menu(ss, TRUE, "Select a State")]
}
