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







.confirmStates <- function(x = NULL) {
  all.st <-  getOption("jgbv.project.states")
  if (is.null(x))
    return(all.st)
  x <- stringr::str_to_title(x)
  match.arg(x, all.st)
}




#' Interactively Select a State
#'
#' Uses global options to identify the States for a given project and prompts
#' the user to select one.
#'
#' @importFrom utils menu
#'
#' @export
pick_one_state <- function() {
  ss <- .confirmStates()
  ss[menu(ss, TRUE, "Select a State")]
}
