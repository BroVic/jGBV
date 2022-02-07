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
#' @param tmplname A string naming the actual template.
#' @param type A character vector of length 1L, representing the type of template
#' to be loaded. One of \emph{dummy}, \emph{report}, \emph{codebook}, or
#' \emph{capacity}.
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
  function(tmplname,
           type = c("dummy", "report", "codebook", "capacity"),
           ...)
  {
    dir <- if (missing(tmplname)) {
      type <- match.arg(type)
      .Templates(type)
    }
    else
      tmplname
    if(!nzchar(system.file("rmarkdown/templates", dir, package = thisPkg())))
      stop("There is no template ", sQuote(dir))
    fn <- paste0(dir, ".Rmd")
    rmarkdown::draft(fn, dir, thisPkg(), ...)
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
  if (identical(tmpl, ""))
    stop("Codebook template not found")
  tmpl
}









#' Produces dual outputs (table & plot) for multi-response variables
#' @param dat The data frame
#' @param indices Columns to be used
#' @param ... Arguments passed for internal used
#' @param na.rm Logical; whether to keep NAs or not.
#'
#' @importFrom flextable theme_box
#' @importFrom flextable autofit
#' @importFrom magrittr %>%
#'
#' @export
dual_multiopts <- function(dat, indices, ..., na.rm = TRUE) {
  stopifnot(is.numeric(indices))
  dat <- applyFilter(dat, ...)
  ft <- table_multiopt(dat, indices = indices) %>%
    theme_box() %>%
    autofit()
  pp <- show_output(dat, indices, size = getOption("jgbv.axis.text.size"))
  list(table = ft, plot = pp)
}








#' Makes outputs like above but for single response questions
#'
#' @param dat The data frame
#' @param x,y An integer or character vector of length \code{1L} for selecting
#' a column from \code{.data}.
#' @param ... Arguments passed on to \code{applyFilter}.
#' @param na.rm Logical
#'
#' @import ggplot2
#' @importFrom flextable flextable
#' @importFrom flextable theme_box
#' @importFrom flextable autofit
#' @importFrom magrittr %>%
#'
#' @export
dual_singleopts <- function(dat, x, y, ..., na.rm = TRUE) {
  stopifnot(is.numeric(x))
  if (!missing(y))
    stopifnot(is.numeric(y))
  dat <- applyFilter(dat, ...)
  tb <- table_singleopt(dat, x, y, data.only = TRUE)
  gg <- if (missing(y)) {
    ggplot(tb, aes(Variable, Freq))
  }
  else
    ggplot(tb, aes(Variable, y))
  gg <- gg +
    theme(axis.text = element_text(size = getOption("jgbv.axis.text.size")))
  ft <- tb %>%
    flextable() %>%
    # theme_box() %>%
    autofit()
  list(table = ft, plot = gg + geom_col())
}







applyFilter <-
  function(data,
           filter = NULL,
           na.rm = TRUE,
           opt.col = "type_of_service")
  {
    vars <- colnames(data)
    colsOfInterst <-
      grep(opt.col,
           vars,
           value = TRUE,
           ignore.case = TRUE)
    if (is.null(filter))
      return(data)
    choiceVal <-
      grep(filter,
           colsOfInterst,
           value = TRUE,
           ignore.case = TRUE)
    choiceIndex <- match(choiceVal, vars)
    data <- data[data[[choiceIndex]],]
    if (na.rm)
      data <- data[!is.na(data[[choiceIndex]]),]
    data
  }
