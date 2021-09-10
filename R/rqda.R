# Source file: rqda.R
#
# MIT License
#
# Copyright (c) 2019 Victor Ordu

globalVariables(c("cid", "codecat"))

#' Launch RQDA
#'
#' Attach RQDA to the Workspace and start up the GUI
#'
#' @importFrom RQDA RQDA
#' @export
launch_rqda <- function() {
  RQDA()
}





#' Get the paths to RQDA project
#'
#' Collect RQDA projects developed by different collaborators
#'
#' @param datafolder The directory housing the projects
#'
#' @export
get_rqda_projs <- function(datafolder = here::here("data/qual/rqda")) {
  stopifnot(dir.exists(datafolder))
  list.files(datafolder, '\\.rqda$', full.names = TRUE)
}





#' Data related to codings
#'
#' GEts a data frame of all the codings across different RQDA projects
#'
#' @param projects A character vector of the paths of one or more related
#' RQDA project databases
#'
#' @importFrom purrr map_dfr
#'
#' @return A data frame containing the data from the combined projects
#'
#' @export
get_codings_master_df <- function(projects) {
  stopifnot(all(file.exists(projects)))
  map_dfr(projects, retrieve_codingtable)  # NB: catid NAs abound!
}





#' @import RQDA
#' @import dplyr
retrieve_codingtable <- function(proj, query = NULL) {
  RQDA::openProject(proj)
  on.exit(RQDA::closeProject())

  tb <- if (is.null(query)) {
    qry <- paste("sELECT treecode.cid AS cid,",
            "codecat.name AS codecat",
            "FROM treecode, codecat",
            "WHERE treecode.catid=codecat.catid AND codecat.status=1;")
    cdt <- RQDA::getCodingTable()
    cats <- RQDA::RQDAQuery(qry) %>%
      group_by(cid) %>%
      count(codecat)
    cdt$codecat <- NA
    for (i in seq_len(nrow(cats))) {
      ind <- which(cdt$cid %in% cats$cid[[i]])
      cdt$codecat[ind] <- cats$codecat[i]
    }
    cdt
  }
  else
    RQDA::RQDAQuery(query)

  ## Get coder's name
  nm <-
    sub("(IUFMP\\s-\\s)([[:upper:]][[:lower:]]+)(\\.?\\d*\\.rqda)$",
        "\\2",
        basename(proj))

  mutate(tb, coder = nm)
}





#' Dataframes for specific Code Categories
#'
#'
#' Gives a list of data frames, each for a given code category
#'
#' @param dir A path to the directory housing the individual RQDA projects
#'
#' @return A list of data frames, one for each category
#'
#' @export
get_codecat_dfs <-
  function(dir = here::here("data/qual/rqda")) {

    tryCatch({
      message("Please wait... ")
      projfiles <- get_rqda_projs(dir)


      cdt <- get_codings_master_df(projfiles)

      split(cdt, cdt$codecat)
    })

  }





#' Get Selected Codings
#'
#' Fetches the portions of text that were selected i.e. the codings for a
#' given code in an RQDA project
#'
#' @param codedata A data frame containing the codings usually as an output
#' of \code{\link[RQDA]{getCodingsTable}}.
#' @param proj An RQDA project.
#' @param code The coding for which the codings are to be retrieved
#'
#' @import RQDA
#'
#' @note For this function to work properly, the RQDA GUI needs to have been
#' opened prior to its being called.
#'
#' @return No value is returned but a window with the codings is opened
#' as a side effect.
#' @export
get_quotes <- function(codedata, proj, coding) {
  RQDA::openProject(proj)
  on.exit(RQDA::closeProject())

  code <- unique(codedata$cid[codedata$codename == coding])
  len <- length(code)

  if (len > 1L)
    warning(paste("cid is ", code, sep = ", ", collapse = ' '), call. = FALSE)
  else if (len == 0L)
    stop("Requested code does not exist in this project")

  RQDA::getCodingsByOne(code)
}
