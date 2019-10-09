#' Interviews
#'
#' This function constructs an S3 object called \code{interview}, which
#' represents the questions asked in the tools. The object also has
#' regular expressions, one for each questions, that can be utilized in
#' carrying out string matching during the analysis.
#'
#' @importFrom Hmisc escapeRegex
#' @importFrom stats setNames
#'
#' @param questions A character vector of the question(s)
#'
#' @return The function returns an object of class \code{interview}.
#'
#' @examples
#' obj <- interview("What are the issue(s)?")
#' obj$question
#' obj$regex
#'
#' @export
interview <- function(questions)
{
  len <- length(unlist(questions))
  nms <- rep(NA, len)
  rgx <- escapeRegex(questions)
  obj <- list()

  for (i in seq_along(questions)) {
    nms[i] <- paste0("Q", i)
    obj[[i]] <- list(
      question = questions[i],
      regex = rgx[i]
    )
  }
 structure(obj, class = "interview", names = nms)
}

