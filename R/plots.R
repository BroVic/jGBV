#' Draw a plot of multi-response questions
#'
#' Draw a plot of multi-response questions using the code for creating
#' the complimentary tables. This construct is used to avoid
#' repetitions.
#'
#' @param expr An expression being captured code from a valid call to
#' \code{table_multiplot}
#' @param ... left for future expansion of the function
#'
#' @import dplyr
#' @import ggplot2
#' @importFrom stringr str_replace
#'
#' @export
plot_multiopt <- function(expr, ...) {
  stopifnot(is.language(expr))

  dd <- .getDataOnlyFromExpression(expr)

  gg.col <- dd %>%
    rename_at(vars(starts_with("Percentage")),
              ~ str_replace(., "^.+$", "Percentage of Facilities")) %>%
    ggplot(aes(Variable, `Percentage of Facilities`)) +
    geom_col()

  .fitIntoLines <- function(x) {
    gsub("\\s", "\n", x)
  }

  gg.col +
    scale_x_discrete(label = .fitIntoLines)
}





##
.getDataOnlyFromExpression <- function(expr) {
  cc <- as.call(expr)
  cc$data.only <- TRUE
  eval(cc)
}



# Creates an expression from x, the function for building tables,
# and draws plot as well as produces a matching object
# of class 'flextable'
# @param expr An expression that produces the table for the relevant data
show_output <- function(expr, type = c("multiplot", "yesno"), ...) {
  exp <- substitute(expr)

  type <- match.arg(type)
  p <- if (type == "multiplot")
    plot_multiopt(exp, ...)
  else if (type == 'yesno')
    plot_yesno(exp, ...)
  else
    stop("No function provided for this type of output")

  # Draw chart
  print(p)

  # Output flextable
  eval(exp)
}


plot_yesno <- function(expr, ...) {
  stopifnot(is.language(expr))

  dd <- .getDataOnlyFromExpression(expr)

  dd %>%
    pivot_longer(1:2, "Response", values_to = "Frequency") %>%
    ggplot(aes(Response, Frequency)) + geom_col()

}
