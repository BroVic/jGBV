globalVariables(c("Variable", "Percentage of Facilities"))

plot_multiopt <- function(x, ...)
  UseMethod("plot_multiopt")

#' Draw a plot of multi-response questions
#'
#' Draw a plot of multi-response questions using the code for creating
#' the complimentary tables. This construct is used to avoid
#' repetitions.
#'
#' @param x An expression being captured code from a valid call to
#' \code{table_multiplot}
#' @param ... left for future expansion of the function
#'
#' @export
plot_multiopt.default <- function(x, ...) {
 stopifnot(is.language(x))

  dd <- .getDataOnlyFromExpression(x)

  .generatePlot(dd)
}




plot_multiopt.data.frame <- function(x, ...)
{
  matchers <- c("Option", "Variable", "Frequency")
  if (sum(matchers %in% colnames(x)) != 3L)
    stop("Object is not a data frame of multi-response outputs")
  .generatePlot(x)
}




##
.getDataOnlyFromExpression <- function(expr) {
  cc <- as.call(expr)
  cc$data.only <- TRUE
  eval(cc)
}


#' @import dplyr
#' @import ggplot2
#' @importFrom stringr str_replace
.generatePlot <- function(data)
{
  gg.col <- data %>%
    rename_at(vars(starts_with("Percentage")),
              ~ str_replace(., "^.+$", "Percentage of Facilities")) %>%
    ggplot(aes(Variable, `Percentage of Facilities`)) +
    geom_col() +
    theme(axis.text.x = element_text(size = 8))

  .fitIntoLines <- function(x) {
    gsub("\\s", "\n", x)
  }

  gg.col +
    scale_x_discrete(label = .fitIntoLines)
}


plot_yesno <- function(expr, ...) {
  stopifnot(is.language(expr))

  dd <- .getDataOnlyFromExpression(expr)

  dd %>%
    pivot_longer(1:2, "Response", values_to = "Frequency") %>%
    ggplot(aes(Response, Frequency)) + geom_col()

}
