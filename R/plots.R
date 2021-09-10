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
#' \code{table_multiplot}.
#' @param ... arguments to control axis label text via
#' \code{\link[ggplot2]{element_text}}.
#'
#' @export
plot_multiopt.default <- function(x, ...) {
 stopifnot(is.language(x))

  dd <- .getDataOnlyFromExpression(x)

  .generatePlot(dd, ...)
}




plot_multiopt.data.frame <- function(x, ...)
{
  matchers <- c("Option", "Variable", "Frequency")
  if (sum(matchers %in% colnames(x)) != 3L)
    stop("Object is not a data frame of multi-response outputs")
  .generatePlot(x, ...)
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
.generatePlot <- function(data, ...)
{
  gg.col <- data %>%
    rename_at(vars(starts_with("Percentage")),
              ~ str_replace(., "^.+$", "Percentage of Facilities")) %>%
    ggplot(aes(Variable, `Percentage of Facilities`)) +
    geom_col() +
    theme(axis.text = element_text(...))

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
    ggplot(aes(Response, Frequency)) +
    geom_col() +
    theme(axis.text = element_text(...))

}




#' Display output
#'
#' Displays some output such as a plot/
#'
#' @param x An object
#' @param ... Additional arguments
#'
#' @rdname outputs
#'
#' @export
show_output <- function(x, ...)
  UseMethod("show_output")

#' Creates an expression from x, the function for building tables,
#' and draws plot as well as produces a matching object
#' of class 'flextable'
#'
#' @param x An expression that produces the table for the relevant data
#' @param type The type of output. Options are \code{multiplot} and
#' \code{yesno}
#' @param ... Arguments passed to \code{plot_multiplot} or \code{plot_yesno}
#'
#' @rdname outputs
#'
#' @export
show_output.default <-
  function(x, type = c("multiplot", "yesno"), ...) {
    exp <- substitute(x)

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




#' @rdname outputs
#'
#' @param x An object of class \code{data.frame} that has the data used in the
#' analysis
#' @param index Integer vector representing indices for the column(s) of
#' interest
#' @param type Either "table" or "plot".
#' @param use.regex Logical.
#' @param ... Arguments passed on to \code{plot_multopt}.
#'
#' @export
show_output.data.frame <-
  function(x,
           index,
           type = c("both", "plot", "table"),
           use.regex = TRUE,
           ...)
  {
    if (length(index) == 1L)
      return()
    d <-
      table_multiopt(x,
                     indices = index,
                     data.only = TRUE,
                     use.regex = use.regex)
    plot_multiopt(d, ...)
  }

