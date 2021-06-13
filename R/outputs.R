

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




#' @param x An object of class \code{data.frame} that has the data used in the
#' analysis
#' @param index Integer vector representing indices for the column(s) of
#' interest
#' @param type Either "table" or "plot".
#'
#' @export
show_output.data.frame <-
  function(x,
           index,
           type = c("both", "plot", "table"),
           ...)
  {
    if (length(index) == 1L)
      return()
    d <-
      table_multiopt(x, indices = index, data.only = TRUE, use.regex = FALSE)
    plot_multiopt(d)
  }
