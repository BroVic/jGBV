# Source file: objects.R
#
# MIT License
#
# Copyright (c) 2019 Victor Ordu

#' Builtin Objects For JHPIEGO GBV Projects
#'
#' @rdname jgbv-objects
#'
#' @export
raampStates <- c("Abia", "Akwa Ibom", "Ogun", "Bauchi")



#' @rdname jgbv-objects
#'
#' @export
tool.sectors <-
  structure(
    sectors <- c(
      "Health",
      "Judicial",
      "Legal",
      "Referral",
      "Security",
      "Social",
      "Temporary"
    ),
    names = tolower(sectors),
    class = "Tools"
  )




#' Print Out The Tools Object
#'
#' Print the \code{tool.sectors} object in an easy to read format.
#'
#' @param x A \code{Tools} object
#' @param ... Additional arguments passed on to \code{print.default}
#'
#' @export
print.Tools <- function(x, ...) {
  print(unclass(unname(x)), ...)
}
