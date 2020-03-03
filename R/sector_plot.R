# Source file: sector_plot.R
#
# MIT License
#
# Copyright (c) 2019 Victor Ordu

#' Plot the Services for a Single Sector Selected
#'
#' @param data The data frame
#'
#' @import ggplot2
#' @importFrom magrittr %>%
#'
#' @export
single_sector_services_plot <- function(data) {
  data %>%
     ggplot(aes(name, fill = name)) +
     geom_bar()
}
