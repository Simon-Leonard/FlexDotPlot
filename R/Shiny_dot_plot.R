#' Shiny dotplot
#'
#' Shiny application to perform dot-plot pacman-plot
#'
#' @importFrom shiny runApp
#' @import shinyWidgets
#' @import shinydashboard
#' @import DT
#' @import bsplus
#' @import colourpicker
#' @import ggforce
#' @import htmltools
#' @import magrittr
#' @examples if(interactive()) Shiny_dot_plot()
#' @export
#' @author Simon Leonard - simon.leonard@univ-rennes1.fr

Shiny_dot_plot <- function ()
{
  Sys.setenv(R_MAX_NUM_DLLS = 180)
  cat("Launching Shiny app...")
  # if (RStudio) {
  #   options(shiny.launch.browser = .rs.invokeShinyWindowViewer)
  # }
  runApp(system.file("app", package = "FlexDotPlot"))
}
