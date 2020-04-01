#' Shiny dotplot
#' 
#' Shiny application to perform dot-plot pacman-plot
#' 
#' @examples Shiny_dot_plot()
#' @author Simon Leonard - simon_leonard[a]hotmail.fr

Shiny_dot_plot <- function (RStudio = F) 
{
  Sys.setenv(R_MAX_NUM_DLLS = 180)
  cat("Launching Shniy app...")
  if (RStudio) {
    options(shiny.launch.browser = .rs.invokeShinyWindowViewer)
  }
  shiny::runApp(system.file("app", package = "FlexDotPlot"))
}
