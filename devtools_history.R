usethis::use_build_ignore("devtools_history.R")

#dot_plot function
usethis::use_package("FactoMineR")
usethis::use_package("ggdendro")
usethis::use_package("ggplot2")
usethis::use_package("grDevices")
usethis::use_package("grid")
usethis::use_package("gridExtra")
usethis::use_package("grImport2")
usethis::use_package("reshape2")
usethis::use_package("scales")
usethis::use_package("sisal")
usethis::use_package("stats")

# rotate_dot_plot_dendrogram function
usethis::use_package("dendextend")

#Shiny_dot_plot function
usethis::use_package("shiny")
usethis::use_package("shinydashboard")
usethis::use_package("shinyWidgets")
usethis::use_package("ggforce")
usethis::use_package("colourpicker")
usethis::use_package("htmltools")
usethis::use_package("bsplus")
usethis::use_package("DT")


#Suggested packages
usethis::use_package("knitr", type = "Suggests")
usethis::use_package("markdown", type = "Suggests")
usethis::use_package("rmarkdown", type = "Suggests")
usethis::use_package("Seurat", type = "Suggests")
usethis::use_package("infercnv", type = "Suggests")
usethis::use_package("SeuratData", type = "Suggests")
usethis::use_package("plyr", type = "Suggests")
usethis::use_package("biomaRt", type = "Suggests")
usethis::use_package("data.table", type = "Suggests")





devtools::build_vignettes()
# pkgdown::build_site()
# devtools::check_rhub(env_vars=c(R_COMPILE_AND_INSTALL_PACKAGES = "always"))

# converts roxygen comments to .Rd files.
# roxygen2::roxygenise() 
