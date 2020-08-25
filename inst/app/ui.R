
function() {
  
  dashboardPage(
    dashboardHeader(title = "Dot_plot"),
    dashboardSidebar(
      tags$head(
        tags$style(HTML("
                        .sidebar { height: 90vh; overflow-y: auto; }
                        " )
        )
        ),
      sidebarMenu(id = "tabs",
                  menuItem("App description", tabName="AppDes", icon = icon("book-open")),
                  menuItem("Import Data", tabName = "readData", icon = icon("arrow-alt-circle-down")),
                  menuItemOutput("viz_panel"),
                  uiOutput("col_panel")
      )
    ),
    dashboardBody(
      use_bs_tooltip(),
      
      tabItems(
        
        #Desc panel
        tabItem(tabName="AppDes",
                h3("Welcome to the FlexDotPlot Shiny App"),
                h5("Use the 'Import Data' panel (at the left) to :"),
                tags$ul(
                  tags$li("Import your dataset or select an example dataset"),
                  tags$li("Choose import parameters"),
                  tags$li("Control/change column types (quantitative or qualitative")
                ),
                h5("Then, you can press the 'Visualization' button to go to the 'Visualize data' panel. 
                   This panel allows you to set all the plot parameters  and visualize the resulting dotplot.
                   Once a dotplot is generated, a new box with export parameters will appear between the parameters boxes and the plot")
                
        ),
        
        # Read data
        tabItem(tabName = "readData",
                h1("Import Data"),
                
                fluidRow(
                  column(12,
                         radioButtons(inputId = "example_data", 
                                      label = "",
                                      choices = c("Load a personnal dataset" = FALSE,
                                                  "Load example dataset" = TRUE),
                                      selected = FALSE, inline=T),
                         uiOutput("data_import")
                         )

                ),

                
                fluidRow(
                  # column(4,
                  #        conditionalPanel(
                  #          condition="input.example_data == 'FALSE'",
                  #          box(title = "Parameters",
                  #              status = "primary", solidHeader = TRUE, collapsible = TRUE, width=12,
                  #              # Input: Checkbox if file has header
                  #              radioButtons(inputId = "header", 
                  #                           label = "Header",
                  #                           choices = c("Yes" = TRUE,
                  #                                       "No" = FALSE),
                  #                           selected = TRUE, inline=T),
                  #              
                  #              # Input: Select separator ----
                  #              radioButtons(inputId = "sep", 
                  #                           label = "Column Separator",
                  #                           choices = c(Comma = ",",
                  #                                       Semicolon = ";",
                  #                                       Tab = "\t"),
                  #                           selected = "\t", inline=F),
                  #              # Input: Select decimal ----
                  #              radioButtons(inputId = "dec", 
                  #                           label = "Decimal Separator",
                  #                           choices = c(Comma = ",",
                  #                                       Point = "."),
                  #                           selected = ".", inline=T)
                  #          )
                  #        )
                  # 
                  # ),
                  
                  column(4, uiOutput("data_param")),

                  column(8,
                         h3(uiOutput("preview_title")),
                         uiOutput("preview_table"),
                         tags$br(),
                         # verbatimTextOutput("quant_fact"),
                         # verbatimTextOutput("qual_fact"),
                         column(6,uiOutput("numeric"),
                                uiOutput("numeric_to_factor")),
                         column(6,uiOutput("factor"),
                                uiOutput("factor_to_numeric"))

                  )
                ), 
                tags$br(),
                
                div(uiOutput("visualization_button"), align = "center")
    
        ),
        
        # visualization
        tabItem(tabName = "visualization",
                fluidRow(
                  box(title = "Shape Parameters",
                      status = "primary", solidHeader = TRUE, collapsible = TRUE, width=12,
                      fluidRow(
                        column(4, uiOutput("x_axis")),
                        column(4, uiOutput("y_axis"))
                      ),
                      fluidRow(
                        column(12, span(style = "color:red;", textOutput("xy_warn")))
                      ),
                      fluidRow(
                        column(4, uiOutput("size_var")),
                        column(2, uiOutput("size_scale")),
                        column(3, uiOutput("size_leg")),
                        column(3, uiOutput("size_breaks"))
                      ),
                      fluidRow(
                        column(4, uiOutput("col_var")),
                        column(3, uiOutput("col_leg")),
                        column(3, uiOutput("col_breaks"))
                      ),
                      fluidRow(
                        column(6, uiOutput("col_cond"))
                      ),
                      fluidRow(
                        column(3, uiOutput("shape_type")),
                        column(3, uiOutput("shape_var")),
                        column(3, uiOutput("shape_leg")),
                        column(3, uiOutput("shape_breaks"))
                      ),

                      fluidRow(
                        column(4, uiOutput("text_var")),
                        column(3, uiOutput("text_size"))
                      )
                      
                  )
                ),
                
                fluidRow(
                  uiOutput("plot_parameters")
                ),
                
                column(12, conditionalPanel(condition="input.generate_plot_button",checkboxInput("auto_refresh", label = "Autorefesh dotplot")), align="center"),
                column(12, uiOutput("refresh_button"), align="center"),
                
                
                # h2("Plot Output"),
                h2(textOutput("plot_output_title")),
                
                fluidRow(
                  column(12, plotOutput("output_plot",inline = TRUE))
                ),
                tags$br(),
                fluidRow(
                  column(4, uiOutput("down_format"))
                ),
                fluidRow(
                  column(4, uiOutput("down_bouton"))
                ),
                tags$br(),
                uiOutput("output_code")

        )
      )
    )
  )
  
}


