


function(input, output, session) {

  
  data <- reactiveValues()
  
  #=============================================================================
  # Load example data
  #=============================================================================
  
  output$data_import <- renderUI({
    if(input$example_data==FALSE){
      fileInput("dataFile",label = NULL,
                buttonLabel = "Browse...",
                placeholder = "No file selected")
    }else {NULL}

  })
  
  #=============================================================================
  # Data & Preview
  #=============================================================================
  mydata <- reactive ({
    if(input$example_data == TRUE){
      
      if(input$example_dataset == "PBMC3K_example_data"){
        data(input$example_dataset)
        df=PBMC3K_example_data
      }else {
        data(input$example_dataset)
        df=CBMC8K_example_data
      }
      
    }else{
      if (is.null(input$dataFile)){
        return(data.frame(x = "Upload data to allow"))
      }else{
        df <- read.csv(input$dataFile$datapath,
                       header = as.logical(input$header),
                       sep = input$sep, dec=input$dec)
      }

    }
    
    data$table=df
    data$colnames=colnames(df)
    df
    
    return(df)
    
  })
  
  output$preview <-  DT::renderDataTable({
    # req(input$dataFile)
    mydata()[1:10,]
  },  options = list(scrollX = TRUE , dom = 't'))
  
  output$preview_table <- renderUI({
    if(!is.null(input$dataFile) | input$example_data==TRUE){
      DT::dataTableOutput(outputId = "preview")
    }
    
  })
  
  output$data_param <- renderUI({
    if(input$example_data==FALSE){
      box(title = "Parameters",
          status = "primary", solidHeader = TRUE, collapsible = TRUE, width=12,
          # Input: Checkbox if file has header
          radioButtons(inputId = "header",
                       label = "Header",
                       choices = c("Yes" = TRUE,
                                   "No" = FALSE),
                       selected = TRUE, inline=T),
          
          # Input: Select separator ----
          radioButtons(inputId = "sep",
                       label = "Column Separator",
                       choices = c(Comma = ",",
                                   Semicolon = ";",
                                   Tab = "\t"),
                       selected = "\t", inline=F),
          # Input: Select decimal ----
          radioButtons(inputId = "dec",
                       label = "Decimal Separator",
                       choices = c(Comma = ",",
                                   Point = "."),
                       selected = ".", inline=T)
      )
    }else{
      selectInput(inputId="example_dataset", label = "Choose example data", 
                  choices=c("PBMC 3k scRNAseq data"="PBMC3K_example_data", "CBMC 8k CITEseq data"="CBMC8K_example_data"), selected=2)
    }

  })
  
  factors=reactive({
    # req(input$dataFile, input$example_data==FALSE)
    sapply(data$table,class)
  })
  
  quant_factor <- reactive ({
    # req(input$dataFile, input$example_data==FALSE)
    names(factors())[factors() %in% c("integer","numeric")]
  })
  
  qual_factor <- reactive ({
    # req(input$dataFile, input$example_data==FALSE)
    names(factors())[!(factors() %in% c("integer","numeric"))]
  })
  

  
  output$qual_fact <- renderText({
    req(input$dataFile, input$example_data==FALSE)
    paste("Columns detected as qualitative factors :", paste(qual_factor(),collapse = "; "))
  })

  output$quant_fact <- renderText({
    req(input$dataFile, input$example_data==FALSE)
    paste("Columns detected as quantitative factors :", paste(quant_factor(),collapse = "; "))
  })
  
  output$preview_title <- renderText({
    if(input$example_data==TRUE | !(is.null(input$dataFile))){
      "File preview (10 first lines)"
    }
  })
  
  
  
  #=============================================================================
  # Numeric to factor conversion
  #=============================================================================
  
  output$numeric <- renderUI({
    # req(input$dataFile, input$example_data==FALSE)
    if(!is.null(input$dataFile) | input$example_data==TRUE){
          checkboxGroupInput("numeric_box", label = "Quantitative factor(s)", 
                       choices = quant_factor(),
                       selected = 0)
    }

  })
  output$numeric_to_factor <- renderUI({
    # req(input$dataFile, input$example_data==FALSE)
    if(!is.null(input$dataFile) | input$example_data==TRUE){
      actionButton("numeric_to_factor", "Convert to qualitative factor")
    }
  })
  
  observeEvent(input$numeric_to_factor, {
    for (colname in input$numeric_box){
      temp=data$table
      temp[,colname]=as.factor(temp[,colname])
     data$table <- temp
    }
  })
  
  
  output$factor <- renderUI({
    # req(input$dataFile, input$example_data==FALSE)
    if(!is.null(input$dataFile) | input$example_data==TRUE){
      checkboxGroupInput("factor_box", label = "Qualitative factor(s)", 
                         choices = qual_factor(),
                         selected = 0)
    }
  })
  output$factor_to_numeric <- renderUI({
    # req(input$dataFile, input$example_data==FALSE)
    if(!is.null(input$dataFile) | input$example_data==TRUE){
      actionButton("factor_to_numeric", "Convert to quantitative factor")
    }
  })
  
  observeEvent(input$factor_to_numeric, {
    for (colname in input$factor_box){
      temp=data$table
      temp[,colname]=as.numeric(as.character((temp[,colname])))
      data$table <- temp
    }
  })
  
  mydata2 <- reactive({
    data$table
  })

  #=============================================================================
  # Lecture
  #=============================================================================
  observeEvent(input$actBtnVisualisation, {
    output$viz_panel=renderMenu({
      sidebarMenu(
        menuItem("Visualize data", tabName = "visualization", icon = icon("poll")),
        br()
        # ,materialSwitch(inputId = "custom_colors", label = "Custom colors ?", status = "success")
        # ,column(8,uiOutput("colors"))
      )
    })
    
    output$col_panel=renderUI({
      if(all(is.na(c(size_var(),col_var(),text_var()))) & input$shape_type==1){
        NULL
      }else {
        sidebarMenu(
          br(),
          column(12,uiOutput("colors"))
          , width=12
        )
      }
    })
    
    updateTabItems(session, "tabs", selected = "visualization")

  })
  
  output$visualization_button<-renderUI({
    if(input$example_data==FALSE & is.null(input$dataFile)){
      
    }else{
      actionButton(inputId = "actBtnVisualisation", label = "Visualization",icon = icon("play"), style='font-size:150%; color:green' )
    }
  }) 
  
  
  #=============================================================================
  # Shape Parameters box
  #=============================================================================
  
  # X & Y axis
  output$x_axis <- renderUI({
    selectInput(inputId="x_axis", label = "x axis", choices=qual_factor(), selected=1)
  })
  
  y_axis_choices <- reactive({

    req(input$x_axis)

    qual_factor()[qual_factor()!=input$x_axis]
  })

  output$y_axis <- renderUI({
    selectInput(inputId="y_axis", label = "y axis", choices=y_axis_choices(), selected=1)
  })
  
  
  # Size parameters
  output$size_var <- renderUI({
    selectInput(inputId="size_var", label = "Column controling shape size", choices=c("Constant size"=NA, quant_factor()), selected=1)
  })
  output$size_scale <- renderUI({
    conditionalPanel(condition = "(input.size_var != 'NA')|(input.col_var != 'NA')|(input.text_var != 'NA')|(input.shape_type!=1)",
                     numericInput("shape.scale", "Size scale", 12)
    )
    # numericInput("shape.scale", "Size scale", 12)
  })
  output$size_leg <- renderUI({
    if(input$size_var!="NA"){
      textInput("size_leg", label = "Size legend title", value = input$size_var)
    }
  })
  output$size_breaks <- renderUI({
    # if(input$size_var!="NA"){
    #   numericInput("size.breaks.number", "Size breaks number", 4)
    # }
    conditionalPanel(condition = "input.size_var != 'NA'",
                     numericInput("size.breaks.number", "Size breaks number", 4)
                     )
  })
  
  # Color parameters
  output$col_var <- renderUI({
    selectInput(inputId="col_var", label = "Column controling shape color", choices=c("Unique color"=NA, data$colnames), selected=1)
  })
  output$col_leg <- renderUI({
    if(input$col_var!="NA"){
      textInput("col_leg", label = "Color legend title", value = input$col_var)
    }
  })
  output$col_breaks <- renderUI({
    # if(input$col_var!="NA" & input$col_var %in% quant_factor()){
    #   numericInput("color.breaks.number", "Color breaks number", 4)
    # }
    conditionalPanel(condition = "input.col_var != 'NA'",
                     numericInput("color.breaks.number", "Color breaks number", 4)
    )
  })
  
  # Color choice
  quant_palette=c("blue","white","red")
  quant_label=c("Min color","Med color","Max color")
  qual_palette= reactive({rainbow(length(unique(mydata()[,which(input$col_var==data$colnames)])))})
  
  output$col_cond=renderUI({
    # if(all(is.na(c(size_var(),col_var(),text_var()))) & input$shape_type==1){
    #   NULL
    # }else {
    #   materialSwitch(inputId = "custom_colors", label = "Custom colors ?", status = "success", value=FALSE)
    # }
    conditionalPanel(condition = "(input.size_var != 'NA')|(input.col_var != 'NA')|(input.text_var != 'NA')|(input.shape_type!=1)",
                     materialSwitch(inputId = "custom_colors", label = "Custom colors ?", status = "success", value=FALSE)
    )
  })
  
  output$colors <- renderUI({
    if(input$custom_colors){
      if(!(input$col_var %in% data$colnames)){
        colourInput("custom_col", "Choose color", "blue")
      }else if(input$col_var %in% quant_factor()){
        fluidRow(
          lapply(1:3, function(i){
            colourInput(paste("custom_col",i,sep=""), quant_label[i],quant_palette[i])
          })
        )
      }else {
        fluidRow(
          lapply(1:length(unique(mydata()[,which(input$col_var==data$colnames)])), function(i){
            colourInput(paste("custom_col",i,sep=""), 
                        label = unique(mydata()[,which(input$col_var==data$colnames)])[i],
                        qual_palette()[i])
          })
        )
      }
    }
  })
  
  cols_to_use=reactive({
    if(all(is.na(c(size_var(),col_var(),text_var()))) & input$shape_type==1){
      "default"
    }else {
      if(input$custom_colors){
        if(!(input$col_var %in% data$colnames)){
          input$custom_col
        }else if(input$col_var %in% quant_factor()){
          c(input$custom_col1,input$custom_col2,input$custom_col3)
        }else {
          cols=c()
          for (i in 1:length(unique(mydata()[,which(input$col_var==data$colnames)]))){
            cols=c(cols,input[[paste("custom_col",i,sep="")]])
          }
          cols
        }
      }else{"default"}
    }
  })
  
  # Shape parameters
  output$shape_type <- renderUI({
    radioButtons("shape_type", label="Shape type", choices=list("Unique"=1,"Controled by a column"=2), selected=1)
  })
  output$shape_var <- renderUI({
    if(input$shape_type==1){
      numericInput("shape_var", "Shape value (pch)", 16)
    }else{
      selectInput(inputId="shape_var", label = "Column controling shape", choices=data$colnames, selected=1)
    }
  })
  output$shape_leg <- renderUI({
    if(input$shape_type==2){
      textInput("shape_leg", label = "Shape legend title", value = input$shape_var)
    }else{
      # selectInput("shape_leg", label = "Shape legend title", choices=c("No shape legend"), selected=1) 
    }
  })
  output$shape_breaks <- renderUI({
    if( (input$shape_type==2) & (input$shape_var %in% quant_factor()) ){
      numericInput("shape.breaks.number", "Shape breaks number", 5)
    }else{
      
    }
  })
  
  # Text parameters
  output$text_var <- renderUI({
    selectInput(inputId="text_var", label = "Column controling text on shape", choices=c("No text"=NA, data$colnames), selected=1)
  })
  output$text_size <- renderUI({
    # if(input$text_var!="NA"){
    #   numericInput("text_size", label = "Text size", value = 3)
    # }
    conditionalPanel(condition = "input.text_var != 'NA'",
                     numericInput("text_size", label = "Text size", value = 3)
    )
  })
  
  #=============================================================================
  # Plot Parameters box
  #=============================================================================
  
  output$display_dist_hclust_method <- reactive({
    return((!is.null(input$x_dend_picker)) | (!is.null(input$y_dend_picker)))
    # return(!is.null(input$x_dend_picker))
    # return("OK")
    # if(is.null(input$x_dend_picker) & is.null(input$y_dend_picker)){
    #   return(FALSE)
    # }else{
    #   return(TRUE)
    # }
  })
  outputOptions(output, "display_dist_hclust_method", suspendWhenHidden = FALSE)
  
  
  output$dist_hclust_method <- renderUI ({
    # if(is.null(input$x_dend_picker) & is.null(input$y_dend_picker)){
    #   
    # }else{
    #   fluidRow(
    #     column(6, pickerInput(inputId = "dist_method", label="Distance method", 
    #                           choices=c("euclidean", "maximum", "manhattan", "canberra","binary", "minkowski")
    #                           , multiple=FALSE)
    #     ),
    #     column(6, pickerInput(inputId = "hclust_method", label="Hclust method", 
    #                           choices= c("ward.D", "single", "complete", "average", "mcquitty", "median", "centroid", "ward.D2")
    #                           , multiple=FALSE)
    #     )
    #   )
    # }
    conditionalPanel(condition = "output.display_dist_hclust_method",
                     fluidRow(
                       column(6, pickerInput(inputId = "dist_method", label="Distance method",
                                             choices=c("euclidean", "maximum", "manhattan", "canberra","binary", "minkowski")
                                             , multiple=FALSE)
                       ),
                       column(6, pickerInput(inputId = "hclust_method", label="Hclust method",
                                             choices= c("ward.D", "single", "complete", "average", "mcquitty", "median", "centroid", "ward.D2")
                                             , multiple=FALSE)
                       )
                     )
    )
    
  })
  
  output$plot_parameters <- renderUI ({
    # if(all(is.na(c(size_var(),col_var(),text_var()))) & input$shape_type==1){
    #   NULL # Display nothing if there is no plot
    # }else{
    #   box(title = "Plot Parameters",
    #       status = "primary", solidHeader = TRUE, collapsible = TRUE, width=12,
    #       fluidRow(
    #         column(4, radioButtons("plot_legend", label="Display legend ?", choices=list("Yes"=TRUE,"No"=FALSE), selected=TRUE)),
    #         column(4, radioButtons("x_lab_rot", label="Rotate x labels ?", choices=list("Yes"=TRUE,"No"=FALSE), selected=FALSE))
    #       ),
    #       fluidRow(
    #         column(3, numericInput("plot_height", "Plot height", value=6)),
    #         column(3, numericInput("plot_width", "Plot width", 6))
    #       ),
    #       fluidRow(
    #         column(6, pickerInput(inputId = "x_dend_picker", label="Column to calculate dendrogramm (x axis)", 
    #                choices=names(factors())[!names(factors()) %in% c(input$x_axis, input$y_axis)],
    #                options=list(`actions-box` = TRUE, `selected-text-format` = "count > 3"), multiple=TRUE)
    #                ),
    #         column(6, pickerInput(inputId = "y_dend_picker", label="Column to calculate dendrogramm (y axis)", 
    #                               choices=names(factors())[!names(factors()) %in% c(input$x_axis, input$y_axis)],
    #                               options=list(`actions-box` = TRUE, `selected-text-format` = "count > 3"), multiple=TRUE)
    #         )
    #       ),
    #       uiOutput("dist_hclust_method")
    #   )
    # }
    
    conditionalPanel(condition = "(input.size_var != 'NA')|(input.col_var != 'NA')|(input.text_var != 'NA')|(input.shape_type!=1)",
                     box(title = "Plot Parameters",
                         status = "primary", solidHeader = TRUE, collapsible = TRUE, width=12,
                         fluidRow(
                           column(4, radioButtons("plot_legend", label="Display legend ?", choices=list("Yes"=TRUE,"No"=FALSE), selected=TRUE)),
                           column(4, radioButtons("x_lab_rot", label="Rotate x labels ?", choices=list("Yes"=TRUE,"No"=FALSE), selected=FALSE)),
                           column(4, selectInput(inputId="x_lab_pos", label = "x label position", 
                                                 choices=c("bottom"="bottom","top"="top","both"="both","none"="none" ), selected=1)),
                           column(4, selectInput(inputId="y_lab_pos", label = "y label position", 
                                                 choices=c("left"="left","right"="right","both"="both","none"="none" ), selected=1))
                         ),
                         fluidRow(
                           column(3, numericInput("plot_height", "Plot height", value=6)),
                           column(3, numericInput("plot_width", "Plot width", 6))
                         ),
                         fluidRow(
                           column(6, pickerInput(inputId = "x_dend_picker", label="Column to calculate dendrogramm (x axis)", 
                                                 choices=names(factors())[!names(factors()) %in% c(input$x_axis, input$y_axis)],
                                                 options=list(`actions-box` = TRUE, `selected-text-format` = "count > 3"), multiple=TRUE)
                           ),
                           column(6, pickerInput(inputId = "y_dend_picker", label="Column to calculate dendrogramm (y axis)", 
                                                 choices=names(factors())[!names(factors()) %in% c(input$x_axis, input$y_axis)],
                                                 options=list(`actions-box` = TRUE, `selected-text-format` = "count > 3"), multiple=TRUE)
                           )
                         ),
                         uiOutput("dist_hclust_method")
                     )
    )

  })
  
  
  
  #=============================================================================
  # Download parameters + output
  #=============================================================================
  
  # Download format
  output$down_format <- renderUI({
    # if(all(is.na(c(size_var(),col_var(),text_var()))) & input$shape_type==1){
    #   NULL
    # }else {selectInput(inputId="down_format", label = "File format", choices=c("png"="png", "tiff"="tiff", "eps"="eps", "pdf"="pdf"), selected=1)}
    conditionalPanel(condition = "(input.size_var != 'NA')|(input.col_var != 'NA')|(input.text_var != 'NA')|(input.shape_type!=1)",
                     selectInput(inputId="down_format", label = "File format", choices=c("png"="png", "tiff"="tiff", "eps"="eps", "pdf"="pdf"), selected=1)
    )
  })
  
  #Download bouton
  output$down_bouton <- renderUI({
    if(all(is.na(c(size_var(),col_var(),text_var()))) & input$shape_type==1){
      NULL
    }else {
      downloadButton("downloadData", "Download")
    }
  })
  
  # Download plot
  output$downloadData <- downloadHandler(
    filename = function(format=input$down_format) {
      fn_ext<-switch(format,
                     png = '.png',
                     tiff = '.tiff',
                     eps = '.eps',
                     pdf = '.pdf'
      )
      paste(Sys.Date(),"my_plot",fn_ext, sep = "")
    },
    content = function(file, format=input$down_format) {
      if(input$down_format=="pdf"){
        require(grDevices)
        cairo_pdf(file, width = input$plot_width*100/72, height=input$plot_height*100/72, family="Lucida Console")
        plot(myplot)
        dev.off()
      }else{
        ggsave(myplot, filename = file, device=input$down_format, width = input$plot_width*100/72, height=input$plot_height*100/72,units = "in",dpi=72)
      }
    }
  )
  
  #=============================================================================
  # Warnings
  #=============================================================================
  
  output$shape_warn <- renderText({
    if(input$shape_var %in% qual_factor()){
      if (length(unique(mydata2()[,input$shape_var]))>13){
        "WARNING : The shiny app does not support more than 13 different discrete shapes"
      } else {NULL}
    }else{NULL}
  })
  
  output$xy_warn <- renderText({
    if(max(table(data$table[,input$x_axis],data$table[,input$y_axis])) > 1){
      "WARNING : Several lines have the same x and y values ! Shapes will overlap."
    }else {NULL}
  })

  
  #=============================================================================
  # Plot
  #=============================================================================
  
  col_var <- reactive({
    if(input$col_var=="NA"){NA}else{input$col_var}
  })
  col_breaks <- reactive({
    if(input$col_var!="NA" & input$col_var %in% quant_factor()){
      input$color.breaks.number
    }else{
      4
    }
  })
  shape_breaks <- reactive({
    if( (input$shape_type==2) & (input$shape_var %in% quant_factor())  ){
      input$shape.breaks.number
    }else{
      5
    }
  })
  
  text_var <- reactive({
    if(input$text_var=="NA"){NA}else{input$text_var}
  })
  text_size <- reactive({
    if(input$text_var=="NA"){NA}else{input$text_size}
  })
  size_var <- reactive({
    if(input$size_var=="NA"){NA}else{input$size_var}
  })
  size_breaks <- reactive({
    if(input$size_var=="NA"){5}else{input$size.breaks.number}
  })
  plot_legend <- reactive({
    if(input$plot_legend=="TRUE"){TRUE}else{FALSE}
  })
  x_lab_rot <- reactive({
    if(input$x_lab_rot=="TRUE"){TRUE}else{FALSE}
  })
  
  shape_var <- reactive({
    if(is.numeric(input$shape_var)){
      input$shape_var
    }else{
      paste('"',input$shape_var,'"',sep="")
    }
  })
  
  shape_legend <- reactive({
    if(input$shape_type==2){input$shape_leg}else{""}
  })
  
  observe({
    output$output_plot <- renderPlot({
      if(!is.null(data$table)){
        data.to.plot=mydata2()[,c(input$x_axis, input$y_axis, data$colnames)]
        
        # Factor level = apparition order
        data.to.plot[,1]<- factor(data.to.plot[,1], levels = unique(data.to.plot[,1]))
        data.to.plot[,2]<- factor(data.to.plot[,2], levels = unique(data.to.plot[,2]))
        
        data.to.plot <<- data.to.plot # Saving data in environment
        
        myplot <<- dot_plot(data.to.plot,
                          size_var=size_var(),col_var=col_var(), text_var=text_var(),shape_var=input$shape_var,
                          size_legend=input$size_leg, col_legend=input$col_leg, shape_legend=input$shape_leg,
                          cols.use = cols_to_use(), shape.scale = input$shape.scale, text.size=text_size(),
                          scale.by = "radius", scale.min = NA, scale.max = NA, plot.legend = plot_legend(), do.return = TRUE,
                          x.lab.rot = x_lab_rot(), horizontal_coloring=NA,
                          size.breaks.number=size_breaks(), color.breaks.number=col_breaks(), transpose=FALSE,
                          dend_x_var = input$x_dend_picker, dend_y_var = input$y_dend_picker,
                          dist_method = input$dist_method, hclust_method = input$hclust_method,
                          x.lab.pos=input$x_lab_pos, y.lab.pos=input$y_lab_pos, shape.breaks.number = shape_breaks()
        )
        print(myplot)
        
      }else {
        NULL
      }
    }, height = ifelse(length(input$plot_height)==0,600,input$plot_height*100), 
    width = ifelse(length(input$plot_width)==0,600,input$plot_width*100))
  })

  #=============================================================================
  # Display executed command
  #=============================================================================
  
  dend_x_print <- reactive({
    if (!is.null(input$x_dend_picker)){
      paste(', dend_x_var=' ,'c("',paste(input$x_dend_picker,collapse = '","'),'")',sep="")
    }
  })
  
  dend_y_print <- reactive({
    if (!is.null(input$y_dend_picker)){
      paste(', dend_y_var=', 'c("',paste(input$y_dend_picker,collapse = '","'),'")',sep="")
    }
  })
  
  dist_meth_print <- reactive ({
    if(!(is.null(input$x_dend_picker) & is.null(input$y_dend_picker))){
      paste(', dist_method="',input$dist_method,'"',sep="")
    }
  })
  
  hclust_meth_print <- reactive ({
    if(!(is.null(input$x_dend_picker) & is.null(input$y_dend_picker))){
      paste(', hclust_method="',input$hclust_method,'"',sep="")
    }
  })
  
  output$executed_code=renderText({
    if(all(is.na(c(size_var(),col_var(),text_var()))) & input$shape_type==1){
      NULL # Display nothing if there is no plot
    }else{
      gsub('"NA"','NA',paste('dot_plot( \n data.to.plot=input_data[,c("',input$x_axis,'","',input$y_axis,'", colnames(input_data)[!(colnames(input_data) %in% c("',input$x_axis,'","',input$y_axis,'"))])]',
                             ', \n size_var="',size_var(),'",col_var="',col_var(),'", text_var="',text_var(),'",shape_var=',shape_var(),
                             ', \n x.lab.pos="',input$x_lab_pos,'",y.lab.pos="',input$y_lab_pos,'"',
                             ', \n cols.use=',paste('c("',paste(cols_to_use(),collapse = '","'),'")',sep=""),',size_legend="',input$size_leg,'", col_legend="',input$col_leg,'", shape_legend="',shape_legend(),
                             '", \n shape.scale =',input$shape.scale,', text.size=',text_size(),
                             ', \n plot.legend = ',plot_legend(),',x.lab.rot = ',x_lab_rot(),
                             ', \n size.breaks.number=',size_breaks(),', color.breaks.number=',col_breaks(),', shape.breaks.number=',shape_breaks(),
                             '\n',dend_x_print(), dend_y_print(),
                             dist_meth_print(), hclust_meth_print(),
                             ")"
                             ,sep=""))
      # Default arguments
      # col.min = -2.5, col.max = 2.5,scale.by = "radius", scale.min = NA, scale.max = NA,vertical_coloring=c(NA,"gray85"), horizontal_coloring=NA, do.return = FALSE,transpose=FALSE
      
    }
 })
  
  output$output_code=renderUI({
    if(all(is.na(c(size_var(),col_var(),text_var()))) & input$shape_type==1){
      NULL # Display nothing if there is no plot
    }else{
      # box(title = "Corresponding code",
      #     status = "primary", solidHeader = TRUE, collapsible = TRUE, width=12,
      #     verbatimTextOutput("executed_code")
      # )
      fluidRow(
        h3("Corresponding code"),
        column(12, verbatimTextOutput("executed_code"))
      )
    }
  })
  

  
}