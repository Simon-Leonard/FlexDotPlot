#' Dot-plot - Pacman-plot function
#' 
#' Create dotplots to represent two discrete factors (x & y) described by several other factors. Each combination of the two discrete factors (x & y) can be described with : 1 continuous factor (setting shape size), 3 continuous or discrete factors (setting shape type, shape color and text on shape).
#'
#' @encoding UTF-8
#' @param data.to.plot Input data. Can be a list or a data.frame.
#'   If data.frame : Column 1 = x axis (Factor); Col2= y axis (Factor).
#'   If list : x and y axis are fixed by row and col names of list elements.
#' @param size_var
#'   If numeric : Column/List index which control shape sizes. This column/element has to be numeric.
#'   Can also be a column/element name or a vector of the same size than the input dataset.
#'   Set to NA if you don't want to control shape size.
#' @param col_var
#'   If numeric : Column/List index which control shape colors. 
#'   Can also be a column/element name or a vector of the same size than the input dataset.
#'   Set to NA if you don't want to control shape color.
#' @param text_var 
#'   If numeric : Column/List index which control text to add on shapes. 
#'   Can also be a column/element name or a vector of the same size than the input dataset.
#'   Set to NA if you don't want to add text.
#' @param size_legend Custom name of shape legend.
#' @param col_legend Custom name of shape color.
#' @param cols.use 1 color or a vector containing multiple colors to color shapes.
#'   If coloring is continuous, default colors are taken from a "lightgrey" to "blue" gradient.
#'   If coloring is discrete, default colors are taken from the default ggplot2 palette.
#' @param shape.scale Scale the size of the shapes, similar to cex.
#' @param display_max_sizes Boolean : Display max shape size behind each shape ? (Default=TRUE)
#' @param scale.by Scale the size by size or radius.
#' @param scale.min/scale.max Set lower and upper limits for scaling, use NA for default values.
#' @param plot.legend Plot the legends ?
#' @param do.return Return ggplot2 object ?
#' @param x.lab.pos Where to display x axis labels. This must be one of "bottom","top","both" or "none".
#' @param y.lab.pos Where to display y axis labels. This must be one of "left","right","both"or "none".
#' @param x.lab.rot Rotate x-axis labels ?
#' @param shape_var If numeric = Similar to pch : square=15; circle=16; triangle=17. Can also be a column/element name or a vector of the same size than the input dataset.
#' @param shape_use Shapes to uses (only when shape is controled by a discrete factor). Default shapes : \\u25A0 \\u25CF \\u25C6 \\u2BC8 \\u2BC7 \\u2BC6 \\u2BC5 \\u25D8 \\u25D9 \\u2726  \\u2605 \\u2736 \\u2737.
#' @param shape_legend Name of the shape legend if shape_var is a vector.
#' @param text.size Size of text to display on the shapes.
#' @param vertical_coloring Which color use to color the plot vertically ? (colors are repeated untill the end of the plot). Setting vertical and horizontal coloring at the same time is not recommended !
#' @param horizontal_coloring Which color use to color the plot horizontally ? (colors are repeated untill the end of the plot). Setting vertical and horizontal coloring at the same time is not recommended !
#' @param size.breaks.number Number of shapes with different size to display in the legend. Not used if size.breaks.values is not NA.
#' @param size.breaks.values Vector containing numerical labels for the size legend.
#' @param color.breaks.number Number of labels for the color gradient legend. Not used if color.breaks.values is not NA.
#' @param color.breaks.values Vector containing numerical labels for continuous color legend.
#' @param shape.breaks.number Number of shapes to display in the legend. Used when shape is controled by a continuous factor only. Not used if shape.breaks.values is not NA.
#' @param shape.breaks.values Vector containing numerical labels for continuous shape legend.
#' @param transpose Reverse x axis and y axis ?
#' @param dend_x_var A vector containing Column/List indexes or Column/List names to compute the x axis dendrogramm.
#' @param dend_y_var A vector containing Column/List indexes or Column/List names to compute the y axis dendrogramm.
#' @param dist_method The distance measure to be used. This must be one of "euclidean", "maximum", "manhattan", "canberra", "binary" or "minkowski".
#' @param hclust_method The agglomeration method to be used. This must be one of "single", "complete", "average", "mcquitty", "ward.D", "ward.D2", "centroid" or "median".
#' 
#' @return Print the plot and return the ggplot object if do.return=TRUE
#' @author Simon Leonard - simon_leonard[a]hotmail.fr
dot_plot <- function(data.to.plot, size_var=NA,col_var=NA, text_var=NA, shape_var=16,
                        size_legend="", col_legend="", shape_legend="",
                        cols.use = "default", shape.scale = 12, text.size=NA,  shape_use="default",
                        scale.by = "radius", scale.min = NA, scale.max = NA, plot.legend = TRUE, do.return = FALSE, 
                        x.lab.rot = TRUE, x.lab.pos=c("both","top","bottom","none"), y.lab.pos=c("left","right","both","none"),
                        vertical_coloring=NA, horizontal_coloring=NA, 
                        size.breaks.number=4, color.breaks.number=5, shape.breaks.number=5,
                        size.breaks.values=NA, color.breaks.values=NA, shape.breaks.values=NA,
                        display_max_sizes=TRUE,
                        transpose=FALSE,
                        dend_x_var=NULL, dend_y_var=NULL, 
                        dist_method=c("euclidean", "maximum", "manhattan", "canberra","binary", "minkowski"),
                        hclust_method=c("ward.D", "single", "complete", "average", "mcquitty", "median", "centroid", "ward.D2")
                     ){
  
  x.lab.pos=match.arg(x.lab.pos)
  y.lab.pos=match.arg(y.lab.pos)
  
  require(ggplot2)
  # If cowplot library is loaded, we have to disable the cowplot default theme and set the ggplot2 default theme
  if("cowplot" %in% (.packages())){theme_set(theme_gray())}
  
  no_color_legend=FALSE # Is TRUE if col_var=NA => one legend to not plot
  no_size_legend=FALSE # Is TRUE if size_var=NA => one legend to not plot
  
  if (!(class(data.to.plot) %in% c("data.frame","list"))){stop("data.to.plot argument has to be a list or a data.frame")}
  if (all(is.na(c(size_var,col_var,text_var)))){
    if(((length(shape_var)!=nrow(data.to.plot)) )){
      if (length(shape_var)==1 & all(is.numeric(shape_var))){
        stop("Nothing to plot. Modify at least one of the following arguments : Shape size, shape color, text on shape, shape type")
      }
    }
  }
  
  
  ### 1: Data formatting ----
  ### 1.1: List to data.frame conversion ----
  if (class(data.to.plot)=="list"){
    #Input=list -> Conversion to data.frame
    require(reshape2)
    
    # Check - All the list elements are data.frame
    if (any(lapply(data.to.plot, class)!="data.frame")){stop("Provide a data.frame in each list element")}
    
    # Check - All the list elements have the same col names and row names (it means they all have the same dimension too)
    col_names=lapply(data.to.plot,colnames)
    if (all(sapply(col_names, identical, col_names[[1]]))==FALSE){stop("Provide data.frames with the same column names")}
    row_names=lapply(data.to.plot,row.names)
    if (all(sapply(row_names, identical, row_names[[1]]))==FALSE){stop("Provide data.frames with the same row names")}
    
    # Giving names to unnamed elements
    names(data.to.plot)=ifelse(names(data.to.plot)!="", names(data.to.plot), paste("Unnamed_column",1:length(data.to.plot), sep="_"))
    
    # d : Col 1 = row names; Col 2 = col names
    d=data.frame(row_names=rownames(data.to.plot[[1]])[row(data.to.plot[[1]])], col_names=colnames(data.to.plot[[1]])[col(data.to.plot[[1]])])
    # d2 = all the list elements converted in columns
    d2=do.call(data.frame,lapply(lapply(data.to.plot, c),unlist))
    
    data.to.plot=data.frame(d,d2)
    row.names(data.to.plot)=paste(data.to.plot$row_names,data.to.plot$col_names,sep="_")
    
  }
  
  ### 1.2: Determine/check parameters to plot ----
  #Input = dataframe
  if (ncol(data.to.plot)<3){
    stop("Provide a data.frame/list with at least 3 columns/elements")
  } 
  
  save.data=data.to.plot
  data.to.plot=data.to.plot[,1:2]
  
  cat("Using : ")
  
  if(!length(size_var) %in% c(1,nrow(data.to.plot))){
    stop(paste("Length of size_var argument has to be equal to 1 or ",nrow(data.to.plot)," (the input dataset size)", sep=""))
  }
  if (length(size_var)==1){
    if (!is.na(size_var)){
      if (size_var %in% colnames(save.data)){
        if (is.numeric(save.data[,size_var])){
          cat(paste("\n -",size_var,"values to set shape size"))
          data.to.plot[,3]=save.data[,size_var]
          size_legend=ifelse(size_legend=="",size_var,size_legend)
        }else {stop(paste("size_var column (",size_var,") has to be numeric"))}
      } else if (is.numeric(size_var) & size_var %in% (1:ncol(save.data))){
        if (is.numeric(save.data[,size_var])){
          cat(paste("\n -",colnames(save.data)[size_var],"values to set shape size"))
          data.to.plot[,3]=save.data[,size_var]
          size_legend=ifelse(size_legend=="",colnames(save.data)[size_var], size_legend)
        }else {stop(paste("size_var column (",size_var,") has to be numeric"))}
      } else {stop("Size_var argument does not correspond to an element/column number or an element/column name")}
    }else {
      data.to.plot[,3]=1
      no_size_legend=TRUE
      cat(paste("\n - Nothing to set shape size"))
    }
  }else {
    if (is.numeric(size_var)){
      data.to.plot[,3]=size_var
      size_legend=size_legend
    }else{stop("Size var vector has to be numeric")}
  }
  
  if(!length(col_var) %in% c(1,nrow(data.to.plot))){
    stop(paste("Length of col_var argument has to be equal to 1 or ",nrow(data.to.plot)," (the input dataset size)", sep=""))
  }
  if (length(col_var)==1){
    if (!is.na(col_var)){
      if (col_var %in% colnames(save.data)){
        cat(paste("\n -",col_var,"values to set shape color"))
        data.to.plot[,4]=save.data[,col_var]
        col_legend=ifelse(col_legend=="",col_var,col_legend)
      } else if (is.numeric(col_var) & col_var %in% (1:ncol(save.data))){
        cat(paste("\n -",colnames(save.data)[col_var],"values to set shape color"))
        data.to.plot[,4]=save.data[,col_var]
        col_legend=ifelse(col_legend=="",colnames(save.data)[col_var], col_legend)
      } else {stop("Col_var argument does not correspond to an element/column number or an element/column name")}
    }else {
      data.to.plot[,4]="no_color"
      no_color_legend=TRUE
      cat(paste("\n - Nothing to set shape color"))
    }
  }else {
    data.to.plot[,4]=col_var
    col_legend=col_legend
  }
  
  if (length(cols.use)==1){
    if (cols.use=="default" & is.numeric(data.to.plot[,4])){
      cols.use=c("lightgrey", "blue")
    }
  }
  
  if(!length(text_var) %in% c(1,nrow(data.to.plot))){
    stop(paste("Length of text_var argument has to be equal to 1 or ",nrow(data.to.plot)," (the input dataset size)", sep=""))
  }
  if (length(text_var)==1){
    if (!is.na(text_var)){
      if (text_var %in% colnames(save.data)){
        cat(paste("\n -",text_var,"values to add text on shapes"))
        data.to.plot[,5]=save.data[,text_var]
      } else if (is.numeric(text_var) & text_var %in% (1:ncol(save.data))){
        cat(paste("\n -",colnames(save.data)[text_var],"values to add text on shapes"))
        data.to.plot[,5]=save.data[,text_var]
      } else {stop("Text_var argument does not correspond to an element/column number or an element/column name")}
    }else {
      data.to.plot[,5]=""
      cat(paste("\n - Nothing to add text on shapes"))
    }
  }else {
    data.to.plot[,5]=text_var
  }
  
  if(!length(shape_var) %in% c(1,nrow(data.to.plot))) {stop(paste("Length of shape_var argument has to be equal to 1 or ",nrow(data.to.plot)," (the input dataset size)", sep=""))}
  if(length(shape_var)==1){
    if (shape_var %in% colnames(save.data)){
      cat(paste("\n - ", shape_var," values to determine shapes (",
                length(unique(save.data[,shape_var]))," shapes detected)", sep=""))
      shape=save.data[,shape_var]
      shape_legend=ifelse(shape_legend=="", 
                          shape_var,
                          shape_legend)
    }else if (is.numeric(shape_var)){
      shape=shape_var
    }else{stop("The shape_var argument is not numeric and does not correspond to a column name/list element name")
    }
  }else{shape=shape_var}
  
  data.to.plot[,1]=factor(data.to.plot[,1], levels=unique(data.to.plot[,1]))
  data.to.plot[,2]=factor(data.to.plot[,2], levels=unique(data.to.plot[,2]))
  data.to.plot[,5]=as.character(data.to.plot[,5])
  
  if (transpose){
    data.to.plot[,1:2]=data.to.plot[,2:1]
    save.data[,1:2]=save.data[,2:1]
  }
  
  ### 2 Calculate dendrograms ----
  check_FAMD_var=function(FAMD_var,type=c("x","y"), save.data){
    out=NULL
    if(!(is.vector(FAMD_var))){
      # Input is not a vector
      out$message=paste("FAMD_",type,"_var arguement has to be a vector",sep="")
      out$success=FALSE
    }else{
      if(is.vector(FAMD_var)){
        if(!is.numeric(FAMD_var)){
          if(all(FAMD_var %in% colnames(save.data))){
            FAMD_var=which(colnames(save.data) %in% FAMD_var)
          }else{          
            out$message=paste("FAMD_",type,"_var names are not in element/column names",sep="")
            out$success=FALSE}
        }
        if(is.numeric(FAMD_var)){
          if(all(FAMD_var %in% 1:ncol(save.data))){
            if(2 %in% FAMD_var){}else{
              print(paste("In FAMD_",type,"_var : Adding y index",sep=""))
              FAMD_var=c(2,FAMD_var)
            }
            if(1 %in% FAMD_var){}else{
              print(paste("In FAMD_",type,"_var : Adding x index",sep=""))
              FAMD_var=c(1,FAMD_var)
            }
            
            out$famd_input=save.data[,FAMD_var]
            out$success=TRUE
          }else{
            out$message=paste("FAMD_",type,"_var indexes are not in element/column indexes",sep="")
            out$success=FALSE
          }
        }
      }
    }
    return(out)
  }
  
  format_FAMD_input=function(FAMD_input, type=c("x","y"), save.data){
    x_name=colnames(save.data)[1]
    y_name=colnames(save.data)[2]
    other_names=colnames(FAMD_input)[!colnames(FAMD_input) %in% c(x_name,y_name)]
    
    list=lapply(other_names, function(x){
      data=data.frame(reshape2::dcast(FAMD_input, list(x_name,y_name), value.var=x))
      rownames(data)=data[,x_name]; data[,x_name]=NULL
      data[sapply(data, is.character)] <- lapply(data[sapply(data, is.character)], as.factor)
      
      # return(data)
      if(type=="y"){
        return(data.frame(t(data)))
      }else{return(data)}
      
    })
    
    FAMD_finalinput=do.call(cbind,list)
    return(FAMD_finalinput)
  }
  
  run_FAMD_and_hclust=function(FAMD_final_input, metric, method){
    require(FactoMineR)
    # require(flashClust)
    
    #Perform FAMD or PCA or MCA
    if( all(lapply(FAMD_final_input,class) %in% c("numeric","integer")) ){
      # Quantitate factors only -> PCA
      res.A=PCA(FAMD_final_input, graph=F, ncp=min(dim(FAMD_final_input))-1)
    } else if ( all(!lapply(FAMD_final_input,class) %in% c("numeric","integer")) ){
      # Qualitative factors only -> MCA
      res.A=MCA(FAMD_final_input, graph=F, ncp=min(dim(FAMD_final_input))-1)
    } else {
      # Mixed data -> FAMD
      res.A=FAMD(FAMD_final_input, graph=F, ncp=min(dim(FAMD_final_input))-1)
    }
    
    # Get coordinates, calculate distance and perform hierarchical clustering
    # Took from the HCPC function of FactoMineR package
    X = as.data.frame(res.A$ind$coord)
    
    do <- dist(X, method = metric)^2
    weight = rep(1, nrow(X))
    eff <- outer(weight, weight, FUN = function(x, y, n) {
      x * y/n/(x + y)
    }, n = sum(weight))
    dissi <- do * eff[lower.tri(eff)]
    
    hc <- hclust(dissi, method = method, members = weight)
    
    return(hc)
  }
  
  dist_method=match.arg(dist_method) 
  hclust_method=match.arg(hclust_method)
  
  require(gtable)
  require(gridExtra)
  require(grid)
  require(FactoMineR)
  
  if(!is.null(dend_x_var)){
    # x dendrogramm
    
    # For the PCA/MCA/FAMD, we change the factor names in factor_1, factor2 ... to avoid syntax problem
    if(class(save.data[,1])!="factor"){save.data[,1]=as.factor(save.data[,1])}
    old_levels=levels(save.data[,1])
    save.data[,1]=paste("factor", as.numeric(save.data[,1]), sep="_")
    
    check_x=check_FAMD_var(dend_x_var, type="x",save.data=save.data)
    if (check_x$success){
      
      FAMD_x_input=format_FAMD_input(check_x$famd_input, type="x", save.data=save.data)
      hc_x_result=run_FAMD_and_hclust(FAMD_x_input, metric=dist_method, method=hclust_method)
      
    }else{
      #check_x$success==FALSE
      stop(check_x$message)
    }
    
    # Reorder x axis according to dendrogramms
    data.to.plot[,1]<- factor(data.to.plot[,1], 
                              levels = old_levels[as.numeric(gsub("factor_","",hc_x_result$labels[hc_x_result$order]))])
  }else{hc_x_result=NULL}
  
  if(!is.null(dend_y_var)){
    #y dendrogram
    
    # For the PCA/MCA/FAMD, we change the factor names in factor_1, factor2 ... to avoid syntax problem
    if(class(save.data[,2])!="factor"){save.data[,2]=as.factor(save.data[,2])}
    old_levels=levels(save.data[,2])
    save.data[,2]=paste("factor",as.numeric(save.data[,2]),sep="_")
    
    check_y=check_FAMD_var(dend_y_var, type="y",save.data=save.data)
    if (check_y$success){
      
      FAMD_y_input=format_FAMD_input(check_y$famd_input, type="y", save.data=save.data)
      hc_y_result=run_FAMD_and_hclust(FAMD_y_input, metric=dist_method, method=hclust_method)
      
    }else{
      #check_y$success==FALSE
      stop(check_y$message)
    }
    
    # Reorder y axis according to dendrogramms
    data.to.plot[,2]<- factor(data.to.plot[,2], 
                              levels = old_levels[as.numeric(gsub("factor_","",hc_y_result$labels[hc_y_result$order]))])
  }else{hc_y_result=NULL}
  
  

  ### 3: GGPlot object ----
  scale.func <- switch(EXPR = scale.by, size = scale_size, 
                       radius = scale_radius, stop("'scale.by' must be either 'size' or 'radius'"))
  
  # Set theme function. Common function between classical dotplot and pacman plot
  # Transparent background, black border, no grid
  set_background=function(p){
    p <- p + theme(
      panel.background = element_rect(fill="transparent",linetype="solid", color="black"),
      panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
      legend.key = element_rect(colour = NA, fill = NA), axis.ticks = element_blank()) 
    
    
    return(p)
  }
  
  ### 3.1: PACMAN PLOT ----
  if (is.numeric(shape) & length(shape)==nrow(data.to.plot)){
    
    ### 2.1.1 : Main plot ----
    require(ggforce)
    
    # scale shape between 0 and 2*pi
    if(any(shape<0)){
      shape_pos=shape-min(na.omit(shape))
    }else{shape_pos=shape}
    pacman_opening=shape_pos*2*pi/max(na.omit(shape_pos))
    
    # scale size between 0 and 0.4
    r=data.to.plot[,3]
    if(any(r<0)){r=r-min(na.omit(r))}
    r=r*0.4/max(na.omit(r))
    
    # ggplot object initialization
    p <- ggplot()
    
    # Set theme
    p <- set_background(p)
    
    # Vertical coloring
    if(any(!is.na(vertical_coloring))){
      
      shading <- data.frame(min = c(0.5,seq(from = 1.5, to = max(as.numeric(as.factor(data.to.plot[,1]))), by = 1)),
                            max = c(seq(from = 1.5, to = max(as.numeric(as.factor(data.to.plot[,1]))) + 0.5, by = 1) ))
      # shading$col = rep_len(x=c(NA,"gray80"),length.out=length(unique(data.to.plot[,1]))))
      shading$col=rep(vertical_coloring, length.out=nrow(shading))
      
      # for (i in 1:nrow(shading)){
      #   p <- p + geom_rect(xmin = shading$min[i], xmax = shading$max[i],   ymin = -Inf, ymax = Inf,   fill = shading$col[i])
      # }
      p <- p + annotate(geom = "rect", xmin = shading$min, xmax = shading$max, 
                        ymin = 0.5, ymax = max(as.numeric(data.to.plot[,2]))+0.5, fill = shading$col)
      
      # Adding black lines to delimitate shades
      # We use another annotate to not display first and last line; otherwise add colour="black" in the previous annotate
      p <- p + annotate(geom = "segment", x = c(shading$min[-1], shading$max[-length(shading$max)]), 
                        xend = c(shading$min[-1], shading$max[-length(shading$max)]), 
                        y = 0.5, yend = max(as.numeric(data.to.plot[,2]))+0.5,
                        colour = "black")
    }
    
    # Horizontal coloring
    if(any(!is.na(horizontal_coloring))){
      shading <- data.frame(min = seq(from = 0.5, to = max(as.numeric(as.factor(data.to.plot[,2]))), by = 1),
                            max = seq(from = 1.5, to = max(as.numeric(as.factor(data.to.plot[,2]))) + 0.5, by = 1))
      # shading$col = rep_len(x=c(NA,"gray80"),length.out=length(unique(data.to.plot[,2])))
      shading$col=rep(horizontal_coloring, length.out=nrow(shading))
      
      # for (i in 1:nrow(shading)){
      #   p <- p + geom_rect(ymin = shading$min[i], ymax = shading$max[i],   xmin = -Inf, xmax = Inf,   fill = shading$col[i])
      # }
      p <- p + annotate(geom = "rect", xmin=0.5,xmax=max(as.numeric(data.to.plot[,1]))+0.5, ymin = shading$min, ymax = shading$max,
                        fill = shading$col)
      
      # Adding black lines to delimitate shades
      # We use another annotate to not display first and last line; otherwise add colour="black" in the previous annotate
      p <- p + annotate(geom = "segment", x=0.5,xend=max(as.numeric(data.to.plot[,1]))+0.5, y = c(shading$min[-1], shading$max[-length(shading$max)]), 
                        yend = c(shading$min[-1], shading$max[-length(shading$max)]),
                        colour = "black")
    }
    
    # Display pacman
    p <- p + geom_arc_bar(aes(x0 = as.numeric(data.to.plot[,1]), y0 = as.numeric(data.to.plot[,2]), r0 = 0, r = r, start = 0,
                              end = pacman_opening, fill = data.to.plot[,4]))
    # Pacman coloring
    if (is.numeric(data.to.plot[,4])){
      if (length(x = cols.use) == 1) {
        cols.use=c(cols.use,cols.use)# Monochrome
      }
      
      if (!(all(is.na(color.breaks.values)))){
        if(all(is.numeric(color.breaks.values))){
          cat("\n Putting color.breaks.values in color legend")
          color_breaks=color.breaks.values
        }else{
          cat("\n Non numeric value in color.breaks.values, considering color.breaks.number instead")
          color_breaks=(seq(min(na.omit(data.to.plot[,4])),max(na.omit(data.to.plot[,4])), length.out=color.breaks.number))
        }
      }else{
        color_breaks=(seq(min(na.omit(data.to.plot[,4])),max(na.omit(data.to.plot[,4])), length.out=color.breaks.number))
      }
      
      color_labels=ifelse((abs(color_breaks)<1e-2 | abs(color_breaks)>1e2) & color_breaks!=0,
                          format(color_breaks, scientific=TRUE, digits=3),
                          round(color_breaks,2)) # Values <1e-3 or >1e3 (excepted 0), are displayed with scientific notation
      
      if (length(x = cols.use) == 2){
        p <- p + scale_fill_gradient(low = cols.use[1], high = cols.use[2],
                                     breaks=color_breaks, labels=color_labels) # Gradient from the first color to the second
      } else {
        p <- p + scale_fill_gradientn(colours=cols.use, breaks=color_breaks, labels=color_labels)
      }
    } else {
      if(all(cols.use !="default")){
        if (length(cols.use)>length(unique(data.to.plot[,4]))){
          cols.use=cols.use[1:length(unique(data.to.plot[,4]))]
          cat(paste("\n To much colors are supplied. Only the first",length(unique(data.to.plot[,4])),"are used"))
        } else if (length(cols.use)<length(unique(data.to.plot[,4]))){
          cols.use=rep_len(cols.use, length.out = length(unique(data.to.plot[,4])))
          cat(paste("\n The number of colors is lower than the modality number. Re-using colors"))
        }
        p <- p + scale_fill_manual(values = cols.use)
      }
    }


    
    # Displaying text
    p <- p + geom_text(mapping = aes(x = as.numeric(data.to.plot[,1]), y = as.numeric(data.to.plot[,2]), label=data.to.plot[,5]), size=text.size)
    
    # Set discrete labels in the y axis - MOVED to common part (2.3)
    # p <- p + scale_y_continuous(breaks = 1:length(levels(data.to.plot[,2])),
    #                             labels = levels(data.to.plot[,2]))
    
    #Fill legend title
    p <- p + labs(fill=col_legend)
    
    # Remove fill legend
    # p <- p + guides(fill=F)
    
    ### 2.1.2 : Legend plot inside the pacman plot ----
    if (plot.legend){

      # Set pacman labels
      if (!(all(is.na(shape.breaks.values)))){
        if(all(is.numeric(shape.breaks.values))){
          cat("\n Putting shape.breaks.values in shape legend")
          ori_labels=shape.breaks.values
        }else{
          cat("\n Non numeric value in shape.breaks.values, considering shape.breaks.number instead")
          ori_labels=seq(min(na.omit(shape)),max(na.omit(shape)),length.out = shape.breaks.number)
        }
      }else{
        ori_labels=seq(min(na.omit(shape)),max(na.omit(shape)),length.out = shape.breaks.number)
      }
      
      cust_labels=ifelse((abs(ori_labels)<1e-2 | abs(ori_labels)>1e2) & ori_labels!=0, 
                         format(ori_labels, scientific=TRUE, digits=3), 
                         round(ori_labels,2)) # Values <1e-3 or >1e3 (excepted 0), are displayed with scientific notation
      
      pacman_breaks=length(ori_labels)
      x_pacman_leg=max(as.numeric(data.to.plot[,1]))+1.5
      y_pacman_leg=0:(pacman_breaks-1)
      
      if (!all(is.na(size_var))){
        
        if (!(all(is.na(size.breaks.values)))){
          if(all(is.numeric(size.breaks.values))){
            cat("\n Putting size.breaks.values in size legend")
            
            rr=data.to.plot[,3]
            if(any(rr<0)){rr=rr-min(na.omit(rr))}
            
            rrr=size.breaks.values
            if(any(rrr<0)){rrr=rrr-min(na.omit(rrr))}
            
            legend_breaks=rrr*0.4/max(na.omit(rr))
            legend_ori=size.breaks.values
            size.breaks.number=length(size.breaks.values)
          }else{
            cat("\n Non numeric value in size.breaks.values, considering size.breaks.number instead")
            legend_breaks=(seq(min(na.omit(r)),max(na.omit(r)), length.out=size.breaks.number))
            legend_ori=seq(min(na.omit(data.to.plot[,3])),max(na.omit(data.to.plot[,3])), length.out=size.breaks.number)
          }
        }else{
          legend_breaks=(seq(min(na.omit(r)),max(na.omit(r)), length.out=size.breaks.number))
          legend_ori=seq(min(na.omit(data.to.plot[,3])),max(na.omit(data.to.plot[,3])), length.out=size.breaks.number)
        }
        
        # legend_ori=legend_breaks/0.4*max(data.to.plot[,3])
        size_labels=ifelse((abs(legend_ori)<1e-2 | abs(legend_ori)>1e2) & legend_ori!=0,
                           format(legend_ori, scientific=TRUE, digits=3),
                           round(legend_ori,2)) # Values <1e-3 or >1e3 (excepted 0), are displayed with scientific notation
        
        if(pacman_breaks+size.breaks.number+1<=max(as.numeric(data.to.plot[,2]))){
          x_size_leg=max(as.numeric(data.to.plot[,1]))+1.5
          y_size_leg=(max(y_pacman_leg)+1.5):(max(y_pacman_leg)+size.breaks.number+0.5)
          
          y_size_leg=y_size_leg+mean(as.numeric(data.to.plot[,2]))-mean(c(y_size_leg,y_pacman_leg))
          y_pacman_leg=y_pacman_leg+mean(as.numeric(data.to.plot[,2]))-mean(c(y_size_leg,y_pacman_leg))
          
        }else{
          x_size_leg=max(as.numeric(data.to.plot[,1]))+4
          y_size_leg=1:size.breaks.number
          
          # Transform y_coord to center the legend according y-axis
          y_size_leg=y_size_leg+mean(as.numeric(data.to.plot[,2]))-mean(y_size_leg)
          y_pacman_leg=y_pacman_leg+mean(as.numeric(data.to.plot[,2]))-mean(y_pacman_leg)
        }
        y_coord=c(y_pacman_leg, y_size_leg)
        x_coord=c(rep(x_pacman_leg,pacman_breaks),rep(x_size_leg,size.breaks.number))
        labels_leg=c(cust_labels,size_labels)
        
        # p <- p + geom_arc_bar(aes(x0 = x_coord, y0 = y_coord, r0 = 0, r = c(rep(0.4,pacman_breaks),legend_breaks),
        #                           start = 0, end = c(seq(0,2*pi,length.out = pacman_breaks),rep(2*pi, size.breaks.number))))
        p <- p + geom_arc_bar(aes(x0 = x_coord, y0 = y_coord, r0 = 0, r = c(rep(0.4,pacman_breaks),legend_breaks),
                                  start = 0, end = c(ori_labels*2*pi/max(na.omit(shape_pos)),rep(2*pi, size.breaks.number))))
        p <- p + annotate(geom = "text", x = x_coord+1, y = y_coord, label=labels_leg)
        p <- p + annotate(geom="text", x = c(x_pacman_leg,x_size_leg), y = c(max(y_pacman_leg),max(y_size_leg))+1, 
                          label=c(shape_legend,size_legend))
        
      }else{
        x_coord=x_pacman_leg
        # Transform y_pacman_leg to center the legend according y-axis
        y_pacman_leg=y_pacman_leg+mean(as.numeric(data.to.plot[,2]))-mean(y_pacman_leg)
        
        p <- p + geom_arc_bar(aes(x0 = x_pacman_leg, y0 = y_pacman_leg, 
                                  r0 = 0, r = 0.4, start = 0,end = ori_labels*2*pi/max(na.omit(shape_pos))), size=0.5)
        p <- p + annotate(geom = "text", x = x_pacman_leg+1, y = y_pacman_leg, label=cust_labels)
        p <- p + annotate(geom="text", x = x_pacman_leg, y = max(y_pacman_leg)+1, label=shape_legend)
      }

    } # End plot legend
    
    # if(x.lab.rot){
    #   p <- p + theme(axis.text.x.bottom = element_text(hjust=1), axis.text.x.top = element_text(hjust=0))
    # }else{
    #   vjust=5
    #   p <- p + theme(axis.text.x.bottom = element_text(vjust=vjust), axis.text.x.top = element_text(vjust=-vjust)) 
    # }
    
    
  } # End PACMAN plot
  
  
  ### 3.2: DOT PLOT ----
  else{
    # ggplot object initilization
    p <- ggplot(mapping = aes(x = as.numeric(data.to.plot[,1]), y = as.numeric(data.to.plot[,2]), label=data.to.plot[,5]))
    
    # theme function
    p <- set_background(p)
    
    # Vertical coloring
    if(any(!is.na(vertical_coloring))){
      
      shading <- data.frame(min = c(0.5,seq(from = 1.5, to = max(as.numeric(as.factor(data.to.plot[,1]))), by = 1)),
                            max = c(seq(from = 1.5, to = max(as.numeric(as.factor(data.to.plot[,1]))) + 0.5, by = 1) ))
      # shading$col = rep_len(x=c(NA,"gray80"),length.out=length(unique(data.to.plot[,1]))))
      shading$col=rep(vertical_coloring, length.out=nrow(shading))
      
      for (i in 1:nrow(shading)){
        p <- p + geom_rect(xmin = shading$min[i], xmax = shading$max[i], 
                           ymin = 0.5, ymax = max(as.numeric(data.to.plot[,2]))+0.5,   fill = shading$col[i])
      }
      
      # p <- p + annotate(geom = "rect", xmin = shading$min, xmax = shading$max, ymin = -Inf, ymax = Inf,alpha = 0.1,
      #                   fill = shading$col) # Deprecated, on utilise geom_rect a la place
      # Adding black lines to delimitate shades
      # We use another annotate to not display first and last line; otherwise add colour="black" in the previous annotate
      p <- p + annotate(geom = "segment", x = c(shading$min[-1], shading$max[-length(shading$max)]),
                        xend = c(shading$min[-1], shading$max[-length(shading$max)]),
                        y = 0.5, yend = max(as.numeric(data.to.plot[,2]))+0.5,
                        colour = "black")
    }
    
    # Horizontal coloring
    if(any(!is.na(horizontal_coloring))){
      shading <- data.frame(min = seq(from = 0.5, to = max(as.numeric(as.factor(data.to.plot[,2]))), by = 1),
                            max = seq(from = 1.5, to = max(as.numeric(as.factor(data.to.plot[,2]))) + 0.5, by = 1))
      # shading$col = rep_len(x=c(NA,"gray80"),length.out=length(unique(data.to.plot[,2])))
      shading$col=rep(horizontal_coloring, length.out=nrow(shading))
      
      for (i in 1:nrow(shading)){
        p <- p + geom_rect(ymin = shading$min[i], ymax = shading$max[i],   
                           xmin=0.5,xmax=max(as.numeric(data.to.plot[,1]))+0.5,   fill = shading$col[i])
      }
      
      # p <- p + annotate(geom = "rect", xmin = -Inf, xmax = Inf, ymin = shading$min, ymax = shading$max,alpha = 0.1,
      #                   fill = shading$col) # Deprecated, on utilise geom_rect a la place
      # Adding black lines to delimitate shades
      # We use another annotate to not display first and last line; otherwise add colour="black" in the previous annotate
      p <- p + annotate(geom = "segment", x = 0.5,xend = max(as.numeric(data.to.plot[,1]))+0.5, y = c(shading$min[-1], shading$max[-length(shading$max)]),
                        yend = c(shading$min[-1], shading$max[-length(shading$max)]),
                        colour = "black")
    }
    
    plot_point=TRUE
    # if (length(size_var)==1 & length(col_var)==1){
    #   if(is.na(size_var) & is.na(col_var)){
    #     plot_point=FALSE
    #   }
    # }
    
    if(plot_point){
      # Displaying shapes
      if(length(shape)==1){
        p <- p + geom_point(mapping = aes(size = data.to.plot[,3], color = data.to.plot[,4]), shape=shape)
        
        # Displaying maximal shapes area (only when shape %in% c(15,16,17,18))
        if (all(shape %in% c(15,16,17,18)) & display_max_sizes){
          background_shape=ifelse(shape %in% c(15,16,17),shape-15, shape-13)
          p <- p + geom_point(mapping = aes(size = max(na.omit(data.to.plot[,3]))), colour="black",shape=background_shape)
        }
      } else {
        p <- p + geom_point(mapping=aes(size=data.to.plot[,3], color=data.to.plot[,4], shape=as.factor(shape)))
        
        # Setting unicode shapes
        
        if(all( shape_use=="default")){
          # unicode_shapes=c("\u25A0","\u25CF","\u25C6","\u25BA","\u25C4","\u25BC","\u25B2","\u25D8","\u25D9","\u2726", "\u2605", "\u2736", "\u2737")
          # unicode_shapes=c("\u23F9","\u23FA","\u25C6","\u25BA","\u25C4","\u25BC","\u25B2","\u25D8","\u25D9","\u2726", "\u2605", "\u2736", "\u2737")
          unicode_shapes=c("\u25A0","\u25CF","\u25C6","\u2BC8","\u2BC7","\u2BC6","\u2BC5","\u25D8","\u25D9","\u2726", "\u2605", "\u2736", "\u2737")
        }else{unicode_shapes= shape_use}
        
        
        if(length(unique(as.factor(shape))) > length(unicode_shapes)){
          cat(paste("\n The default shape palette can deal with a maximum of", length(unicode_shapes),"discrete values (You have",length(unique(as.factor(shape))) 
                    ,"). \n Recycling shapes. \n Consider specifying shapes manually by setting 'do.return=TRUE' and adding a scale_shape_manual command"))
          custom_shapes=rep_len(unicode_shapes,length.out = length(unique(as.factor(shape))))
        }else {custom_shapes=unicode_shapes[1:length(unique(as.factor(shape)))]}
        
        p <- p + scale_shape_manual(values=custom_shapes)
        
        # Adding maximal shape area as a shade
        if(display_max_sizes){
          p <- p + geom_point(mapping = aes(size = max(na.omit(data.to.plot[,3])),shape=as.factor(shape)), alpha=0.1)
        }
        
        # Increasing shape legend size
        p <- p + guides(shape = guide_legend(override.aes = list(size = 5)))
      }
      
      # Changing shape size + shape size legend
      
      if (!(all(is.na(size.breaks.values)))){
        if(all(is.numeric(size.breaks.values))){
          cat("Putting size.breaks.values in size legend")
          legend_breaks=size.breaks.values
        }else{
          cat("Non numeric value in size.breaks.values, considering size.breaks.number instead")
          legend_breaks=(seq(min(na.omit(data.to.plot[,3])),max(na.omit(data.to.plot[,3])), length.out=size.breaks.number))
        }
      }else{
        legend_breaks=(seq(min(na.omit(data.to.plot[,3])),max(na.omit(data.to.plot[,3])), length.out=size.breaks.number))
      }
      

      legend_labels=ifelse((abs(legend_breaks)<1e-2 | abs(legend_breaks)>1e2) & legend_breaks!=0, 
                           format(legend_breaks, scientific=TRUE, digits=3), 
                           round(legend_breaks,2)) # Values <1e-3 or >1e3 (excepted 0), are displayed with scientific notation
      p <- p + scale.func(range = c(0.1, shape.scale),limits = c(scale.min, scale.max), breaks = legend_breaks, labels = legend_labels)
      # /!\ It is important to no set the minimal range value to 0 (and >0.1) because its can induce shape size and position errors with specific shapes
      
      # Coloring shapes
      if (is.numeric(data.to.plot[,4])){
        if (length(x = cols.use) == 1) {
          cols.use=c(cols.use,cols.use)# Monochrome
        }
        
        if (!(all(is.na(color.breaks.values)))){
          if(all(is.numeric(color.breaks.values))){
            cat("\n Putting color.breaks.values in color legend")
            color_breaks=color.breaks.values
          }else{
            cat("\n Non numeric value in color.breaks.values, considering color.breaks.number instead")
            color_breaks=(seq(min(na.omit(data.to.plot[,4])),max(na.omit(data.to.plot[,4])), length.out=color.breaks.number))
          }
        }else{
          color_breaks=(seq(min(na.omit(data.to.plot[,4])),max(na.omit(data.to.plot[,4])), length.out=color.breaks.number))
        }
        
        color_labels=ifelse((abs(color_breaks)<1e-2 | abs(color_breaks)>1e2) & color_breaks!=0, 
                            format(color_breaks, scientific=TRUE, digits=3), 
                            round(color_breaks,2)) # Values <1e-3 or >1e3 (excepted 0), are displayed with scientific notation
        
        if (length(x = cols.use) == 2){
          p <- p + scale_color_gradient(low = cols.use[1], high = cols.use[2], 
                                        breaks=color_breaks, labels=color_labels) # Gradient from one color to an other
        } else {
          p <- p + scale_color_gradientn(colours=cols.use, breaks=color_breaks, labels=color_labels)
        }
      } else {
        if(all(cols.use !="default")){
          if (length(cols.use)>length(unique(data.to.plot[,4]))){
            cols.use=cols.use[1:length(unique(data.to.plot[,4]))]
            cat(paste("\n To much colors are supplied. Only the first",length(unique(data.to.plot[,4])),"are used"))
          } else if (length(cols.use)<length(unique(data.to.plot[,4]))){
            cols.use=rep_len(cols.use, length.out = length(unique(data.to.plot[,4])))
            cat(paste("\n The number of colors is lower than the modality number. Re-using colors"))
          }
          p <- p + scale_color_manual(values = cols.use)
        }
      }
      
    }
    
    # Displaying text
    p <- p + geom_text(size=text.size)
    
  }
  
  
  ### 3.3: Command lignes in common ----
  
  # Legend titles
  p$labels$size=size_legend
  p$labels$colour=col_legend
  p$labels$shape=shape_legend
  
  # Remove panel border wich contain plot and legend
  p <- p + theme(panel.background = element_rect(fill="transparent",linetype=0))
  
  # Add panel border which contain plot only (legend outside) => /!\ It adds an additional margin in dotplot (not in pacman plot)
  p <- p + geom_rect(aes(xmin=0.5,xmax=max(as.numeric(data.to.plot[,1]))+0.5,ymin=0.5,ymax=max(as.numeric(data.to.plot[,2]))+0.5),
                     alpha=0, colour="black")
  
  # Displaying x axis labels and y axis
  if(x.lab.pos %in% c("top","bottom")){
    p <- p + scale_x_continuous(breaks = 1:length(levels(data.to.plot[,1])),labels = levels(data.to.plot[,1]),
                                position = x.lab.pos)
  }else if (x.lab.pos == "both"){
    p <- p + scale_x_continuous(breaks = 1:length(levels(data.to.plot[,1])),labels = levels(data.to.plot[,1]),
                                sec.axis = dup_axis())
  }else if (x.lab.pos == "none"){
    p <- p + scale_x_continuous(breaks = NULL,labels = NULL)
  }
  
  if(y.lab.pos %in% c("left","right")){
    p <- p + scale_y_continuous(breaks = 1:length(levels(data.to.plot[,2])),
                                labels = levels(data.to.plot[,2]), position=y.lab.pos)
  }else if (y.lab.pos == "both"){
    p <- p + scale_y_continuous(breaks = 1:length(levels(data.to.plot[,2])),
                                labels = levels(data.to.plot[,2]), sec.axis = dup_axis())
  }else if (y.lab.pos == "none"){
    p <- p + scale_y_continuous(breaks = NULL,labels = NULL)
  }
  

  
  # Deleting axis titles
  p <- p + theme(axis.title.x = element_blank(), axis.title.y = element_blank())
  
  # No display legend if true
  if (!plot.legend) {
    p <- p + theme(legend.position = "none")
  }
  
  # No color legend if true
  if(no_color_legend){
    p <- p+guides(colour=F, fill=F)
  }
  # No size legend if true
  if(no_size_legend){
    p <- p+guides(size=F)
  }
  
  
  # Rotate x-axis labels if TRUE
  if (x.lab.rot) {
    p <- p + theme(axis.text.x.bottom = element_text(angle=90, hjust=1, vjust = 0.5), 
                   axis.text.x.top = element_text(angle=90, hjust=0, vjust = 0.5))
  }
  
  
  ### 3: Output ----
  
  # Arrange dotplot with dendrogramms
  theme_dendro <- theme(
    # panel.border = element_rect(linetype = "dashed", colour = "black", fill=NA),
    plot.margin=unit(c(0, 0, 0, 0), "in"),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_blank(),
    axis.title = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    axis.line = element_blank())
  
  if(!is.null(hc_x_result)){
    # Arrange x dendrogram
    ddata_x <- ggdendro::segment(dendro_data(hc_x_result))
    dendro_horizontal <- 
      ggplot(ddata_x) + 
      geom_segment(aes(x=x, y=y, xend=xend, yend=yend))  + theme_dendro
    
    # If pacman plot, adjust xlim to not overlap with legend plot
    if(is.numeric(shape) & length(shape)==nrow(data.to.plot)){
      if(plot.legend){
        dendro_horizontal <- dendro_horizontal + xlim(1-0.4,max(x_pacman_leg,x_coord)+0.75)
      }else {
        dendro_horizontal <- dendro_horizontal + xlim(1-0.4,length(levels(data.to.plot[,1]))+0.4)
      }
    } else {
      # If dot plot, adjust xlim to panel border
      dendro_horizontal <- dendro_horizontal + xlim(0.5,max(as.numeric(data.to.plot[,1]))+0.5)
      
    }
    
    
    
    AlignPlots <- function(...) {
      # https://stackoverflow.com/a/30414008
      
      LegendWidth <- function(x) x$grobs[[8]]$grobs[[1]]$widths[[4]]
      
      plots.grobs <- lapply(list(...), ggplotGrob)
      
      ##Original command lines
      # max.widths <- do.call(unit.pmax, lapply(plots.grobs, "[[", "widths"))
      # plots.grobs.eq.widths <- lapply(plots.grobs, function(x) {
      #   x$widths <- max.widths
      #   x
      # })
      
      # Custom command lines : Reference widths = dotplot widths (second list element)
      plots.grobs.eq.widths <- lapply(plots.grobs, function(x) {
        x$widths <- plots.grobs[[2]]$widths
        x
      })
      # End custom command lines
      
      legends.widths <- lapply(plots.grobs, LegendWidth)
      max.legends.width <- do.call(max, legends.widths)
      plots.grobs.eq.widths.aligned <- lapply(plots.grobs.eq.widths, function(x) {
        if (is.gtable(x$grobs[[8]])) {
          x$grobs[[8]] <- gtable_add_cols(x$grobs[[8]],
                                          unit(abs(diff(c(LegendWidth(x),
                                                          max.legends.width))),
                                               "mm"))
        }
        x
      })
      
      plots.grobs.eq.widths.aligned
    }
    plots <- AlignPlots(dendro_horizontal,p)
  }
  
  if(!is.null(hc_y_result)){
    # Arrange y dendrogram
    ddata_y <- ggdendro::segment(dendro_data(hc_y_result))
    dendro_vertical <- 
      ggplot(ddata_y) + 
      geom_segment(aes(x=x, y=y, xend=xend, yend=yend)) +
      coord_flip()+ scale_y_reverse() + theme_dendro
    
    if(is.numeric(shape) & length(shape)==nrow(data.to.plot)){
      # If pacman plot, change dendrogramm xlim
      dendro_vertical <- dendro_vertical + xlim(1-0.4,length(levels(data.to.plot[,2]))+0.4)
    } else {
      # If dotplot, adjust xlim to panel border
      dendro_vertical <- dendro_vertical + xlim(0.5,max(as.numeric(data.to.plot[,2]))+0.5)
    }
    
    # For vertical dendrogramm, we adjust heigths instead of widths
    # https://stackoverflow.com/a/30414008
    dotplot_grobs=ggplotGrob(p)
    y_dendro_grobs=ggplotGrob(dendro_vertical)
    
    plots.grobs <- list(y_dendro_grobs,dotplot_grobs)
    plot.grobs <- list(dotplot_grobs)
    
    ##Original command lines
    # max.heights <- do.call(unit.pmax, lapply(plots.grobs, "[[", "heights"))
    # plots.grobs.eq.heights <- lapply(plots.grobs, function(x) {
    #   x$heights <- max.heights
    #   x
    # })
    
    # Custom command lines : Reference heights = dotplot heights
    plots.grobs.eq.heights <- lapply(plots.grobs, function(x) {
      x$heights <- plots.grobs[[2]]$heights
      x
    })
    # End Custom command lines
    
    LegendHeight <- function(x) x$grobs[[8]]$grobs[[1]]$heights[[4]]
    legends.heights <- lapply(plots.grobs, LegendHeight)
    max.legends.height <- do.call(max, legends.heights)
    plots.grobs.eq.heights.aligned <- lapply(plots.grobs.eq.heights, function(x) {
      if (is.gtable(x$grobs[[8]])) {
        x$grobs[[8]] <- gtable_add_cols(x$grobs[[8]],
                                        unit(abs(diff(c(LegendHeight(x),
                                                        max.legends.height))),
                                             "mm"))
      }
      x
    })
    
  }
  
  if(!is.null(hc_x_result) & !is.null(hc_y_result)){
    #display dotplot and both dendrograms
    final_plot <- grid.arrange(grob(),plots[[1]],plots.grobs.eq.heights.aligned[[1]],plots[[2]], ncol=2, widths=c(1,5), heights=c(1,5))
  } else if (!is.null(hc_x_result)){
    #display dotplot and horizontal dendrogram only
    final_plot <- grid.arrange(plots[[1]],plots[[2]], ncol=1, heights=c(1,5))
  } else if (!is.null(hc_y_result)){
    #display dotplot and vertical dendrogram only
    final_plot <- grid.arrange(plots.grobs.eq.heights.aligned[[1]],plots.grobs.eq.heights.aligned[[2]], ncol=2, widths=c(1,5))
  }else {
    #display dotplot only
    final_plot <- p
    plot(final_plot)
  }
  
  
  if (do.return) {
    return(final_plot)
  }
  
  ### End function ----
}
