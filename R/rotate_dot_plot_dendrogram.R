#' Interactively rotate dendrograms from dot_plot outputs
#' 
#' Take a output from dot_plot function and allow interactive dendrogram rotation with dendextend package
#'
#' @encoding UTF-8
#' @param dot_plot_output Output from dot_plot function 
#' @param axis_to_rotate Dendrogram to rotate "x" or "y"
#' 
#' @return Print and return rotated dot plot output
#' @examples 
#' # Perform dot_plot
#' dot_plot_output = dot_plot(...)
#' # y rotation
#' r1=rotate_dot_plot_dendrogram(dot_plot_output, "y")
#' # add x rotation to previous result
#' r2=rotate_dot_plot_dendrogram(r1, "x")
#' @author Simon Leonard - simon_leonard[a]hotmail.fr

rotate_dot_plot_dendrogram=function(dot_plot_output, axis_to_rotate=c("x","y")){
  require(dendextend)
  require(ggplot2)
  
  axis_to_rotate=match.arg(axis_to_rotate)
  
  
  ### Check that dot_plot_output is an output of dot_plot_function ----
  if(class(dot_plot_output)!="list"){
    stop("input object not supported, please provide an output from the dot_plot function")
  }
  if(!all(c("input_data","command","plot") %in% names(dot_plot_output))){
    stop("input object not supported, please provide an output from the dot_plot function")
  }
  
  # Check that dendrogram were computed
  if(axis_to_rotate=="x" & (!"dendrogram_x" %in% names(dot_plot_output))){
    stop("Cannot rotate x dendrogramm as it was not computed")
  }
  if(axis_to_rotate=="y" & (!"dendrogram_y" %in% names(dot_plot_output))){
    stop("Cannot rotate y dendrogramm as it was not computed")
  }
  
  
  
  ### Rotate dendrogram and change levels order ----
  index=ifelse(axis_to_rotate=="x",1,2)
  
  dend_to_rotate=dot_plot_output[[paste("dendrogram", axis_to_rotate, sep="_")]]
  labels(dend_to_rotate)=levels(dot_plot_output$input_data[,index])
  
  new_dend=click_rotate(dend_to_rotate, continue = TRUE)

  dot_plot_output$input_data[,index]=factor(dot_plot_output$input_data[,index], levels=labels(new_dend))

    

  ### Performing dotplot again without dendrogramms----
  # Get original command & modify some parameters (no plot no original dendro) 
  com=dot_plot_output$command
  com[[paste("dend",axis_to_rotate,"var", sep="_")]]=NULL
  com$do.plot=FALSE
  
  # Replace data.to.plot input with ne re ordered one
  com=as.character(as.expression(com))
  com=gsub("\n","",com)
  
  part1=gregexpr("dot_plot\\(", com)[[1]][1]
  part1_end=part1+nchar("dot_plot(")
  
  part2=gregexpr(",", com)[[1]][1]
  
  com=paste(substring(com, part1, part1_end-1),
       "dot_plot_output$input_data",
       substring(com, part2), sep="")
  
  # Execute new dot plot
  new_dot_plot=eval(parse(text = com))
  
  
  
  ### Adding dendrogram to new dotplot plot and output object ----
  new_dot_plot[[paste("dendrogram", axis_to_rotate, sep="_")]]=new_dend
  new_dend_data=segment(dendro_data(new_dend))
  
  if(axis_to_rotate=="y"){
    
    dendro_vertical <- dot_plot_output$raw_dend_ggplot + geom_segment(data = new_dend_data, mapping = aes(x=length(labels(dend_to_rotate))+0.5-y,
                                                                          xend=length(labels(dend_to_rotate))+0.5-yend, 
                                                                          y=x,yend=xend, label=NULL))
    # dendro_vertical <- dendro_vertical+ theme_void() + 
    #   coord_cartesian(xlim = range(c(length(labels(dend_to_rotate))+0.5-new_dend_data$y,length(labels(dend_to_rotate))+0.5-new_dend_data$yend)),
    #                                                    ylim=c(0.5,length(labels(dend_to_rotate))+0.5),
    #                                                    expand=F, default = T) + theme(plot.margin = unit(c(0,0,0,2), units = "points"))
    # dendro_vertical <- dendro_vertical+ theme_void()
    
    new_dot_plot$plot[[1]][[3]]=dendro_vertical
    
    #Keep other dendrogram from input object
    new_dot_plot$plot[[1]][[1]]=dot_plot_output$plot[[1]][[1]]
    
  }else{
    dendro_horizontal <- dot_plot_output$raw_dend_ggplot + geom_segment(data = new_dend_data, mapping = aes(x=x,xend=xend, 
                                                                            y=y,yend=yend, label=NULL))
    dendro_horizontal <- dendro_horizontal+ theme_void()
    
    new_dot_plot$plot[[1]][[1]]=dendro_horizontal
    
    #Keep other dendrogram from input object
    new_dot_plot$plot[[1]][[3]]=dot_plot_output$plot[[1]][[3]]
  }
  
  
  ### final output ----
  # Arrange final object
  layout=rbind(c(NA,NA,1,NA,NA,NA,NA),
               c(NA,NA,2,NA,NA,NA,NA),
               c(3,4,5,6,7,8,9),
               c(NA,NA,10,NA,NA,NA,NA))
  final_plot=grid.arrange(grobs=new_dot_plot$plot[[1]], 
                         layout_matrix = layout, 
                         widths =dot_plot_output$plot$widths, 
                         heights = dot_plot_output$plot$heights)
  new_dot_plot$plot=final_plot
  
  # Add dot plot command to object
  new_dot_plot$command=parse(text=com)[[1]]
  
  #Return object
  return(new_dot_plot)
  
}





