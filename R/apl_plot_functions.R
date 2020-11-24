
#' plotting directed closeness centrality
#' @param g igraph graph object
#' @export
g_plot_vertex_stats_closeness_in_out<-function(g){
  vs<-g_vertex_stats(g)
  ggplot(data = vs,mapping = aes(cl_out,cl_in))+theme_minimal()+xlim(c(-0.2,1.2))+ylim(c(-0.2,1.2))+
    geom_point(aes(),color='red',alpha=0.5)+
    # geom_text(label=vs$name,alpha=1,nudge_x = 0.01,nudge_y = -0.015,hjust=0,check_overlap = T,size=2.6)
    annotate(geom = "text",vs$cl_out+0.01,vs$cl_in, label =  tolower(vs$name), color = "black",
             angle = 45,hjust=0,check_overlap=T)+theme(plot.margin = unit(c(1,1,1,1), "cm"))+
    labs(x='average distance to influencing other patterns ',y="average distance being influenced by other patterns")

}


#' plotting node in/out degree (scatterplot)
#' @param g igraph graph object
#' @param labels if TRUE, points are labeled (not implemented)
#' @export
g_plot_vertex_stats_degree_in_out<-function(g,labels=F){
  vs<-g_vertex_stats(g)
  ggplot(data = vs,mapping = aes(vs$deg_in,vs$deg_out))+theme_minimal()+
  # +xlim(c(-0.2,1.2))+ylim(c(-0.2,1.2))+
    geom_point(aes(),color='red',alpha=0.5)+
    # geom_text(label=vs$name,alpha=1,nudge_x = 0.01,nudge_y = -0.015,hjust=0,check_overlap = T,size=2.6)
    annotate(geom = "text",vs$deg_in+0.01,vs$deg_out, label = tolower(vs$name), color = "black",
             angle = -45,hjust=0,check_overlap=T)+theme(plot.margin = unit(c(1,1,1,1), "cm"))+
    labs(x='constructed by patterns',y="contributing to patterns")

}

#' plotting node degree (density distribution)
#'
#' @param g igraph graph object
#' @export
g_plot_degree_all<-function(g){
  vst<-g_vertex_stats(g)
  ggplot(vst,aes(vst$deg_all))+theme_minimal()+
    labs(x='degree',y='probability density')+geom_histogram(aes(y=..density..),fill='lightgrey',binwidth=1)+stat_density(col='black',fill=NA)
  # +
  #  stat_density(data=data.frame(deg_all=rnorm(1000,degree(g) %>% mean,sd = sd(degree(g)))),mapping = aes(deg_all),col='black',fill=NA)
  }




#' plotting the local part of a graph
#'
#' @param g igraph graph object
#' @export
g_plot_local<-function(g,v,direction = "upstream",max.levels = 3,...){
  local_nodes<-node_plant_node_ids(g,
                                   v = v,
                                   direction = direction,
                                   max.levels = max.levels)
  V(g)$cols<-'grey'
  V(g)$cols[v]<-'red'
  subg<-subgraph(g,local_nodes$children_found)

  g_plot(subg,vertex.color=V(subg)$cols,...)
}


#' simple graph plotting
#'
#' @param g the igraph object to plot
#' @param interactive logical; if TRUE, uses networkD3 for interactive animated plot
#' @export
g_plot <- function(g,interactive=T,...){
  if(interactive){

    return(g_plot_interactive(g,...))}
  if(!('vertex.label.color' %in% names(list(...)))){vertex.label.color<-'black'}else{vertex.label.color<-list(...)$vertex.label.color}
  if(!('vertex.frame.color' %in% names(list(...)))){vertex.frame.color<-'black'}else{vertex.frame.color<-list(...)$vertex.frame.color}
  if(!('vertex.label.cex' %in% names(list(...)))){vertex.label.cex<-0.8}else{vertex.label.cex<-list(...)$vertex.label.cex}

  plot.igraph(g,
              vertex.size=4,vertex.label.color=vertex.label.color,vertex.frame.color=vertex.frame.color,vertex.label.cex=vertex.label.cex,vertex.label.dist=1,
              vertex.label.family='sans', edge.label.family='sans',
              ...)

}


#' simplified networkD3::forceNetwork() + with new defaults
#'
#' @param g igraph graph object
#' @groups groups to assign to nodes
#' @weights weights to assign to edges
#' @export
g_plot_interactive<-function(g,...,groups=1,weights=1){
  gd3<-igraph_to_networkD3(g)
  gd3$nodes$group<-groups
  gd3$links$value<-weights
  # why networkD3, why?
  d3ColourScale<-function(groups,colors){
    jsscale<-paste0('d3.scaleOrdinal().domain([',
                    paste0(paste0('"',groups,'"'),collapse=","),
                    "]).range([" ,
                    paste0(paste0('"',colors,'"'),collapse=","),
                    "]);")
    return(jsscale)
  }

  charge<--4000
  # smaller charge if many nodes:
  charge<- charge/log(length(V(g)))

  forceNetwork(Links = gd3$links, Nodes = gd3$nodes,
               Source = "source", Target = "target",
               Value = "value", NodeID = "name",
               Group = "group",
               arrows = T,
               opacityNoHover = 1,
               legend = F,
               fontFamily = 'sans-serif',
               fontSize = 12,
               linkColour = 'grey',
               charge = charge,
               linkDistance=50,
               # width=1,
               colourScale = d3ColourScale(groups,'#000000'),
               zoom=T,
               opacity=1
  )


}






set_defaults<-function(arglist,...){
defaults<-list(...)
for(i in 1:length(defaults)){
  if(!(names(defaults)[i]%in%names(arglist))){
    arglist[[names(defaults)[i]]]<-defaults[[i]]
  }
}
return(arglist)
}


#' plot graph distance matrix
#'
#' @param g igraph graph object
#' @param mode 'in', 'out' or 'all' (for directed graphs)
#' @export
g_plot_distance<-function(g,mode='all'){
  dists<-distances(g=g,mode=mode)
  return(dists %>% raster %>% plot(box=F,axes=F,frame.plot=F,col=topo.colors(100),xaxt='n',ann=FALSE, yaxt='n',useRaster=F,asp=1))
}

