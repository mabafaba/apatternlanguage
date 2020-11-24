#' parse the prepared text into a graph object
#'
#' @useinternal if true, uses the data included in the package (highly recommended.). Otherwise you need the specifically prepared text files.
#' @value the chapters of the book 'A pattern language' as an igraph object
#' @export
#'
parse_apl_to_graph<-function(){
  # if we didn't have the text saved inside the package as r objects, we would do:
  # chapter_headings<-read.csv("./apl_titles.csv",stringsAsFactors = F) %>% unlist %>% unname
  # apl<-readChar('./a_pattern_language_markers_final.txt',file.info('./a_pattern_language_markers_final.txt')$size)

  chapter_divider<-">>>>>"
  section_divider<-"<<<<<"
  apl<-unlist(strsplit(apatternlanguage::apl,chapter_divider))[-1]
  apl<-lapply(lapply(apl,strsplit,split=section_divider),unlist)

  lapply(apl,function(chapter){
    lapply(chapter,function(x){
      links<-str_extract_all(x,"\\([0-9][0-9]*\\)") %>% unlist %>% gsub("[^0-9]", "",.) %>% as.numeric
    })
  })->b

  b<-b %>% lapply(function(x){lapply(x,function(x){chapter_headings[x]})})
  names(b) <-chapter_headings

  (lapply(apl,function(x){
    strsplit(x[1],split='\n')[[1]][1]
  }) %>% unlist)



  partA<-lapply(seq_along(b),function(i){
    cbind(rep(chapter_headings[i],length(unlist(b[[i]][1]))),unlist(b[[i]][1]))
  })%>%do.call(rbind,.)

  partB<-lapply(seq_along(b),function(i){
    cbind(rep(chapter_headings[i],length(unlist(b[[i]][2]))),unlist(b[[i]][2]))
  })%>%do.call(rbind,.)

  partC<-lapply(seq_along(b),function(i){
    cbind(rep(chapter_headings[i],length(unlist(b[[i]][3]))),unlist(b[[i]][3]))
  })%>%do.call(rbind,.)



  el<-rbind(data.frame(from = partA[,1],to=partA[,2],part='A'),
            data.frame(from = partC[,2],to=partC[,1],part='C'))

  vl<-do.call(rbind,apl)
  colnames(vl)<-LETTERS[1:3]
  rownames(vl)<-names(b)
  g<-graph.data.frame(el)
  g<-simplify(g,remove.multiple=T,remove.loops=T)


  return(g)
}




#' identifying downstream / upstream nodes
#'
#' @export
#'
node_river<-function(g,v,upstream=T,downstream=T,keep.bridges=T,ids_only=T){
  g %>% igraph::as_data_frame("both") ->gdf
  vname<-gdf$vertices$name[v]

  upstream_edges<-which(gdf$edges$to==vname)
  upstream_nodes<-match(gdf$edges[upstream_edges,]$from,gdf$vertices$name)
  downstream_edges<-which(gdf$edges$from==vname)
  downstream_nodes<-match(gdf$edges[downstream_edges,]$to,gdf$vertices$name)
  edges_to_keep<-c()
  nodes_to_keep<-c()
  if(upstream){
    edges_to_keep<-upstream_edges
    nodes_to_keep<-upstream_nodes
  }

  if(downstream){
    edges_to_keep<-c(edges_to_keep,downstream_edges)
    nodes_to_keep<-c(nodes_to_keep,downstream_nodes)
  }
  if(!keep.bridges){
    g %>% delete.edges(c(1:length(E(g)))[-edges_to_keep]) ->g
  }
  node.relation<-rep(NA,length(V(g)))
  node.relation[upstream_nodes]<-'upstream'
  node.relation[downstream_nodes]<-'downstream'
  node.relation[v]<-"none"
  vertex_attr(g,'relation')<-node.relation

  edge.relation<-rep('none',length(E(g)))
  edge.relation[upstream_edges]<-'upstream'
  edge.relation[downstream_edges]<-'downstream'
  edge.relation[v]<-"self"
  edge_attr(g,'relation')<-edge.relation


  g2<-subgraph(g,c(nodes_to_keep,v))
  if(ids_only){
    return(list(edges.down=downstream_edges,
                edges.up=upstream_edges,
                nodes.down=downstream_nodes,
                nodes.up=upstream_nodes
    )
    )

  }else{
    return(g2)
  }

}


#' Recursively identifying downstream nodes
#' @param g igraph graph object
#' @param v the id of the central node to start from
#' @param direction should be 'downstream', 'upstream' or 'both'
#' @param max.levels how many steps away from source node to go
#' @level what is the level we start from (default 0). Usually best to leave at 0
#' @level children_found leave empty
#' @export
#'
node_plant_node_ids<-function(g,v,direction='downstream',max.levels=2,level=0,children_found=c()){
  level<-level+1
  if(level>=max.levels){
    return( list(downstream_tree=unique(c(v,children_found)),
                 children_found=unique(c(v,children_found))))
  }

if(!(direction %in%c('downstream','upstream','both'))){stop('direction must be "downstream", "upstream" or "both".')}
  if(direction=="downstream"){downstream_this_children_vs<-node_river(g,v)$nodes.down}
  if(direction=="upstream"){downstream_this_children_vs<-node_river(g,v)$nodes.up}
  if(direction=="both"){downstream_this_children_vs<-c(node_river(g,v)$nodes.down,node_river(g,v)$nodes.up)}
  children_found<-unique(c(children_found,v))

  downstream_this_legacy<-c()
  downstream_this_legacy_nodes<-c()
  for(this_child_v in downstream_this_children_vs){
    if(!(this_child_v%in%children_found)){
      # get info from downstream
      downstream_this_legacy<-node_plant_node_ids(g = g,
                                                  v = this_child_v,
                                                  direction = direction,
                                                  max.levels=max.levels,
                                                  level=level,
                                                  children_found = children_found)
      # nodes to add:
      downstream_this_legacy_nodes<-c(downstream_this_legacy_nodes,downstream_this_legacy$downstream_tree)

      # add nodes already checked for children to skip list:
      children_found<-unique(c(children_found,downstream_this_legacy$children_found))
    }
  }

  list(downstream_tree=unique(c(v,downstream_this_children_vs,downstream_this_legacy_nodes)),
       children_found=children_found)

}




#' get the id of a a node in an igraph object by regex name search
#' @param g igraph graph object
#' @nodename part of the name of the node we look for
#' @value the integer id of the first matching node
#' @export
nodeid<-function(g,nodename){
  grep(tolower(nodename),tolower(V(g)$name))[1]
}


#' common graph vertex statistics
#' @param g igraph graph object
#' @value a dataframe with the results
#' @export
g_vertex_stats<-function(g){
  normalise<-function(x){(x-min(x))/max(x)}
  vs<-data.frame(name=V(g)$name,
                 cl_in=normalise(closeness(g,mode = 'in')),
                 cl_out=normalise(closeness(g,mode = 'out')),
                 cl_all=normalise(closeness(g,mode = 'all')),
                 deg_in=(degree(g,mode='in')),
                 deg_out=(degree(g,mode='out')),
                 deg_all=(degree(g,mode='all')),
                 pagerank_in=normalise(page_rank_standardised(g,'in')),
                 pagerank_out=normalise(page_rank_standardised(g,'out')),
                 pagerank_all=normalise(page_rank_standardised(g,'all'))
                 )




}

#' graph page rank with 'mode' argument
#'
#' to match the other igraph vertex statistic functions
#' @param g igraph graph object
#' @param mode mode must be "in", "out" or "all' (for directed pagerank)
#' @export
page_rank_standardised<-function(g,mode='all'){
  if(!(mode %in% c('in','out','all'))){stop('mode must be "in", "out" or "all')}

  directed<-ifelse(mode=='all',FALSE,TRUE)
  if(mode=='out'){g<-graph.reverse(g)}
    pr<-page.rank(g,directed = directed)$vector
}

#' reverse graph edge direction
#'
#' @param graph igraph graph object
#' @value igraph graph object (reversed edge direction)
#' @export
graph.reverse <- function (graph) {
  if (!is.directed(graph))
    return(graph)
  e <- get.data.frame(graph, what="edges")
  ## swap "from" & "to"
  neworder <- 1:length(e)
  neworder[1:2] <- c(2,1)
  e <- e[neworder]
  names(e) <- names(e)[neworder]
  graph.data.frame(e, vertices = get.data.frame(graph, what="vertices"))
}


#' get graph vertices' chapter number
#'
#' @param graph igraph graph object
#' @return integer vector with chapter numbers matching vertex order in g
#' @export
apl_chapter_number<-function(g){
  strsplit(names(V(g))," ") %>% lapply(first) %>% unlist %>% as.numeric
  }




