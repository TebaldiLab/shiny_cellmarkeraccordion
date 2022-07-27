# hierachies plot function cell types input ----
hierac_plot1<-function(marker_table, ontology_celltype, graphnel_plot, input_celltype,cellid) {
  if (length(input_celltype) >= 1) {
    onto_igraph<-graph_from_graphnel(graphnel_plot, name = TRUE, weight = TRUE, unlist.attrs = TRUE)
    V(onto_igraph)$label = V(onto_igraph)$name
    V(onto_igraph)$name = factor(V(onto_igraph)$name, levels=as.character(V(onto_igraph)$name))
    mygraph2 <- from_igraph(onto_igraph)
    nodes<-mygraph2[["nodes_df"]]
    edges<-mygraph2[["edges_df"]]
    colnames(edges)<-c("edge_id", "from" ,"to"  , "rel" )
    nodes_edges1<-merge(nodes,edges,by.x="id",by.y="from")
    colnames(nodes_edges1)<-c("from_id" ,"type" ,   "label_from"  , "edge_id" ,"to_id"    ,  "color" )
    nodes_edges1$rel<-NULL
    nodes_edges1$type<-NULL
    
    nodes_edges2<-merge(nodes,edges,by.x="id",by.y="to")
    colnames(nodes_edges2)<-c("to_id" ,"type" ,   "label_to"  , "edge_id" ,"from_id"    ,  "col" )
    nodes_edges2$rel<-NULL
    nodes_edges2$type<-NULL
    
    new_nodes_edges<-merge(nodes_edges1,nodes_edges2,by="edge_id")
    nodes_edges<-new_nodes_edges[,c(1,2,3,4,7)]
    colnames(nodes_edges)<-c("edge_id","from_id","label_from","to_id","label_to"  )
    
    node_list_from<-unique(nodes_edges[,c("from_id","label_from")])
    colnames(node_list_from)<-c("id","label")
    node_list_to<-unique(nodes_edges[,c("to_id","label_to")])
    colnames(node_list_to)<-c("id","label")
    node_list<-rbind(node_list_from,node_list_to)
    
    node_list<-as.data.table(node_list)
    node_list[,value:=0.8]
    
    if(nrow(nodes)==1){
      node_list<-nodes 
      node_list$type<-NULL
      node_list<-as.data.table(node_list)
      node_list[,fillcolor:= ifelse(label %in%  marker_table[celltype_species %in% input_celltype]$celltype, "#FF8C69","#E1E1E1")]
      node_list[,value:=0.8]
      
    }
    if (length(input_celltype)>=1){
      node_list[,fillcolor:= ifelse(label %in%  marker_table[celltype_species %in% input_celltype]$celltype, "#FF8C69","#E1E1E1")]
    }
    
    
    
    # node_list<-node_list[, color:= ifelse(label %in%  marker_table[celltype_species %in% input$descendantsof]$celltype, "#7B1B02",
    #                                       ifelse(label %in% marker_table[celltype_species %in% input$celltype]$celltype, "#FF8C69","#E1E1E1"))]
    
    node_list<-node_list[, fontcolor:="black"]
    
    if(cellid==TRUE){
      node_list<-merge(node_list,ontology_celltype,by.x="label",by.y="cell_type")
      node_list<-as.data.table(node_list)
      node_list<-node_list[, label:= paste0(label," ", cell_ID)]
      
    }
    node_list<-unique(node_list)
    
    node_list<-node_list[, label:=str_replace_all(label, " ", "\n")]
    
    
    edge_list<-nodes_edges[,c("from_id","to_id")]
    #assign color
    edge_list<-as.data.table(edge_list)
    edge_list[,color:="#4F4F4F"]
    # Create the graph object
    if(nrow(nodes)==1){
      i_graph_1 <- create_graph()
      
      # It will start off as empty
      i_graph_1 %>% is_graph_empty()
      # Add the nodes to the graph
      i_graph_2 <-
        i_graph_1 %>%
        add_nodes_from_table(
          table = node_list,
          label_col = label,
        ) %>%
        set_node_attrs(
          node_attr = shape,
          value = "circle"
        ) %>%
        set_node_attrs(
          node_attr = width,
          value = 0.2
        ) %>%
        set_node_attrs(
          node_attr = height,
          value = 0.2
        ) %>%
        set_node_attrs(
          node_attr = fixedsize,
          value = "shape"
        ) %>%
        set_node_attrs(
          node_attr = fontsize,
          value = 3
        ) %>%
        set_node_attrs(
          node_attr = fontname,
          value = "URWTimes"
        )
      
      
      
      return(render_graph(i_graph_2))
    }
    else{
      i_graph_1 <- create_graph()
      
      # It will start off as empty
      i_graph_1 %>% is_graph_empty()
      # Add the nodes to the graph
      i_graph_2 <-
        i_graph_1 %>%
        add_nodes_from_table(
          table = node_list,
          label_col = label,
        ) %>% 
        add_edges_from_table(
          table = edge_list,
          from_col = from_id,
          to_col = to_id,
          from_to_map = id_external
        ) %>%
        
        set_node_attrs(
          node_attr = shape,
          value = "circle"
        ) %>%
        set_node_attrs(
          node_attr = fixedsize,
          value = "shape"
        ) %>%
        # set_node_attrs(
        #   node_attr = height,
        #   value = 1
        # ) %>%
        set_node_attrs(
          node_attr = fontsize,
          value = 9
        ) %>%
        set_node_attrs(
          node_attr = fontname,
          value = "URWTimes"
        ) %>%
        set_edge_attrs(
          edge_attr = arrowsize,
          value = 0.5
        ) %>%
        set_edge_attrs(
          edge_attr = penwidth,
          value = 1
        )  #FF0000=red , #008000 green  ,
      
      i_graph_tree<-render_graph(i_graph_2,layout = "tree",output = "graph",width=150,height=1000)
      
      return(i_graph_tree)  
    }
    
  }
}


hierac_plot1_desc<-function(marker_table, ontology_celltype, graphnel_plot, input_celltype,descendant_table,cellid) {
  if (length(input_celltype) >= 1) {
    onto_igraph<-graph_from_graphnel(graphnel_plot, name = TRUE, weight = TRUE, unlist.attrs = TRUE)
    V(onto_igraph)$label = V(onto_igraph)$name
    V(onto_igraph)$name = factor(V(onto_igraph)$name, levels=as.character(V(onto_igraph)$name))
    mygraph2 <- from_igraph(onto_igraph)
    nodes<-mygraph2[["nodes_df"]]
    edges<-mygraph2[["edges_df"]]
    colnames(edges)<-c("edge_id", "from" ,"to"  , "rel" )
    nodes_edges1<-merge(nodes,edges,by.x="id",by.y="from")
    colnames(nodes_edges1)<-c("from_id" ,"type" ,   "label_from"  , "edge_id" ,"to_id"    ,  "color" )
    nodes_edges1$rel<-NULL
    nodes_edges1$type<-NULL
    
    nodes_edges2<-merge(nodes,edges,by.x="id",by.y="to")
    colnames(nodes_edges2)<-c("to_id" ,"type" ,   "label_to"  , "edge_id" ,"from_id"    ,  "col" )
    nodes_edges2$rel<-NULL
    nodes_edges2$type<-NULL
    
    new_nodes_edges<-merge(nodes_edges1,nodes_edges2,by="edge_id")
    nodes_edges<-new_nodes_edges[,c(1,2,3,4,7)]
    colnames(nodes_edges)<-c("edge_id","from_id","label_from","to_id","label_to"  )
    
    node_list_from<-unique(nodes_edges[,c("from_id","label_from")])
    colnames(node_list_from)<-c("id","label")
    node_list_to<-unique(nodes_edges[,c("to_id","label_to")])
    colnames(node_list_to)<-c("id","label")
    node_list<-rbind(node_list_from,node_list_to)
    
    node_list<-as.data.table(node_list)
    node_list[,value:=0.8]
    
    if(nrow(nodes)==1){
      node_list<-nodes 
      node_list$type<-NULL
      node_list<-as.data.table(node_list)
      node_list[,fillcolor:= ifelse(label %in%  marker_table[celltype_species %in% input_celltype]$celltype, "#FF8C69","#E1E1E1")]
      node_list[,value:=0.8]
      
    }
    if(length(input_celltype)>=1 & nrow(descendant_table) != 0){
      node_list[,fillcolor:= ifelse(label %in% marker_table[celltype_species %in% input_celltype]$celltype, "#FF8C69",
                                    ifelse(label %in% descendant_table$cell_type,"#FFDB58","#E1E1E1"))]
    }
    
    # node_list<-node_list[, color:= ifelse(label %in%  marker_table[celltype_species %in% input$descendantsof]$celltype, "#7B1B02",
    #                                       ifelse(label %in% marker_table[celltype_species %in% input$celltype]$celltype, "#FF8C69","#E1E1E1"))]
    
    node_list<-node_list[, fontcolor:="black"]
    
    if(cellid==TRUE){
      node_list<-merge(node_list,ontology_celltype,by.x="label",by.y="cell_type")
      node_list<-as.data.table(node_list)
      node_list<-node_list[, label:= paste0(label," ", cell_ID)]
      
    }
    node_list<-unique(node_list)
    
    node_list<-node_list[, label:=str_replace_all(label, " ", "\n")]
    
    
    edge_list<-nodes_edges[,c("from_id","to_id")]
    #assign color
    edge_list<-as.data.table(edge_list)
    edge_list[,color:="#4F4F4F"]
    # Create the graph object
    if(nrow(nodes)==1){
      i_graph_1 <- create_graph()
      
      # It will start off as empty
      i_graph_1 %>% is_graph_empty()
      # Add the nodes to the graph
      i_graph_2 <-
        i_graph_1 %>%
        add_nodes_from_table(
          table = node_list,
          label_col = label,
        ) %>%
        set_node_attrs(
          node_attr = shape,
          value = "circle"
        ) %>%
        set_node_attrs(
          node_attr = width,
          value = 0.2
        ) %>%
        set_node_attrs(
          node_attr = height,
          value = 0.2
        ) %>%
        set_node_attrs(
          node_attr = fixedsize,
          value = "shape"
        ) %>%
        set_node_attrs(
          node_attr = fontsize,
          value = 3
        ) %>%
        set_node_attrs(
          node_attr = fontname,
          value = "URWTimes"
        )
      
      
      
      return(render_graph(i_graph_2))
    }
    else{
      i_graph_1 <- create_graph()
      
      # It will start off as empty
      i_graph_1 %>% is_graph_empty()
      # Add the nodes to the graph
      i_graph_2 <-
        i_graph_1 %>%
        add_nodes_from_table(
          table = node_list,
          label_col = label,
        ) %>% 
        add_edges_from_table(
          table = edge_list,
          from_col = from_id,
          to_col = to_id,
          from_to_map = id_external
        ) %>%
        
        set_node_attrs(
          node_attr = shape,
          value = "circle"
        ) %>%
        set_node_attrs(
          node_attr = fixedsize,
          value = "shape"
        ) %>%
        # set_node_attrs(
        #   node_attr = height,
        #   value = 1
        # ) %>%
        set_node_attrs(
          node_attr = fontsize,
          value = 9
        ) %>%
        set_node_attrs(
          node_attr = fontname,
          value = "URWTimes"
        ) %>%
        set_edge_attrs(
          edge_attr = arrowsize,
          value = 0.5
        ) %>%
        set_edge_attrs(
          edge_attr = penwidth,
          value = 1
        )  #FF0000=red , #008000 green  ,
      
      i_graph_tree<-render_graph(i_graph_2,layout = "tree",output = "graph",width=150,height=1000)
      
      return(i_graph_tree)  
    }
    
  }
}


hierac_plot2<-function(marker_table, ontology_celltype, graphnel_plot, table_complete_input,cellid) {
  if (nrow(table_complete_input)>=1) {
    onto_igraph<-graph_from_graphnel(graphnel_plot, name = TRUE, weight = TRUE, unlist.attrs = TRUE)
    V(onto_igraph)$label = V(onto_igraph)$name
    V(onto_igraph)$name = factor(V(onto_igraph)$name, levels=as.character(V(onto_igraph)$name))
    mygraph2 <- from_igraph(onto_igraph)
    nodes<-mygraph2[["nodes_df"]]
    edges<-mygraph2[["edges_df"]]
    colnames(edges)<-c("edge_id", "from" ,"to"  , "rel" )
    nodes_edges1<-merge(nodes,edges,by.x="id",by.y="from")
    colnames(nodes_edges1)<-c("from_id" ,"type" ,   "label_from"  , "edge_id" ,"to_id"    ,  "color" )
    nodes_edges1$rel<-NULL
    nodes_edges1$type<-NULL
    
    nodes_edges2<-merge(nodes,edges,by.x="id",by.y="to")
    colnames(nodes_edges2)<-c("to_id" ,"type" ,   "label_to"  , "edge_id" ,"from_id"    ,  "col" )
    nodes_edges2$rel<-NULL
    nodes_edges2$type<-NULL
    
    new_nodes_edges<-merge(nodes_edges1,nodes_edges2,by="edge_id")
    nodes_edges<-new_nodes_edges[,c(1,2,3,4,7)]
    colnames(nodes_edges)<-c("edge_id","from_id","label_from","to_id","label_to"  )
    
    node_list_from<-unique(nodes_edges[,c("from_id","label_from")])
    colnames(node_list_from)<-c("id","label")
    node_list_to<-unique(nodes_edges[,c("to_id","label_to")])
    colnames(node_list_to)<-c("id","label")
    node_list<-rbind(node_list_from,node_list_to)
    
    node_list<-as.data.table(node_list)
    node_list[,value:=0.8]
    
    if(nrow(nodes)==1){
      node_list<-nodes 
      node_list$type<-NULL
      node_list<-as.data.table(node_list)
      node_list[,value:=0.8]
      
    }
    node_list[,fillcolor:= ifelse(label %in%  marker_table[celltype %in% unique(table_complete_input$cell_type)]$celltype, "#FF8C69","#E1E1E1")]
    
    # node_list<-node_list[, color:= ifelse(label %in%  marker_table[celltype_species %in% input$descendantsof]$celltype, "#7B1B02",
    #                                       ifelse(label %in% marker_table[celltype_species %in% input$celltype]$celltype, "#FF8C69","#E1E1E1"))]
    
    node_list<-node_list[, fontcolor:="black"]
    
    if(cellid==TRUE){
      node_list<-merge(node_list,ontology_celltype,by.x="label",by.y="cell_type")
      node_list<-as.data.table(node_list)
      node_list<-node_list[, label:= paste0(label," ", cell_ID)]
      
    }
    node_list<-unique(node_list)
    
    node_list<-node_list[, label:=str_replace_all(label, " ", "\n")]
    
    
    edge_list<-nodes_edges[,c("from_id","to_id")]
    #assign color
    edge_list<-as.data.table(edge_list)
    edge_list[,color:="#4F4F4F"]
    # Create the graph object
    if(nrow(nodes)==1){
      i_graph_1 <- create_graph()
      
      # It will start off as empty
      i_graph_1 %>% is_graph_empty()
      # Add the nodes to the graph
      i_graph_2 <-
        i_graph_1 %>%
        add_nodes_from_table(
          table = node_list,
          label_col = label,
        ) %>%
        set_node_attrs(
          node_attr = shape,
          value = "circle"
        ) %>%
        set_node_attrs(
          node_attr = width,
          value = 0.2
        ) %>%
        set_node_attrs(
          node_attr = height,
          value = 0.2
        ) %>%
        set_node_attrs(
          node_attr = fixedsize,
          value = "shape"
        ) %>%
        set_node_attrs(
          node_attr = fontsize,
          value = 3
        ) %>%
        set_node_attrs(
          node_attr = fontname,
          value = "URWTimes"
        )
      
      
      
      return(render_graph(i_graph_2))
    }
    else{
      i_graph_1 <- create_graph()
      
      # It will start off as empty
      i_graph_1 %>% is_graph_empty()
      # Add the nodes to the graph
      i_graph_2 <-
        i_graph_1 %>%
        add_nodes_from_table(
          table = node_list,
          label_col = label,
        ) %>% 
        add_edges_from_table(
          table = edge_list,
          from_col = from_id,
          to_col = to_id,
          from_to_map = id_external
        ) %>%
        
        set_node_attrs(
          node_attr = shape,
          value = "circle"
        ) %>%
        set_node_attrs(
          node_attr = fixedsize,
          value = "shape"
        ) %>%
        # set_node_attrs(
        #   node_attr = height,
        #   value = 1
        # ) %>%
        set_node_attrs(
          node_attr = fontsize,
          value = 9
        ) %>%
        set_node_attrs(
          node_attr = fontname,
          value = "URWTimes"
        ) %>%
        set_edge_attrs(
          edge_attr = arrowsize,
          value = 0.5
        ) %>%
        set_edge_attrs(
          edge_attr = penwidth,
          value = 1
        )  #FF0000=red , #008000 green  ,
      
      #i_graph_2 %>% get_edge_df() #look at the color
      #i_graph_2 %>% get_node_df()
      
      i_graph_tree<-render_graph(i_graph_2,layout = "tree",output = "graph",width=150,height=1000)
      #i_graph_tree<-render_graph(i_graph_2,layout = "tree",output = "graph",  width ="", height = input$height)
      
      return(i_graph_tree)
    }
  }
}


#click_node function ----
click_node1<-function(marker_table, ontology_celltype, graphnel_plot, input_celltype, input_descendant, merged_descendant_table,cellid,ontology_def){
  if (length(input_celltype) >= 1) {
    onto_igraph<-graph_from_graphnel(graphnel_plot, name = TRUE, weight = TRUE, unlist.attrs = TRUE)
    V(onto_igraph)$label = V(onto_igraph)$name
    V(onto_igraph)$name = factor(V(onto_igraph)$name, levels=as.character(V(onto_igraph)$name))
    mygraph2 <- from_igraph(onto_igraph)
    nodes<-mygraph2[["nodes_df"]]
    edges<-mygraph2[["edges_df"]]
    colnames(edges)<-c("edge_id", "from" ,"to"  , "rel" )
    nodes_edges1<-merge(nodes,edges,by.x="id",by.y="from")
    colnames(nodes_edges1)<-c("from_id" ,"type" ,   "label_from"  , "edge_id" ,"to_id"    ,  "color" )
    nodes_edges1$rel<-NULL
    nodes_edges1$type<-NULL
    
    nodes_edges2<-merge(nodes,edges,by.x="id",by.y="to")
    colnames(nodes_edges2)<-c("to_id" ,"type" ,   "label_to"  , "edge_id" ,"from_id"    ,  "col" )
    nodes_edges2$rel<-NULL
    nodes_edges2$type<-NULL
    
    new_nodes_edges<-merge(nodes_edges1,nodes_edges2,by="edge_id")
    nodes_edges<-new_nodes_edges[,c(1,2,3,4,7)]
    colnames(nodes_edges)<-c("edge_id","from_id","label_from","to_id","label_to"  )
    
    i_graph_1 <- create_graph()
    node_list_from<-unique(nodes_edges[,c("from_id","label_from")])
    colnames(node_list_from)<-c("id","label")
    node_list_to<-unique(nodes_edges[,c("to_id","label_to")])
    colnames(node_list_to)<-c("id","label")
    node_list<-rbind(node_list_from,node_list_to)
    node_list<-as.data.table(node_list)
    node_list[,value:=0.8]
    
    
    if(nrow(nodes)==1){
      node_list<-nodes 
      node_list$type<-NULL
      node_list<-as.data.table(node_list)
      node_list[,fillcolor:= ifelse(label %in%  marker_table[celltype_species %in% input_celltype]$celltype, "#FF8C69","#E1E1E1")]
      node_list[,value:=0.8]
      
    }
    
    if(length(input_descendant) != 0){
      node_list[,fillcolor:= ifelse(label %in% marker_table[celltype_species %in% input_celltype]$celltype, "#FF8C69",
                                    ifelse(label %in% merged_descendant_table$cell_type,"#FFDB58","#E1E1E1"))]
    }
    else{
      node_list[,fillcolor:= ifelse(label %in%  marker_table[celltype_species %in% input_celltype]$celltype, "#FF8C69","#E1E1E1")]
    }
    
    # node_list<-node_list[, color:= ifelse(label %in%  marker_table[celltype_species %in% input$descendantsof]$celltype, "#7B1B02",
    #                                       ifelse(label %in% marker_table[celltype_species %in% input$celltype]$celltype, "#FF8C69","#E1E1E1"))]
    
    node_list<-node_list[, fontcolor:="black"]
    
    node_list<-merge(node_list,ontology_def[,c("cell_def","cell_type")],by.x="label",by.y="cell_type")
    node_list<-unique(node_list)
    
    
    if(cellid==TRUE){
      node_list<-merge(node_list,ontology_celltype,by.x="label",by.y="cell_type")
      node_list<-as.data.table(node_list)
      node_list<-node_list[, label:= paste0(label," ", cell_ID)]
      
    }
    node_list<-unique(node_list)
    
    edge_list<-nodes_edges[,c("from_id","to_id")]
    #assign color
    edge_list<-as.data.table(edge_list)
    edge_list[,color:="#4F4F4F"]
    
    
    if(nrow(nodes)==1){
      i_graph_1 <- create_graph()
      
      # It will start off as empty
      i_graph_1 %>% is_graph_empty()
      # Add the nodes to the graph
      i_graph_2 <-
        i_graph_1 %>%
        add_nodes_from_table(
          table = node_list,
          label_col = label,
        ) %>%
        set_node_attrs(
          node_attr = shape,
          value = "circle"
        ) %>%
        set_node_attrs(
          node_attr = fixedsize,
          value = "shape"
        ) %>%
        set_node_attrs(
          node_attr = fontsize,
          value = 9
        ) %>%
        set_node_attrs(
          node_attr = fontname,
          value = "URWTimes"
        )
      
      return(as.data.table(i_graph_2 %>% get_node_df())[,c("label","id_external","cell_def")])
    }
    
    
    else{
      # Create the graph object
      i_graph_1 <- create_graph()
      
      # It will start off as empty
      i_graph_1 %>% is_graph_empty()
      # Add the nodes to the graph
      i_graph_2 <-
        i_graph_1 %>%
        add_nodes_from_table(
          table = node_list,
          label_col = label,
        ) %>% add_edges_from_table(
          table = edge_list,
          from_col = from_id,
          to_col = to_id,
          from_to_map = id_external
        ) %>%
        
        # set_node_attrs(
        #   node_attr = width,
        #   value = 1
        # ) %>%
        set_node_attrs(
          node_attr = shape,
          value = "circle"
        ) %>%
        set_node_attrs(
          node_attr = fixedsize,
          value = "shape"
        ) %>%
        # set_node_attrs(
        #   node_attr = height,
        #   value = 1
        # ) %>%
        set_node_attrs(
          node_attr = fontsize,
          value = 9
        ) %>%
        set_node_attrs(
          node_attr = fontname,
          value = "URWTimes"
        ) %>%
        set_edge_attrs(
          edge_attr = arrowsize,
          value = 0.5
        ) %>%
        set_edge_attrs(
          edge_attr = penwidth,
          value = 1
        )  #FF0000=red , #008000 green  ,
      
      return(as.data.table(i_graph_2 %>% get_node_df())[,c("label","id_external","cell_def")])
    }
  } 
}


click_node2<-function(marker_table, ontology_celltype, graphnel_plot, table_complete_input,cellid, ontology_def){
  
  if (nrow(table_complete_input)>=1) {
    onto_igraph<-graph_from_graphnel(graphnel_plot, name = TRUE, weight = TRUE, unlist.attrs = TRUE)
    V(onto_igraph)$label = V(onto_igraph)$name
    V(onto_igraph)$name = factor(V(onto_igraph)$name, levels=as.character(V(onto_igraph)$name))
    mygraph2 <- from_igraph(onto_igraph)
    nodes<-mygraph2[["nodes_df"]]
    edges<-mygraph2[["edges_df"]]
    colnames(edges)<-c("edge_id", "from" ,"to"  , "rel" )
    nodes_edges1<-merge(nodes,edges,by.x="id",by.y="from")
    colnames(nodes_edges1)<-c("from_id" ,"type" ,   "label_from"  , "edge_id" ,"to_id"    ,  "color" )
    nodes_edges1$rel<-NULL
    nodes_edges1$type<-NULL
    
    nodes_edges2<-merge(nodes,edges,by.x="id",by.y="to")
    colnames(nodes_edges2)<-c("to_id" ,"type" ,   "label_to"  , "edge_id" ,"from_id"    ,  "col" )
    nodes_edges2$rel<-NULL
    nodes_edges2$type<-NULL
    
    new_nodes_edges<-merge(nodes_edges1,nodes_edges2,by="edge_id")
    nodes_edges<-new_nodes_edges[,c(1,2,3,4,7)]
    colnames(nodes_edges)<-c("edge_id","from_id","label_from","to_id","label_to"  )
    
    i_graph_1 <- create_graph()
    node_list_from<-unique(nodes_edges[,c("from_id","label_from")])
    colnames(node_list_from)<-c("id","label")
    node_list_to<-unique(nodes_edges[,c("to_id","label_to")])
    colnames(node_list_to)<-c("id","label")
    node_list<-rbind(node_list_from,node_list_to)
    node_list<-as.data.table(node_list)
    node_list[,value:=0.8]
    
    
    if(nrow(nodes)==1){
      node_list<-nodes 
      node_list$type<-NULL
      node_list<-as.data.table(node_list)
      node_list[,value:=0.8]
      
    }
    node_list[,fillcolor:= ifelse(label %in%  marker_table[celltype %in% unique(table_complete_input$cell_type)]$celltype, "#FF8C69","#E1E1E1")]
    
    
    # node_list<-node_list[, color:= ifelse(label %in%  marker_table[celltype_species %in% input$descendantsof]$celltype, "#7B1B02",
    #                                       ifelse(label %in% marker_table[celltype_species %in% input$celltype]$celltype, "#FF8C69","#E1E1E1"))]
    
    node_list<-node_list[, fontcolor:="black"]
    
    node_list<-merge(node_list,ontology_def[,c("cell_def","cell_type")],by.x="label",by.y="cell_type")
    node_list<-unique(node_list)
    
    
    if(cellid==TRUE){
      node_list<-merge(node_list,ontology_celltype,by.x="label",by.y="cell_type")
      node_list<-as.data.table(node_list)
      node_list<-node_list[, label:= paste0(label," ", cell_ID)]
      
    }
    node_list<-unique(node_list)
    
    edge_list<-nodes_edges[,c("from_id","to_id")]
    #assign color
    edge_list<-as.data.table(edge_list)
    edge_list[,color:="#4F4F4F"]
    
    
    if(nrow(nodes)==1){
      i_graph_1 <- create_graph()
      
      # It will start off as empty
      i_graph_1 %>% is_graph_empty()
      # Add the nodes to the graph
      i_graph_2 <-
        i_graph_1 %>%
        add_nodes_from_table(
          table = node_list,
          label_col = label,
        ) %>%
        set_node_attrs(
          node_attr = shape,
          value = "circle"
        ) %>%
        set_node_attrs(
          node_attr = fixedsize,
          value = "shape"
        ) %>%
        set_node_attrs(
          node_attr = fontsize,
          value = 9
        ) %>%
        set_node_attrs(
          node_attr = fontname,
          value = "URWTimes"
        )
      
      return(as.data.table(i_graph_2 %>% get_node_df())[,c("label","id_external","cell_def")])
    }
    
    
    else{
      # Create the graph object
      i_graph_1 <- create_graph()
      
      # It will start off as empty
      i_graph_1 %>% is_graph_empty()
      # Add the nodes to the graph
      i_graph_2 <-
        i_graph_1 %>%
        add_nodes_from_table(
          table = node_list,
          label_col = label,
        ) %>% add_edges_from_table(
          table = edge_list,
          from_col = from_id,
          to_col = to_id,
          from_to_map = id_external
        ) %>%
        
        # set_node_attrs(
        #   node_attr = width,
        #   value = 1
        # ) %>%
        set_node_attrs(
          node_attr = shape,
          value = "circle"
        ) %>%
        set_node_attrs(
          node_attr = fixedsize,
          value = "shape"
        ) %>%
        # set_node_attrs(
        #   node_attr = height,
        #   value = 1
        # ) %>%
        set_node_attrs(
          node_attr = fontsize,
          value = 9
        ) %>%
        set_node_attrs(
          node_attr = fontname,
          value = "URWTimes"
        ) %>%
        set_edge_attrs(
          edge_attr = arrowsize,
          value = 0.5
        ) %>%
        set_edge_attrs(
          edge_attr = penwidth,
          value = 1
        )  #FF0000=red , #008000 green  ,
      
      return(as.data.table(i_graph_2 %>% get_node_df())[,c("label","id_external","cell_def")])
    }
  }  
}
#select descendant ----
select_descendant<-function(onto_plot, marker_table, input_descendant){
  onto_igraph<-graph_from_graphnel(onto_plot, name = TRUE, weight = TRUE, unlist.attrs = TRUE)
  node<-as.data.table(unique(marker_table[celltype_species %in% (input_descendant)]$celltype))
  distan<-max(eccentricity(onto_igraph, vids = node$V1, mode = c("out")))
  subnetwork <- induced.subgraph(onto_igraph, vids = as.vector(unlist(neighborhood(onto_igraph, distan, nodes = node$V1, mode = 'out'))))
  dt_subnodes<-as.data.table(V(subnetwork)$name)
  table_descendant<-marker_table[celltype %in% dt_subnodes$V1]
  table_descendant<-table_descendant[,c("celltype_species","celltype","cell_ID","marker","gene_description","marker_type","species","times","specificity","original_celltype.CellMarker",
                                        "original_celltype.PanglaoDB","original_celltype.GeneMarkeR","original_celltype.Azimuth","original_celltype.ASCTB",
                                        "original_celltype.MSigDB","original_celltype.CellTypist","original_celltype.Abcam")]
  colnames(table_descendant)<-c("celltype_species","cell_type","CL_ID","marker","gene_description","marker_type","species","EC_score","database_specificity","original_celltype.CellMarker",
                                "original_celltype.PanglaoDB","original_celltype.GeneMarkeR","original_celltype.Azimuth","original_celltype.ASCTB",
                                "original_celltype.MSigDB","original_celltype.CellTypist","original_celltype.Abcam")
  return(table_descendant)
}
#merge descendats ----
table_merge_descendant<-function(onto_plot,cell_onto, marker_table, input_descendant,input_species){
  onto_igraph<-graph_from_graphnel(onto_plot, name = TRUE, weight = TRUE, unlist.attrs = TRUE)
  node<-as.data.table(unique(marker_table[celltype_species %in% (input_descendant)]$celltype))
  distan<-max(eccentricity(onto_igraph, vids = node$V1, mode = c("out")))
  subnetwork <- induced.subgraph(onto_igraph, vids = as.vector(unlist(neighborhood(onto_igraph, distan, nodes = node$V1, mode = 'out'))))
  dt_subnodes<-as.data.table(V(subnetwork)$name)
  subnodes_id<-as.data.table(unique(marker_table[celltype %in% dt_subnodes$V1]$cell_ID))
  onto_subplot<-onto_plot2(cell_onto, subnodes_id$V1,cex=0.8)
  
  marker_table<-marker_table[celltype %in% dt_subnodes$V1]
  marker_table<-marker_table[,ancestor:= NA]
  marker_table<-marker_table[,-c("times")]
  marker_table<-marker_table[species %in% input_species]
  
  for (i in 1:length(input_descendant)){
    onto_igraph<-graph_from_graphnel(onto_plot, name = TRUE, weight = TRUE, unlist.attrs = TRUE)
    node<-as.data.table(unique(marker_table[celltype_species %in% (input_descendant[[i]])]$celltype))
    distan<-max(eccentricity(onto_igraph, vids = node$V1, mode = c("out")))
    subnetwork <- induced.subgraph(onto_igraph, vids = as.vector(unlist(neighborhood(onto_igraph, distan, nodes = node$V1, mode = 'out'))))
    dt_subnodes<-as.data.table(V(subnetwork)$name)
    marker_table<-marker_table[,ancestor:=ifelse(celltype %in% dt_subnodes$V1,marker_table[celltype_species %in% input_descendant[[i]]]$celltype,ancestor)]
    # re-count the times a marker for a specific cell type appear 
  }
  
  unite_table<- setDT(marker_table[,-c("celltype","celltype_species","cell_ID","gene_description","specificity")])[, lapply(.SD, function(x) sum(!is.na(x))), c("ancestor","marker","species")]
  unite_table[,times:=rowSums(!(unite_table[,c(4:9)]==0))]
  unite_table<-merge(unite_table[,c("ancestor","marker","species","times")],marker_table,by=c("ancestor","marker","species"))
  
  
  unite_table<- unite_table[,c("ancestor","celltype","cell_ID","marker","gene_description","marker_type","species","times","specificity","original_celltype.CellMarker",
                               "original_celltype.PanglaoDB","original_celltype.GeneMarkeR","original_celltype.Azimuth","original_celltype.ASCTB",
                               "original_celltype.MSigDB","original_celltype.CellTypist","original_celltype.Abcam")]
  colnames(unite_table)<-c("cell_type_ancestor","cell_type","CL_ID","marker","gene_description","marker_type","species","EC_score","database_specificity","original_celltype.CellMarker",
                           "original_celltype.PanglaoDB","original_celltype.GeneMarkeR","original_celltype.Azimuth","original_celltype.ASCTB",
                           "original_celltype.MSigDB","original_celltype.CellTypist","original_celltype.Abcam")
  return(unite_table)
  
}

# create table with merged descendant + the other cell types ----
table_merged_desc_other<-function(marker_table,input_celltype,merged_descendant_table,input_species){
  
  table_not_desc<-marker_table[!(celltype %in% merged_descendant_table$cell_type)]
  table_not_desc<-as.data.table(table_not_desc)
  table_not_desc<-table_not_desc[celltype_species %in% input_celltype]
  table_not_desc[,ancestor:=NA]
  table_not_desc<-table_not_desc[,c("ancestor","celltype","cell_ID","marker","gene_description","marker_type","species","times","specificity","original_celltype.CellMarker",
                                    "original_celltype.PanglaoDB","original_celltype.GeneMarkeR","original_celltype.Azimuth","original_celltype.ASCTB",
                                    "original_celltype.MSigDB","original_celltype.CellTypist","original_celltype.Abcam")]
  colnames(table_not_desc)<-c("cell_type_ancestor","cell_type","CL_ID","marker","gene_description","marker_type","species","EC_score","database_specificity","original_celltype.CellMarker",
                              "original_celltype.PanglaoDB","original_celltype.GeneMarkeR","original_celltype.Azimuth","original_celltype.ASCTB",
                              "original_celltype.MSigDB","original_celltype.CellTypist","original_celltype.Abcam")
  combine_table<-rbind(table_not_desc,merged_descendant_table)
  
  #compute specificity 
  combine_table<-as.data.table(combine_table)
  mark_spec<-ddply(combine_table,.(marker),nrow)
  colnames(mark_spec)<-c("marker","query_specificity")
  combine_table<-merge(combine_table,mark_spec,by="marker",all.x = TRUE)
  combine_table[,query_specificity:=format(round(1/query_specificity,2), nsmall=2)]
  
  
  combine_table<-combine_table[,c("cell_type_ancestor","cell_type","CL_ID","marker","gene_description","marker_type","species","EC_score","database_specificity","query_specificity","original_celltype.CellMarker",
                                  "original_celltype.PanglaoDB","original_celltype.GeneMarkeR","original_celltype.Azimuth","original_celltype.ASCTB",
                                  "original_celltype.MSigDB","original_celltype.CellTypist","original_celltype.Abcam")]
  return(combine_table)
  
}

#table with descandant (not merged) and the other cell types ----
table_desc_other<-function(marker_table,input_celltype,descendant_table,input_species){
  all_types<-c(input_celltype,descendant_table$celltype_species)
  table_all<-marker_table[celltype_species %in% all_types & species %in% input_species]
  #compute specificity 
  table_all<-as.data.table(table_all)
  mark_spec<-ddply(table_all,.(marker),nrow)
  colnames(mark_spec)<-c("marker","query_specificity")
  table_all<-merge(table_all,mark_spec,by="marker",all.x = TRUE)
  table_all[,query_specificity:=format(round(1/query_specificity,2), nsmall=2)]
  
  table_all<-table_all[,c("celltype","cell_ID","marker","gene_description","marker_type","species","times","specificity","query_specificity","original_celltype.CellMarker",
                          "original_celltype.PanglaoDB","original_celltype.GeneMarkeR","original_celltype.Azimuth","original_celltype.ASCTB",
                          "original_celltype.MSigDB","original_celltype.CellTypist","original_celltype.Abcam")]
  
  colnames(table_all)<-c("cell_type","CL_ID","marker","gene_description","marker_type","species","EC_score","database_specificity","query_specificity","original_celltype.CellMarker",
                         "original_celltype.PanglaoDB","original_celltype.GeneMarkeR","original_celltype.Azimuth","original_celltype.ASCTB",
                         "original_celltype.MSigDB","original_celltype.CellTypist","original_celltype.Abcam")
  return(table_all)
}

#marker table with only input cell types (no descendant)
table_input_celltypes<-function(marker_table,input_celltype,input_species){
  marker_table_simple<-marker_table[celltype_species %in% input_celltype & species %in% input_species]
  
  #compute specificity 
  mark_spec<-ddply(marker_table_simple,.(marker),nrow)
  colnames(mark_spec)<-c("marker","query_specificity")
  marker_table_simple<-merge(marker_table_simple,mark_spec,by="marker",all.x = TRUE)
  marker_table_simple[,query_specificity:=format(round(1/query_specificity,2), nsmall=2)]
  
  marker_table_simple<-marker_table_simple[,c("celltype","cell_ID","marker","gene_description","marker_type","species","times","specificity","query_specificity","original_celltype.CellMarker",
                                              "original_celltype.PanglaoDB","original_celltype.GeneMarkeR","original_celltype.Azimuth","original_celltype.ASCTB",
                                              "original_celltype.MSigDB","original_celltype.CellTypist","original_celltype.Abcam")]
  colnames(marker_table_simple)<-c("cell_type","CL_ID","marker","gene_description","marker_type","species","EC_score","database_specificity","query_specificity","original_celltype.CellMarker",
                                   "original_celltype.PanglaoDB","original_celltype.GeneMarkeR","original_celltype.Azimuth","original_celltype.ASCTB",
                                   "original_celltype.MSigDB","original_celltype.CellTypist","original_celltype.Abcam")  
  #filter on specificity
  return(marker_table_simple)
}


