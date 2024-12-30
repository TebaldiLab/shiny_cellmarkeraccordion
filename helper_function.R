# hierachies plot function cell types input ----
hierac_plot1<-function(marker_table, graphnel_plot, input_celltype,input_cellid, input_disease) {
  if (length(input_celltype) >= 1 & "healthy" %in% input_disease) {
    onto_igraph<-graph_from_graphnel(graphnel_plot, name = TRUE, weight = TRUE, unlist.attrs = TRUE)
    V(onto_igraph)$label = V(onto_igraph)$name
    V(onto_igraph)$name = factor(V(onto_igraph)$name, levels=as.character(V(onto_igraph)$name))
    #mygraph2 <- from_igraph(onto_igraph)
    mygraph2 <- from_adj_matrix(as.matrix(get.adjacency(onto_igraph)), mode = "directed")
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
    colnames(nodes_edges)<-c("edge_id","from_id","label_from","to_id","label_to")
    
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
      node_list[,fillcolor:= ifelse(label %in%  unique(marker_table[DO_diseasetype !="healthy"]$celltype), "#FF6A6A", ifelse(label %in%  marker_table[celltype_ID %in% input_celltype]$celltype, "#FF8C69", "#E1E1E1"))]
      
      node_list[,value:=0.8]
      
    }
    if (length(input_celltype)>=1){
      node_list[,fillcolor:= ifelse(label %in%  unique(marker_table[DO_diseasetype !="healthy"]$celltype), "#FF6A6A", ifelse(label %in%  marker_table[celltype_ID %in% input_celltype]$celltype, "#FF8C69", "#E1E1E1"))]
    }
  
  
    
    node_list<-node_list[, fontcolor:="black"]
    
    if(input_cellid==TRUE){
      node_list<-merge(node_list,marker_table[,c("celltype","celltype_ID")], by.x="label",by.y="celltype")
      node_list<-as.data.table(node_list)
      node_list<-node_list[, label:= paste0(label," ", celltype_ID)]
      
    }
    node_list<-unique(node_list)
    
    id_disease<-node_list[label %in% unique(marker_table[DO_diseasetype !="healthy"]$celltype)]$id
    node_list<-node_list[, label:=str_replace_all(label, " ", "\n")]
    
    edge_list<-nodes_edges[,c("from_id","to_id")]
    #assign color
    edge_list<-as.data.table(edge_list)
    edge_list[,color:= ifelse(to_id %in% id_disease, "#FF6A6A","#4F4F4F")]
    edge_list[,style := ifelse(to_id %in% id_disease, "dashed","solid")]
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
          value = "URWEC_score"
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
          value = "URWEC_score"
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


hierac_plot1_desc<-function(marker_table, graphnel_plot, input_celltype,descendant_table,input_cellid, input_disease) {
  if (length(input_celltype) >= 1 & "healthy" %in% input_disease) {
    onto_igraph<-graph_from_graphnel(graphnel_plot, name = TRUE, weight = TRUE, unlist.attrs = TRUE)
    V(onto_igraph)$label = V(onto_igraph)$name
    V(onto_igraph)$name = factor(V(onto_igraph)$name, levels=as.character(V(onto_igraph)$name))
    #mygraph2 <- from_igraph(onto_igraph)
    mygraph2 <- from_adj_matrix(as.matrix(get.adjacency(onto_igraph)), mode = "directed")
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
      #node_list[,fillcolor:= ifelse(label %in%  unique(marker_table[DO_diseasetype !="healthy"]$celltype), "#FF6A6A", ifelse(label %in%  marker_table[celltype_species %in% input_celltype]$celltype, "#FF8C69", "#E1E1E1"))]
      node_list[,value:=0.8]
      
    }
       # node_list[,fillcolor:= ifelse(label %in%  unique(marker_table[DO_diseasetype !="healthy"]$celltype), "#FF6A6A", ifelse(marker_table[celltype_species %in% input_celltype]$celltype, "#FF8C69",
       #                               ifelse(label %in% unique(descendant_table$celltype),"#FFDB58","#E1E1E1")))]
    
      node_list[,fillcolor:= ifelse(label %in%  unique(marker_table[DO_diseasetype !="healthy"]$celltype), "#FF6A6A", ifelse(label %in%  marker_table[celltype_ID %in% input_celltype]$celltype, "#FF8C69", "#E1E1E1"))]
      node_list[,fillcolor:= ifelse(!(label %in%  marker_table[celltype_ID %in% input_celltype]$celltype) & label %in% unique(descendant_table$celltype), "#FFDB58", fillcolor)]
      node_list<-node_list[, fontcolor:="black"]
    
    if(input_cellid==TRUE){
      node_list<-merge(node_list,marker_table[,c("celltype","celltype_ID")], by.x="label",by.y="celltype")
      node_list<-as.data.table(node_list)
      node_list<-node_list[, label:= paste0(label," ", celltype_ID)]
      
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
          value = "URWEC_score"
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
          value = "URWEC_score"
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


hierac_plot2<-function(marker_table, graphnel_plot, table_complete_input,input_cellid, input_disease) {
  if (nrow(table_complete_input)>=1 &  "healthy" %in% input_disease) {
    onto_igraph<-graph_from_graphnel(graphnel_plot, name = TRUE, weight = TRUE, unlist.attrs = TRUE)
    V(onto_igraph)$label = V(onto_igraph)$name
    V(onto_igraph)$name = factor(V(onto_igraph)$name, levels=as.character(V(onto_igraph)$name))
    #mygraph2 <- from_igraph(onto_igraph)
    mygraph2 <- from_adj_matrix(as.matrix(get.adjacency(onto_igraph)), mode = "directed")
    
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
      node_list[,fillcolor:= ifelse(label %in%  unique(marker_table[DO_diseasetype !="healthy"]$celltype), "#FF6A6A", ifelse(label %in%  unique(marker_table$celltype), "#FF8C69", "#E1E1E1"))]
      
      node_list[,value:=0.8]
      
    }
    if (nrow(nodes)>1){
      node_list[,fillcolor:= ifelse(label %in%  unique(marker_table[DO_diseasetype !="healthy"]$celltype), "#FF6A6A", ifelse(label %in%  unique(marker_table$celltype), "#FF8C69", "#E1E1E1"))]
    }
    
    
    node_list<-node_list[, fontcolor:="black"]
    
    if(input_cellid==TRUE){
      node_list<-merge(node_list,marker_table[,c("celltype","celltype_ID")], by.x="label",by.y="celltype")
      node_list<-as.data.table(node_list)
      node_list<-node_list[, label:= paste0(label," ", celltype_ID)]
      
    }
    #assign color
    node_list<-unique(node_list)
    
    id_disease<-node_list[label %in% unique(marker_table[DO_diseasetype !="healthy"]$celltype)]$id
    node_list<-node_list[, label:=str_replace_all(label, " ", "\n")]
    
    edge_list<-nodes_edges[,c("from_id","to_id")]
    #assign color
    edge_list<-as.data.table(edge_list)
    edge_list[,color:= ifelse(to_id %in% id_disease, "#FF6A6A","#4F4F4F")]
    edge_list[,style := ifelse(to_id %in% id_disease, "dashed","solid")]
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
          value = "URWEC_score"
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
          value = "URWEC_score"
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

click_node<-function(marker_table, ontology_celltype, graphnel_plot, table_complete_input,cellid, ontology_def, input_disease){
  
  if (nrow(table_complete_input)>=1 & "healthy" %in% input_disease) {
    onto_igraph<-graph_from_graphnel(graphnel_plot, name = TRUE, weight = TRUE, unlist.attrs = TRUE)
    V(onto_igraph)$label = V(onto_igraph)$name
    V(onto_igraph)$name = factor(V(onto_igraph)$name, levels=as.character(V(onto_igraph)$name))
    #mygraph2 <- from_igraph(onto_igraph)
    mygraph2 <- from_adj_matrix(as.matrix(get.adjacency(onto_igraph)), mode = "directed")
    
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
    node_list[,fillcolor:= ifelse(label %in%  marker_table[celltype %in% unique(table_complete_input$celltype)]$celltype, "#FF8C69","#E1E1E1")]
    
    
    # node_list<-node_list[, color:= ifelse(label %in%  marker_table[celltype_species %in% input$descendantsof]$celltype, "#7B1B02",
    #                                       ifelse(label %in% marker_table[celltype_species %in% input$celltype]$celltype, "#FF8C69","#E1E1E1"))]
    
    node_list<-node_list[, fontcolor:="black"]
    
    node_list<-merge(node_list,ontology_def[,c("cell_definition","celltype")],by.x="label",by.y="celltype")
    node_list<-unique(node_list)
    
    
    if(cellid==TRUE){
      node_list<-merge(node_list,ontology_celltype,by.x="label",by.y="celltype")
      node_list<-as.data.table(node_list)
      node_list<-node_list[, label:= paste0(label," ", celltype_ID)]
      
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
          value = "URWEC_score"
        )
      
      return(as.data.table(i_graph_2 %>% get_node_df())[,c("label","id_external","cell_definition")])
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
          value = "URWEC_score"
        ) %>%
        set_edge_attrs(
          edge_attr = arrowsize,
          value = 0.5
        ) %>%
        set_edge_attrs(
          edge_attr = penwidth,
          value = 1
        )  #FF0000=red , #008000 green  ,
      
      return(as.data.table(i_graph_2 %>% get_node_df())[,c("label","id_external","cell_definition")])
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
  return(table_descendant)
}
#merge descendats ----
table_merge_descendant<-function(onto_plot,cell_onto, marker_table, input_descendant,input_species,col_name){
  onto_igraph<-graph_from_graphnel(onto_plot, name = TRUE, weight = TRUE, unlist.attrs = TRUE)
  node<-as.data.table(unique(marker_table[celltype_species %in% (input_descendant)]$celltype))
  distan<-max(eccentricity(onto_igraph, vids = node$V1, mode = c("out")))
  subnetwork <- induced.subgraph(onto_igraph, vids = as.vector(unlist(neighborhood(onto_igraph, distan, nodes = node$V1, mode = 'out'))))
  dt_subnodes<-as.data.table(V(subnetwork)$name)
  subnodes_id<-as.data.table(unique(marker_table[celltype %in% dt_subnodes$V1]$celltype_ID))
  onto_subplot<-onto_plot2(cell_onto, subnodes_id$V1,cex=0.8)
  
  marker_table<-marker_table[celltype %in% dt_subnodes$V1]
  marker_table<-marker_table[,celltype_ancestor:= NA]
  marker_table<-marker_table[,-c("EC_score")]
  marker_table<-marker_table[species %in% input_species]
  
  for (i in 1:length(input_descendant)){
    onto_igraph<-graph_from_graphnel(onto_plot, name = TRUE, weight = TRUE, unlist.attrs = TRUE)
    node<-as.data.table(unique(marker_table[celltype_species %in% (input_descendant[[i]])]$celltype))
    distan<-max(eccentricity(onto_igraph, vids = node$V1, mode = c("out")))
    subnetwork <- induced.subgraph(onto_igraph, vids = as.vector(unlist(neighborhood(onto_igraph, distan, nodes = node$V1, mode = 'out'))))
    dt_subnodes<-as.data.table(V(subnetwork)$name)
    marker_table<-marker_table[,celltype_ancestor:=ifelse(celltype %in% dt_subnodes$V1,marker_table[celltype_species %in% input_descendant[[i]]]$celltype,celltype_ancestor)]
    # re-count the EC_score a marker for a specific cell type appear 
  }
  
  unite_table<- setDT(marker_table[,-c("DO_ID","celltype","celltype_species","celltype_ID","cell_definition","gene_description","specificity_score")])[, lapply(.SD, function(x) sum(!is.na(x))), c("celltype_ancestor","marker","species")]
  unite_table[,EC_score:=rowSums(!(unite_table[,c(4:9)]==0))]
  unite_table<-merge(unite_table[,c("celltype_ancestor","marker","species","EC_score")],marker_table,by=c("celltype_ancestor","marker","species"))
  col_sel<-c("DO_diseasetype","celltype_ancestor",col_name[ !col_name =="DO_diseasetype"])  
  unite_table<- unite_table[, ..col_sel]

  return(unite_table)
  
}

# create table with merged descendant + the other cell types ----
table_merged_desc_other<-function(marker_table,input_celltype,merged_descendant_table,input_species,col_name){
  
  table_not_desc<-marker_table[!(celltype %in% merged_descendant_table$celltype)]
  table_not_desc<-as.data.table(table_not_desc)
  table_not_desc<-table_not_desc[celltype_species %in% input_celltype]
  table_not_desc[,celltype_ancestor:=NA]
  col_sel<-c("DO_diseasetype","celltype_ancestor",col_name[!col_name =="DO_diseasetype"])  
  table_not_desc<- table_not_desc[, ..col_sel]
  
  combine_table<-rbind(table_not_desc,merged_descendant_table)
  combine_table<- combine_table[, ..col_sel]
  
  return(combine_table)
  
}

#table with descandant (not merged) and the other cell types ----
table_desc_other<-function(marker_table,input_celltype,descendant_table,input_species,col_name){
  all_types<-c(input_celltype,descendant_table$celltype_species)
  table_all<-marker_table[celltype_species %in% all_types & species %in% input_species]
  table_all<- table_all[, ..col_name]
  
  return(table_all)
}

#marker table with only input cell types (no descendant)
table_input_celltypes<-function(marker_table,input_celltype, input_species, input_tissue, input_disease){
  marker_table_simple<-marker_table[celltype_species %in% input_celltype & species %in% input_species & Uberon_tissue %in% input_tissue & DO_diseasetype %in% input_disease]
  
  #filter on specificity
  return(marker_table_simple)
}

