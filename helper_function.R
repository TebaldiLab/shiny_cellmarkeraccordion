# hierachies plot function cell types input ----
hierac_plot1<-function(marker_table, graphnel_plot, input_celltype,input_cellid) {
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
      node_list[,fillcolor:= ifelse(label %in%  unique(marker_table[condition !="healthy"]$cell_type), "#FF6A6A", ifelse(label %in%  marker_table[celltype_species %in% input_celltype]$cell_type, "#FF8C69", "#E1E1E1"))]
      
      node_list[,value:=0.8]
      
    }
    if (length(input_celltype)>=1){
      node_list[,fillcolor:= ifelse(label %in%  unique(marker_table[condition !="healthy"]$cell_type), "#FF6A6A", ifelse(label %in%  marker_table[celltype_species %in% input_celltype]$cell_type, "#FF8C69", "#E1E1E1"))]
    }
  
  
    
    node_list<-node_list[, fontcolor:="black"]
    
    if(input_cellid==TRUE){
      node_list<-merge(node_list,marker_table[,c("cell_type","cell_ID")], by.x="label",by.y="cell_type")
      node_list<-as.data.table(node_list)
      node_list<-node_list[, label:= paste0(label," ", cell_ID)]
      
    }
    node_list<-unique(node_list)
    
    id_disease<-node_list[label %in% unique(marker_table[condition !="healthy"]$cell_type)]$id
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


hierac_plot1_desc<-function(marker_table, graphnel_plot, input_celltype,descendant_table,input_cellid) {
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
      node_list[,fillcolor:= ifelse(label %in%  unique(marker_table[condition !="healthy"]$cell_type), "#FF6A6A", ifelse(label %in%  marker_table[celltype_species %in% input_celltype]$cell_type, "#FF8C69", "#E1E1E1"))]
      
      node_list[,value:=0.8]
      
    }
    if(length(input_celltype)>=1 & nrow(descendant_table) != 0){
      node_list[,fillcolor:= ifelse(label %in%  unique(marker_table[condition !="healthy"]$cell_type), "#FF6A6A", ifelse(marker_table[celltype_species %in% input_celltype]$cell_type, "#FF8C69",
                                    ifelse(label %in% descendant_table$cell_type,"#FFDB58","#E1E1E1")))]
    }
    
    # node_list<-node_list[, color:= ifelse(label %in%  marker_table[celltype_species %in% input$descendantsof]$cell_type, "#7B1B02",
    #                                       ifelse(label %in% marker_table[celltype_species %in% input$celltype]$cell_type, "#FF8C69","#E1E1E1"))]
    
    node_list<-node_list[, fontcolor:="black"]
    
    if(input_cellid==TRUE){
      node_list<-merge(node_list,marker_table[,c("cell_type","cell_ID")], by.x="label",by.y="cell_type")
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


hierac_plot2<-function(marker_table, graphnel_plot, table_complete_input,input_cellid) {
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
      node_list[,fillcolor:= ifelse(label %in%  unique(marker_table[condition !="healthy"]$cell_type), "#FF6A6A", ifelse(label %in%  unique(marker_table$cell_type), "#FF8C69", "#E1E1E1"))]
      
      node_list[,value:=0.8]
      
    }
    if (nrow(nodes)>1){
      node_list[,fillcolor:= ifelse(label %in%  unique(marker_table[condition !="healthy"]$cell_type), "#FF6A6A", ifelse(label %in%  unique(marker_table$cell_type), "#FF8C69", "#E1E1E1"))]
    }
    
    
    node_list<-node_list[, fontcolor:="black"]
    
    if(input_cellid==TRUE){
      node_list<-merge(node_list,marker_table[,c("cell_type","cell_ID")], by.x="label",by.y="cell_type")
      node_list<-as.data.table(node_list)
      node_list<-node_list[, label:= paste0(label," ", cell_ID)]
      
    }
    #assign color
    node_list<-unique(node_list)
    
    id_disease<-node_list[label %in% unique(marker_table[condition !="healthy"]$cell_type)]$id
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
click_node1<-function(marker_table, graphnel_plot, input_celltype, input_descendant, merged_descendant_table,input_cellid){
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
      node_list[,fillcolor:= ifelse(label %in%  unique(marker_table[condition !="healthy"]$cell_type), "#FF6A6A", ifelse(label %in%  marker_table[celltype_species %in% input_celltype]$cell_type, "#FF8C69", "#E1E1E1"))]
      
      node_list[,value:=0.8]
      
    }
    if (length(input_celltype)>=1){
      node_list[,fillcolor:= ifelse(label %in%  unique(marker_table[condition !="healthy"]$cell_type), "#FF6A6A", ifelse(label %in%  marker_table[celltype_species %in% input_celltype]$cell_type, "#FF8C69", "#E1E1E1"))]
    }
    
    if(length(input_descendant) != 0){
      node_list[,fillcolor:= ifelse(label %in%  unique(marker_table[condition !="healthy"]$cell_type), "#FF6A6A", ifelse(marker_table[celltype_species %in% input_celltype]$cell_type, "#FF8C69",
                                    ifelse(label %in% merged_descendant_table$cell_type,"#FFDB58","#E1E1E1")))]
    }
    
    
    
    
    node_list<-node_list[, fontcolor:="black"]
    
    node_list<-merge(node_list,marker_table[,c("cell_type","cell_def")],by.x="label",by.y="cell_type")
    node_list<-unique(node_list)
    
    
    if(input_cellid==TRUE){
      node_list<-merge(node_list,marker_table[,c("cell_type","cell_ID")], by.x="label",by.y="cell_type")
      node_list<-as.data.table(node_list)
      node_list<-node_list[, label:= paste0(label," ", cell_ID)]
      
    }
    node_list<-unique(node_list)
    
    id_disease<-node_list[label %in% unique(marker_table[condition !="healthy"]$cell_type)]$id
    node_list<-node_list[, label:=str_replace_all(label, " ", "\n")]
    
    edge_list<-nodes_edges[,c("from_id","to_id")]
    #assign color
    edge_list<-as.data.table(edge_list)
    edge_list[,color:= ifelse(to_id %in% id_disease, "#FF6A6A","#4F4F4F")]
    edge_list[,style := ifelse(to_id %in% id_disease, "dashed","solid")]
    
    
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
    node_list[,fillcolor:= ifelse(label %in%  marker_table[cell_type %in% unique(table_complete_input$cell_type)]$cell_type, "#FF8C69","#E1E1E1")]
    
    
    # node_list<-node_list[, color:= ifelse(label %in%  marker_table[celltype_species %in% input$descendantsof]$cell_type, "#7B1B02",
    #                                       ifelse(label %in% marker_table[celltype_species %in% input$celltype]$cell_type, "#FF8C69","#E1E1E1"))]
    
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
          value = "URWEC_score"
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
      
      return(as.data.table(i_graph_2 %>% get_node_df())[,c("label","id_external","cell_def")])
    }
  }  
}
#select descendant ----
select_descendant<-function(onto_plot, marker_table, input_descendant, col_name){
  onto_igraph<-graph_from_graphnel(onto_plot, name = TRUE, weight = TRUE, unlist.attrs = TRUE)
  node<-as.data.table(unique(marker_table[celltype_species %in% (input_descendant)]$cell_type))
  distan<-max(eccentricity(onto_igraph, vids = node$V1, mode = c("out")))
  subnetwork <- induced.subgraph(onto_igraph, vids = as.vector(unlist(neighborhood(onto_igraph, distan, nodes = node$V1, mode = 'out'))))
  dt_subnodes<-as.data.table(V(subnetwork)$name)
  table_descendant<-marker_table[cell_type %in% dt_subnodes$V1]
  
  table_descendant<-table_descendant[, ..col_name]
  return(table_descendant)
}
#merge descendats ----
table_merge_descendant<-function(onto_plot,cell_onto, marker_table, input_descendant,input_species,col_name){
  onto_igraph<-graph_from_graphnel(onto_plot, name = TRUE, weight = TRUE, unlist.attrs = TRUE)
  node<-as.data.table(unique(marker_table[celltype_species %in% (input_descendant)]$cell_type))
  distan<-max(eccentricity(onto_igraph, vids = node$V1, mode = c("out")))
  subnetwork <- induced.subgraph(onto_igraph, vids = as.vector(unlist(neighborhood(onto_igraph, distan, nodes = node$V1, mode = 'out'))))
  dt_subnodes<-as.data.table(V(subnetwork)$name)
  subnodes_id<-as.data.table(unique(marker_table[cell_type %in% dt_subnodes$V1]$cell_ID))
  onto_subplot<-onto_plot2(cell_onto, subnodes_id$V1,cex=0.8)
  
  marker_table<-marker_table[cell_type %in% dt_subnodes$V1]
  marker_table<-marker_table[,cell_type_ancestor:= NA]
  marker_table<-marker_table[,-c("EC_score")]
  marker_table<-marker_table[species %in% input_species]
  
  for (i in 1:length(input_descendant)){
    onto_igraph<-graph_from_graphnel(onto_plot, name = TRUE, weight = TRUE, unlist.attrs = TRUE)
    node<-as.data.table(unique(marker_table[celltype_species %in% (input_descendant[[i]])]$cell_type))
    distan<-max(eccentricity(onto_igraph, vids = node$V1, mode = c("out")))
    subnetwork <- induced.subgraph(onto_igraph, vids = as.vector(unlist(neighborhood(onto_igraph, distan, nodes = node$V1, mode = 'out'))))
    dt_subnodes<-as.data.table(V(subnetwork)$name)
    marker_table<-marker_table[,cell_type_ancestor:=ifelse(cell_type %in% dt_subnodes$V1,marker_table[celltype_species %in% input_descendant[[i]]]$cell_type,cell_type_ancestor)]
    # re-count the EC_score a marker for a specific cell type appear 
  }
  
  unite_table<- setDT(marker_table[,-c("disease_id","cell_type","celltype_species","cell_ID","cell_def","gene_description","database_specificity")])[, lapply(.SD, function(x) sum(!is.na(x))), c("cell_type_ancestor","marker","species")]
  unite_table[,EC_score:=rowSums(!(unite_table[,c(4:9)]==0))]
  unite_table<-merge(unite_table[,c("cell_type_ancestor","marker","species","EC_score")],marker_table,by=c("cell_type_ancestor","marker","species"))
  col_sel<-c("disease_type","cell_type_ancestor",col_name[ !col_name =="disease_type"])  
  unite_table<- unite_table[, ..col_sel]

  return(unite_table)
  
}

# create table with merged descendant + the other cell types ----
table_merged_desc_other<-function(marker_table,input_celltype,merged_descendant_table,input_species,col_name){
  
  table_not_desc<-marker_table[!(cell_type %in% merged_descendant_table$cell_type)]
  table_not_desc<-as.data.table(table_not_desc)
  table_not_desc<-table_not_desc[celltype_species %in% input_celltype]
  table_not_desc[,cell_type_ancestor:=NA]
  col_sel<-c("disease_type","cell_type_ancestor",col_name[ !col_name =="disease_type"])  
  table_not_desc<- table_not_desc[, ..col_sel]

  
  combine_table<-rbind(table_not_desc,merged_descendant_table)
  
  #compute specificity 
  combine_table<-as.data.table(combine_table)
  mark_spec<-ddply(combine_table,.(marker),nrow)
  colnames(mark_spec)<-c("marker","query_specificity")
  combine_table<-merge(combine_table,mark_spec,by="marker",all.x = TRUE)
  combine_table[,query_specificity:=format(round(1/query_specificity,2), nsmall=2)]
  col_sel<-c("disease_type","cell_type_ancestor",col_name[ !col_name =="disease_type"])  
  
  combine_table<- combine_table[, ..col_sel]
  
  return(combine_table)
  
}

#table with descandant (not merged) and the other cell types ----
table_desc_other<-function(marker_table,input_celltype,descendant_table,input_species,col_name){
  all_types<-c(input_celltype,descendant_table$celltype_species)
  table_all<-marker_table[celltype_species %in% all_types & species %in% input_species]
  #compute specificity 
  table_all<-as.data.table(table_all)
  mark_spec<-ddply(table_all,.(marker),nrow)
  colnames(mark_spec)<-c("marker","query_specificity")
  table_all<-merge(table_all,mark_spec,by="marker",all.x = TRUE)
  table_all[,query_specificity:=format(round(1/query_specificity,2), nsmall=2)]
  col_sel<-c(col_name,"query_specificity")
  table_all<- table_all[, ..col_sel]
  
  return(table_all)
}

#marker table with only input cell types (no descendant)
table_input_celltypes<-function(marker_table,input_celltype,input_species,col_name){
  marker_table_simple<-marker_table[celltype_species %in% input_celltype & species %in% input_species]
  
  #compute specificity 
  mark_spec<-ddply(marker_table_simple,.(marker),nrow)
  colnames(mark_spec)<-c("marker","query_specificity")
  marker_table_simple<-merge(marker_table_simple,mark_spec,by="marker",all.x = TRUE)
  marker_table_simple[,query_specificity:=format(round(1/query_specificity,2), nsmall=2)]
  col_sel<-c(col_name,"query_specificity")
  marker_table_simple<- marker_table_simple[, ..col_sel]
  
  #filter on specificity
  return(marker_table_simple)
}


