library("rstudioapi")  
setwd(dirname(getActiveDocumentContext()$path)) 

library(shinydashboard)
library(shiny)
library(stringr)
library(DiagrammeR)
library(igraph)
library(shinyWidgets)
library(ontologyIndex)
library(plyr)
library(data.table)
library(ontologyPlot)
library(ontoProc)
library(writexl)
library(dplyr)
library(shinyBS)
library(shinyhelper)
library(shinydashboardPlus)

source("packages.R")


#marker_table<-read.table("C:/Users/emmab/Desktop/PhD/Datasets_SC/Anno_marker-based/Marker/Original_site/6marker_alltissue_integration_wide_sub.txt",sep='\t',header=TRUE)
marker_table<-read.table("data/Hema_Accordion_wideTable.txt",sep='\t',header=TRUE)
marker_table<-as.data.table(marker_table)

#load ontology 
cell_onto<-get_ontology("data/cl-basic.obo", propagate_relationships = c("is_a","develops_from"), extract_tags = "everything")
onto_plot<-onto_plot2(cell_onto, marker_table$cell_ID)
nodes<-as.data.table(onto_plot@nodes)
nodes<-nodes[,V1:=tstrsplit(nodes$V1,"CL", fixed = TRUE, keep = 1)]
nodes<-nodes[, V1:=str_replace_all(V1,"\n"," ")]
nodes<-nodes[, V1:=str_sub(V1,1,nchar(V1)-1)]
onto_plot@nodes<-nodes$V1

#ontology cell types
ontology_celltype<-as.data.frame(cell_onto[["name"]])
colnames(ontology_celltype)<-"cell_type"
ontology_celltype$cell_ID<-rownames(ontology_celltype)
ontology_celltype<-as.data.table(ontology_celltype)

#definition of cell types
ontology_def<-as.data.frame(cell_onto[["def"]])
colnames(ontology_def)<-"cell_def"
ontology_def$cell_ID<-rownames(ontology_def)
ontology_def<-as.data.table(ontology_def)
ontology_def<-merge(ontology_def,ontology_celltype,by="cell_ID")
ontology_def[,cell_def:=str_remove_all(cell_def, '"')]
ontology_def[,cell_def:=str_remove_all(cell_def, "\\$")]
ontology_def[,cell_def:=tstrsplit(cell_def, "[", fixed = TRUE, keep = 1)]


ui <- dashboardPage(
  dashboardHeader(title = "The Cell Marker Accordion", 
                  
                  disable = FALSE, 
                  titleWidth  = 1000
                  ),
  ## Sidebar content
  dashboardSidebar(width=300,
    sidebarMenu(
      menuItem("Homepage", tabName = "dashboard", icon = icon("home")),
      menuItem("Search by cell types", tabName = "celltype", icon = icon("circle-notch")),
      menuItem("Search by marker genes", tabName = "marker", icon = icon("dna"))
      
    )
  ),
  dashboardBody(tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "styles.css"),
    tags$style("@import url(https://use.fontawesome.com/releases/v6.1.1/css/all.css);")),
    tabItems(
      # First tab content
      tabItem(tabName = "dashboard",
              fluidRow(
                HTML("<h3>The Cell Marker Accordion for Hematopoietic cell types </h3> "),
                HTML("<h2> A Web tool for single-cell and spatial RNA-seq cell types annotation </h2> "),
                div(img(src = "Logo.png"),style="text-align: center;"),
                br(),
                br(),
                tags$style("@import url(https://use.fontawesome.com/releases/v6.1.1/css/all.css);"),
                p(style="text-align: justify;", HTML("<h>A crucial and challenging step in single-cell and spatial data analysis is the annotation of cell types. The Cell Marker Accordion adress the need for robust and reproducible cell type identification through standardization and integration of multiple published gene marker databases. <br> The Cell Marker Accordion web interface allows to easily: </h> "))),

      titlePanel(shiny::span((icon("circle-notch",class = "about-icon fa-pull-left", lib = "font-awesome")), p(style="text-align: justify;", HTML("<h>  Search and download lists of marker genes by cell types. </h>")))),          
      titlePanel(shiny::span((icon("dna",class = "about-icon fa-pull-left", lib = "font-awesome")), p(style="text-align: justify;", HTML("<h>  Search and download lists of cell types by marker genes. </h>")))),                                                                                                              
      titlePanel(shiny::span((icon("sitemap",class = "about-icon fa-pull-left", lib = "font-awesome")), p(style="text-align: justify;", HTML("<h>  Browse hierarchies of cell types following the Cell Ontology structure in order to obtain the desired level of specificity in the markers in both searches options. </h>")))),
      titlePanel(shiny::span((icon("arrow-down-short-wide",class ="about-icon fa-pull-left", lib = "font-awesome")), p(style="text-align: justify;", HTML("<h> Rank and select marker genes by their evidence consistency scores. </h>"))))),                                        
                                                                                                                                              
  
      
      # Second tab content: Search markers according to cell types 
      tabItem(tabName = "celltype",
              div(fluidRow(column(width=6,wellPanel(id="sidebar",
                                                    checkboxGroupInput("species", "Select species:",
                                                                       choiceNames =
                                                                         list(tags$img(src = "human.png"),tags$img(src = "mouse.png")),
                                                                       choiceValues =
                                                                         list("Human","Mouse"),selected = c("Human","Mouse"),inline=TRUE),
                                                    br(),
                                                    pickerInput('celltype', 'Cell type', choices= NULL ,multiple=TRUE, options = list(`actions-box` = TRUE,`live-search`=TRUE)),
                                                    pickerInput('descendantsof', 'See subtypes of:', choices= NULL,multiple=TRUE, options = list(`actions-box` = TRUE,`live-search`=TRUE)),
                                                    checkboxInput("cellid","Plot CL_ID",value=FALSE))),
                           br(),
                           #change style sliderinput
                           tags$style(HTML(".js-irs-0 .irs-single, .js-irs-0 .irs-bar-edge, .js-irs-0 .irs-bar {background: #990000}")),
                           tags$style(HTML(".js-irs-1 .irs-single, .js-irs-1 .irs-bar-edge, .js-irs-1 .irs-bar {background: #990000}")),
                           titlePanel(shiny::span((icon("sliders",class = "about-icon fa-pull-left", lib = "font-awesome")), p(style="text-align: justify;", HTML("<h>Adjust plot size </h>")))),
                           column(width=6,offset=0, sliderInput(inputId = "height", label = "Height", min = 200, max = 6500, value = 400, step = 200,width="80%"),
                                  br(),
                                  sliderInput(inputId = "width", label = "Width", min = 200, max = 6500, value = 400, step=200,width="80%"))),
                  tags$head(tags$style(HTML('
         #sidebar {
            background-color: #ad000019;
            font-size: 16px;
        }'))),
                  #HTML(icon("hand-back-point-up"),"<h4> <strong> Click </strong> on a node to look at cell type description</h4>"),
                  titlePanel(shiny::span((icon("hand-pointer",class = "about-icon fa-pull-left", lib = "font-awesome")), p(style="text-align: justify;", HTML("<h> <strong> Click </strong> on a node to look at cell type description</h>")))),
                  fluidRow(column(12,align="center",div(style='width:100%;overflow-x: scroll;height:100%;overflow-y: scroll;', uiOutput('scalableplot')))),
                  #grVizOutput("plot1"),
                  tags$style(
                    '#test {
    cursor: grap;
    color: black;
    }'),
                  conditionalPanel(id="test",condition= "input.plot1_click",wellPanel(textOutput("cell_type_def"))),
                  br(),
                  br(),

                  conditionalPanel(condition= "input.descendantsof != ''",titlePanel(shiny::span(p(style="text-align: justify;", HTML("<h>Table options</h>"),actionButton('help', 'Info',icon= icon("info"), align="left"))))),
                   conditionalPanel(condition= "input.descendantsof == ''",titlePanel(shiny::span(p(style="text-align: justify;", HTML("<h>Table options</h>"),actionButton('help_empty', 'Info',icon= icon("info"), align="left"))))),
                  
                    fluidRow(column(width=12,wellPanel(id="sidebar2",
                                                    fluidRow(column(2,radioButtons('times','EC_score', c(">=1",">=2",">=3",">=4"),selected = ">=1")),
                                                             column(3,radioButtons('database_spec','database_specificity', c(">=0",">=0.25",">=0.5","=1"),selected = ">=0")),
                                                             column(3,radioButtons('query_spec','query_specificity', c(">=0",">=0.25",">=0.5","=1"),selected = ">=0")),
                                                             column(2,radioButtons("tabletype","Table type",c("Simple","Complete"),selected="Simple")),
                                                             conditionalPanel(condition= "input.descendantsof != ''", column(2,radioButtons("unitedescendant","Merge subtypes", c("Yes","No"),selected="No"))))))),

                  
                  fluidRow(column(4,radioButtons("downloadType", "Download Format", choices = c("CSV" = ".csv",
                                                                                                "XLSX" = ".xlsx",
                                                                                                "TSV" = ".tsv"),inline = TRUE),
                                  column(4,downloadButton("downloadData", "Download")))),
                  
                  tags$head(tags$style(HTML('
         #sidebar2 {
            background-color: #ad000019;
            font-size: 16px;
        }'))),
                  # bsTooltip("times", "Evidence consensus score (number of databases)", placement = "top", trigger = "hover",
                  #           options = NULL),
                  br(),
                    br(),
                    dataTableOutput('table1'),
                    br(),
                    tags$p("References",style = "font-size:25px;"),
                    tags$p(tags$strong("CellMarker"), tags$a("CellMarker: a manually curated resource of cell markers in human and mouse, Nucleic Acids Research 2019",href="https://pubmed.ncbi.nlm.nih.gov/30289549/"),
                           tags$strong("-"),tags$a("Web Application",href = "http://bio-bigdata.hrbmu.edu.cn/CellMarker/")),
                    tags$p(tags$strong("PanglaoDB"), tags$a("PanglaoDB: a web server for exploration of mouse and human single-cell RNA sequencing data, Database 2019",href="https://www.ncbi.nlm.nih.gov/pmc/articles/PMC6450036/"),
                           tags$strong("-"),tags$a("Web Application",href = "https://panglaodb.se/")),
                    tags$p(tags$strong("GeneMarkeR"), tags$a("GeneMarkeR: A Database and User Interface for scRNA-seq Marker Genes, Front. Genet. 2021",href="https://www.ncbi.nlm.nih.gov/pmc/articles/PMC6450036/"),
                           tags$strong("-"),tags$a("Web Application",href = "https://shiny.ph.iu.edu/GeneMarkeR/")),
                    tags$p(tags$strong("Azimuth"), tags$a("Integrated analysis of multimodal single-cell data, Cell 2021",href="https://www.sciencedirect.com/science/article/pii/S0092867421005833"),
                           tags$strong("-"),tags$a("Web Application",href = "https://azimuth.hubmapconsortium.org/")),
                    tags$p(tags$strong("ASCTB"), tags$a("Anatomical structures, cell types and biomarkers of the Human Reference Atlas, Nature Cell Biology 2021",href="https://www.nature.com/articles/s41556-021-00788-6"),
                           tags$strong("-"),tags$a("Web Application",href = "https://hubmapconsortium.github.io/ccf/pages/ccf-anatomical-structures.html")),
                    tags$p(tags$strong("MSigDB"), tags$a("The Molecular Signatures Database Hallmark Gene Set Collection, Cell Systems 2015",href="https://www.cell.com/cell-systems/fulltext/S2405-4712(15)00218-5?_returnURL=https%3A%2F%2Flinkinghub.elsevier.com%2Fretrieve%2Fpii%2FS2405471215002185%3Fshowall%3Dtrue"),
                           tags$strong("-"),tags$a("Web Application",href = "http://www.gsea-msigdb.org/gsea/msigdb/collections.jsp#C8")))),
      
      
      
      
      tabItem(tabName="marker", 
              div(fluidRow(column(width=6,wellPanel(id="sidebar",checkboxGroupInput("speciesM", "Select species:",
                                              choiceNames =
                                                list(tags$img(src = "human.png"),tags$img(src = "mouse.png")),
                                              choiceValues =
                                                list("Human","Mouse"),selected = c("Human","Mouse"),inline=TRUE),
                  br(),
                  textInput("marker", "Insert marker genes", value = "", width = NULL, placeholder = NULL),
                  fileInput("markerfile", "Load text file with marker genes ",buttonLabel=list(icon("upload")),
                            multiple = FALSE),
                  checkboxInput("cellidM","Plot CL_ID",value=FALSE))),
                  br(),
                  #change style sliderinput
                  tags$style(HTML(".js-irs-2 .irs-single, .js-irs-2 .irs-bar-edge, .js-irs-2 .irs-bar {background: #990000}")),
                  tags$style(HTML(".js-irs-3 .irs-single, .js-irs-3 .irs-bar-edge, .js-irs-3 .irs-bar {background: #990000}")),
                  titlePanel(shiny::span((icon("sliders",class = "about-icon fa-pull-left", lib = "font-awesome")), p(style="text-align: justify;", HTML("<h>Adjust plot size </h>")))),
                  column(width=6,offset=0, sliderInput(inputId = "heightM", label = "Height", min = 200, max = 3500, value = 400, step = 200,width="80%"),
                         br(),
                         sliderInput(inputId = "widthM", label = "Width", min = 200, max = 3500, value = 400, step=200,width="80%"))),
                  tags$head(tags$style(HTML('
         #sidebar {
            background-color: #ad000019;
            font-size: 16px;
        }'))),
                  #HTML(icon("hand-back-point-up"),"<h4> <strong> Click </strong> on a node to look at cell type description</h4>"),
                  titlePanel(shiny::span((icon("hand-pointer",class = "about-icon fa-pull-left", lib = "font-awesome")), p(style="text-align: justify;", HTML("<h> <strong> Click </strong> on a node to look at cell type description</h>")))),
                  fluidRow(column(12,align="center",div(style='width:100%;overflow-x: scroll;height:100%;overflow-y: scroll;', uiOutput('scalableplotM')))),
                  #grVizOutput("plot1"),
                  tags$style(
                    '#testM {
    cursor: grap;
    color: black;
    }'),
                  conditionalPanel(id="testM",condition= "input.plot1M_click",wellPanel(textOutput("cell_type_defM"))),
                  br(),
                  br(),
                  titlePanel(shiny::span(p(style="text-align: justify;", HTML("<h>Table options</h>"),actionButton('helpM', 'Info',icon= icon("info"), align="left")))),                  
                  fluidRow(column(width=8,wellPanel(id="sidebar2M",
                                                    fluidRow(column(3,radioButtons('timesM','EC_score', c(">=1",">=2",">=3",">=4"),selected = ">=1")),
                                                             column(3,radioButtons('database_specM','database_specificity', c(">=0",">=0.25",">=0.5","=1"),selected = ">=0")),
                                                             column(3,radioButtons("tabletypeM","Table type",c("Simple","Complete"),selected="Simple")))))),
                  fluidRow(column(4,radioButtons("downloadTypeM", "Download Format", choices = c("CSV" = ".csv",
                                                                                                "XLSX" = ".xlsx",
                                                                                                "TSV" = ".tsv"),inline = TRUE),
                                  column(4,downloadButton("downloadDataM", "Download")))),
                  
                  tags$head(tags$style(HTML('
         #sidebar2M {
            background-color: #ad000019;
            font-size: 16px;
        }'))),
                  # bsTooltip("times", "Evidence consensus score (number of databases)", placement = "top", trigger = "hover",
                  #           options = NULL),
                  br(),
                    br(),
                    dataTableOutput('table1M'),
                    br(),
                    tags$p("References",style = "font-size:25px;"),
                    tags$p(tags$strong("CellMarker"), tags$a("CellMarker: a manually curated resource of cell markers in human and mouse, Nucleic Acids Research 2019",href="https://pubmed.ncbi.nlm.nih.gov/30289549/"),
                           tags$strong("-"),tags$a("Web Application",href = "http://bio-bigdata.hrbmu.edu.cn/CellMarker/")),
                    tags$p(tags$strong("PanglaoDB"), tags$a("PanglaoDB: a web server for exploration of mouse and human single-cell RNA sequencing data, Database 2019",href="https://www.ncbi.nlm.nih.gov/pmc/articles/PMC6450036/"),
                           tags$strong("-"),tags$a("Web Application",href = "https://panglaodb.se/")),
                    tags$p(tags$strong("GeneMarkeR"), tags$a("GeneMarkeR: A Database and User Interface for scRNA-seq Marker Genes, Front. Genet. 2021",href="https://www.ncbi.nlm.nih.gov/pmc/articles/PMC6450036/"),
                           tags$strong("-"),tags$a("Web Application",href = "https://shiny.ph.iu.edu/GeneMarkeR/")),
                    tags$p(tags$strong("Azimuth"), tags$a("Integrated analysis of multimodal single-cell data, Cell 2021",href="https://www.sciencedirect.com/science/article/pii/S0092867421005833"),
                           tags$strong("-"),tags$a("Web Application",href = "https://azimuth.hubmapconsortium.org/")),
                    tags$p(tags$strong("ASCTB"), tags$a("Anatomical structures, cell types and biomarkers of the Human Reference Atlas, Nature Cell Biology 2021",href="https://www.nature.com/articles/s41556-021-00788-6"),
                           tags$strong("-"),tags$a("Web Application",href = "https://hubmapconsortium.github.io/ccf/pages/ccf-anatomical-structures.html")),
                    tags$p(tags$strong("MSigDB"), tags$a("The Molecular Signatures Database Hallmark Gene Set Collection, Cell Systems 2015",href="https://www.cell.com/cell-systems/fulltext/S2405-4712(15)00218-5?_returnURL=https%3A%2F%2Flinkinghub.elsevier.com%2Fretrieve%2Fpii%2FS2405471215002185%3Fshowall%3Dtrue"),
                           tags$strong("-"),tags$a("Web Application",href = "http://www.gsea-msigdb.org/gsea/msigdb/collections.jsp#C8" ))))
      
    )
  )
)
server <- function(input, output, session) {
  
  #server for cell type search 
  
  observeEvent(input$help,{
    showModal(modalDialog(
      title = "Table Option Information",
      HTML("Marker genes can be ranked and selected by: <br>
      <ul><li> <strong> EC_score </strong>: evidence consistency score, measuring the agreement of different annotation sources </li>
       <li> <strong> database specificity </strong>: whether a gene is a marker for different cell types present in all the accordion database </li>
        <li> <strong> query specificity </strong> : whether a gene is a marker for different cell types present in the query input </li></ul>

      In addition:  <br>
      <strong> Merged subtypes </strong>:  if subtypes of at least one input cell type is displayed
      <br> <ul><li> yes: merge together the subtypes gene markers and assign them to selected cell type </li><li> no: merge of subtypes is not performed </li></ul>
      <strong> Table type </strong> <br> <ul><li>simple: retrieves a compact and easy table format</li><li> complete: additional information are added</li></ul>")
      #textInput('text2', 'You can also put UI elements here')
    ))
  })
  
  
  observeEvent(input$help_empty,{
    showModal(modalDialog(
      title = "Table Option Information",
      HTML("Marker genes can be ranked and selected by: <br>
             <ul><li> <strong> EC_score </strong>: evidence consistency score, measuring the agreement of different annotation sources </li>
             <li> <strong> database specificity </strong>: whether a gene is a marker for different cell types present in all the accordion database </li>
             <li> <strong> query specificity </strong> : whether a gene is a marker for different cell types present in the query input </li></ul>
             In addition: <br>
             <strong> Table type </strong> <br> <ul><li>simple: retrieves a compact and easy table format</li><li> complete: additional information are added</li></ul>")
      #textInput('text2', 'You can also put UI elements here')
    ))
  })
  
  observeEvent(input$species,{
    updatePickerInput(session,'celltype', selected=c("hematopoietic stem cell (Hs, Mm)", "hematopoietic multipotent progenitor cell (Hs, Mm)","hematopoietic lineage restricted progenitor cell (Hs, Mm)"),
                      choices=unique(marker_table[species %in% input$species]$celltype_species),
                      option=list(`actions-box` = TRUE,style="box-celltypes"),
                      choicesOpt = list(style = rep(("font-size: 18px; line-height: 1.6;"), 103)))
  })
  
  controlDescendant<-reactive({
    onto_igraph<-graph_from_graphnel(onto_plot, name = TRUE, weight = TRUE, unlist.attrs = TRUE)
    node<-as.data.table(unique(marker_table[celltype_species %in% input$celltype]$celltype))
    dt<- data.table(celltype=character(), distance=numeric())
    for (i in 1:nrow(node)){
      distan<-max(eccentricity(onto_igraph, vids = node[i]$V1, mode = c("out")))
      data<-as.data.table(t(c(node[i]$V1,distan)))
      colnames(data)<-c("celltype","distance")
      dt<-rbind(dt,data)
    }
    as.data.table(dt)
  })
  
  observeEvent(input$celltype,{
    updatePickerInput(session,'descendantsof',
                      choices=unique(marker_table[species %in% input$species & celltype %in% controlDescendant()[distance!=0]$celltype]$celltype_species),
                      option=list(`actions-box` = TRUE,style="box-celltypes"),
                      choicesOpt = list(style = rep(("font-size: 18px; line-height: 1.6;"), 103)))
  })
  
  
  selectedData <- reactive({
    as.data.table(unique(marker_table[celltype_species %in% (input$celltype)]$cell_ID))
  })
  
  selectedDescendant <- reactive({
    onto_igraph<-graph_from_graphnel(onto_plot, name = TRUE, weight = TRUE, unlist.attrs = TRUE)
    node<-as.data.table(unique(marker_table[celltype_species %in% (input$descendantsof)]$celltype))
    distan<-max(eccentricity(onto_igraph, vids = node$V1, mode = c("out")))
    subnetwork <- induced.subgraph(onto_igraph, vids = as.vector(unlist(neighborhood(onto_igraph, distan, nodes = node$V1, mode = 'out'))))
    dt_subnodes<-as.data.table(V(subnetwork)$name)
    as.data.table(unique(marker_table[celltype %in% dt_subnodes$V1]$cell_ID))
  })
  
  selectedBoth <- reactive({
    rbind(selectedData(),selectedDescendant())
    
  })
  
  markerTable <- reactive({
    marker_table_simple<-marker_table[celltype_species %in% (input$celltype) & species %in% input$species & times >= str_replace_all(input$times, ">=","")]

    #compute specificity 
    mark_spec<-ddply(marker_table_simple,.(marker),nrow)
    colnames(mark_spec)<-c("marker","query_specificity")
    marker_table_simple<-merge(marker_table_simple,mark_spec,by="marker",all.x = TRUE)
    marker_table_simple[,query_specificity:=format(round(1/query_specificity,2), nsmall=2)]
    
    marker_table_simple<-marker_table_simple[,c("celltype","cell_ID","marker","gene_description","species","times","specificity","query_specificity","original_celltype.CellMarker",
                                                "original_celltype.PanglaoDB","original_celltype.GeneMarkeR","original_celltype.Azimuth","original_celltype.ASCTB",
                                                "original_celltype.MSigDB")]
    colnames(marker_table_simple)<-c("cell_type","CL_ID","marker","gene_description","species","EC_score","database_specificity","query_specificity","original_celltype.CellMarker",
                                     "original_celltype.PanglaoDB","original_celltype.GeneMarkeR","original_celltype.Azimuth","original_celltype.ASCTB",
                                     "original_celltype.MSigDB")  
    #filter on specificity
    marker_table_simple<-marker_table_simple[database_specificity >= str_replace_all(input$database_spec, ">=|=","") & query_specificity >= str_replace_all(input$query_spec, ">=|=","")]
    marker_table_simple
    })
  
  uniteDescendant <- reactive ({
    onto_igraph<-graph_from_graphnel(onto_plot, name = TRUE, weight = TRUE, unlist.attrs = TRUE)
    node<-as.data.table(unique(marker_table[celltype_species %in% (input$descendantsof)]$celltype))
    distan<-max(eccentricity(onto_igraph, vids = node$V1, mode = c("out")))
    subnetwork <- induced.subgraph(onto_igraph, vids = as.vector(unlist(neighborhood(onto_igraph, distan, nodes = node$V1, mode = 'out'))))
    dt_subnodes<-as.data.table(V(subnetwork)$name)
    subnodes_id<-as.data.table(unique(marker_table[celltype %in% dt_subnodes$V1]$cell_ID))
    onto_subplot<-onto_plot2(cell_onto, subnodes_id$V1,cex=0.8)
    
    marker_table<-marker_table[celltype %in% dt_subnodes$V1]
    marker_table<-marker_table[,ancestor:= NA]
    marker_table<-marker_table[,-c("times")]
    marker_table<-marker_table[species %in% input$species]

    for (i in 1:length(input$descendantsof)){
      onto_igraph<-graph_from_graphnel(onto_plot, name = TRUE, weight = TRUE, unlist.attrs = TRUE)
      node<-as.data.table(unique(marker_table[celltype_species %in% (input$descendantsof[[i]])]$celltype))
      distan<-max(eccentricity(onto_igraph, vids = node$V1, mode = c("out")))
      subnetwork <- induced.subgraph(onto_igraph, vids = as.vector(unlist(neighborhood(onto_igraph, distan, nodes = node$V1, mode = 'out'))))
      dt_subnodes<-as.data.table(V(subnetwork)$name)
      marker_table<-marker_table[,ancestor:=ifelse(celltype %in% dt_subnodes$V1,marker_table[celltype_species %in% input$descendantsof[[i]]]$celltype,ancestor)]
      # re-count the times a marker for a specific cell type appear 
    }
    
    unite_table<- setDT(marker_table[,-c("celltype","celltype_species","cell_ID","gene_description","specificity")])[, lapply(.SD, function(x) sum(!is.na(x))), c("ancestor","marker","species")]
    unite_table[,times:=rowSums(!(unite_table[,c(4:9)]==0))]
    unite_table<-merge(unite_table[,c("ancestor","marker","species","times")],marker_table,by=c("ancestor","marker","species"))
    unite_table<-unite_table[times >= str_replace_all(input$times, ">=","")]

    
    unite_table<- unite_table[,c("ancestor","celltype","cell_ID","marker","gene_description","species","times","specificity","original_celltype.CellMarker",
                                 "original_celltype.PanglaoDB","original_celltype.GeneMarkeR","original_celltype.Azimuth","original_celltype.ASCTB",
                                 "original_celltype.MSigDB")]
    colnames(unite_table)<-c("cell_type_ancestor","cell_type","CL_ID","marker","gene_description","species","EC_score","database_specificity","original_celltype.CellMarker",
                             "original_celltype.PanglaoDB","original_celltype.GeneMarkeR","original_celltype.Azimuth","original_celltype.ASCTB",
                             "original_celltype.MSigDB")
    unite_table

    })
  
  descendantTable <- reactive ({
    type_notdesc<-input$celltype[!(input$celltype %in% input$descendantsof)]
    table_notdesc<-marker_table[celltype_species %in% (type_notdesc) & species %in% input$species & times >= str_replace_all(input$times, ">=","")]
    table_notdesc[,ancestor:=NA]
    
    table_notdesc<-table_notdesc[,c("ancestor","celltype","cell_ID","marker","gene_description","species","times","specificity","original_celltype.CellMarker",
                                    "original_celltype.PanglaoDB","original_celltype.GeneMarkeR","original_celltype.Azimuth","original_celltype.ASCTB",
                                    "original_celltype.MSigDB")]
    colnames(table_notdesc)<-c("cell_type_ancestor","cell_type","CL_ID","marker","gene_description","species","EC_score","database_specificity","original_celltype.CellMarker",
                               "original_celltype.PanglaoDB","original_celltype.GeneMarkeR","original_celltype.Azimuth","original_celltype.ASCTB",
                               "original_celltype.MSigDB") 
    combine_table<-rbind(uniteDescendant(),table_notdesc)
    #compute specificity 
    combine_table<-as.data.table(combine_table)
    
    mark_spec<-ddply(combine_table,.(marker),nrow)
    colnames(mark_spec)<-c("marker","query_specificity")
    combine_table<-merge(combine_table,mark_spec,by="marker",all.x = TRUE)
    combine_table[,query_specificity:=format(round(1/query_specificity,2), nsmall=2)]

    combine_table<-combine_table[,c("cell_type_ancestor","cell_type","CL_ID","marker","gene_description","species","EC_score","database_specificity","query_specificity","original_celltype.CellMarker",
                                              "original_celltype.PanglaoDB","original_celltype.GeneMarkeR","original_celltype.Azimuth","original_celltype.ASCTB",
                                              "original_celltype.MSigDB")]
    combine_table<-combine_table[database_specificity >= str_replace_all(input$database_spec, ">=|=","") & query_specificity >= str_replace_all(input$query_spec, ">=|=","")]
    combine_table
    
    })
  

  Plot <- reactive({
    if (length(input$celltype) > 1 )
    {
      if (length(input$descendantsof)>=1) {
        ontosubplot<-onto_plot2(cell_onto,selectedBoth()$V1 ,cex=0.8)
        nodes<-as.data.table(ontosubplot@nodes)
        nodes<-nodes[,V1:=tstrsplit(nodes$V1,"CL", fixed = TRUE, keep = 1)]
        nodes<-nodes[, V1:=str_replace_all(V1,"\n"," ")]
        nodes<-nodes[, V1:=str_sub(V1,1,nchar(V1)-1)]
        ontosubplot@nodes<-nodes$V1
        ontosubplot
      }
      else{
        ontosubplot<-onto_plot2(cell_onto,selectedData()$V1 ,cex=0.8)
        nodes<-as.data.table(ontosubplot@nodes)
        nodes<-nodes[,V1:=tstrsplit(nodes$V1,"CL", fixed = TRUE, keep = 1)]
        nodes<-nodes[, V1:=str_replace_all(V1,"\n"," ")]
        nodes<-nodes[, V1:=str_sub(V1,1,nchar(V1)-1)]
        ontosubplot@nodes<-nodes$V1  
        ontosubplot
      }
    }
    else if(length(input$celltype) == 1 ){
      if (length(input$descendantsof)>=1) {
        ontosubplot<-onto_plot2(cell_onto,selectedBoth()$V1 ,cex=0.8)
        nodes<-as.data.table(ontosubplot@nodes)
        nodes<-nodes[,V1:=tstrsplit(nodes$V1,"CL", fixed = TRUE, keep = 1)]
        nodes<-nodes[, V1:=str_replace_all(V1,"\n"," ")]
        nodes<-nodes[, V1:=str_sub(V1,1,nchar(V1)-1)]
        ontosubplot@nodes<-nodes$V1
        ontosubplot
      }
      else{
        ontosubplot<-onto_plot(cell_onto,selectedData()$V1 ,cex=0.8)
        ontosubplot2<-make_graphNEL_from_ontology_plot(ontosubplot)
        dt_onto2<-as.data.table(ontosubplot2@nodes)
        label<-merge(dt_onto2,ontology_celltype,by.x="V1",by.y="cell_ID")
        ontosubplot[["node_attributes"]][["label"]][[label$V1]]<-label$cell_type        
        ontosubplot2@nodes<-ontosubplot[["node_attributes"]][["label"]]
        
        ontosubplot2
      }
      
    }
  })
  
  
  output$plot1 <- renderGrViz({
    if (length(input$celltype) >= 1) {
      onto_igraph<-graph_from_graphnel(Plot(), name = TRUE, weight = TRUE, unlist.attrs = TRUE)
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
        node_list[,fillcolor:= ifelse(label %in%  marker_table[celltype_species %in% input$celltype]$celltype, "#FF8C69","#E1E1E1")]
        node_list[,value:=0.8]
        
      }
      if(length(input$celltype)>=1 & length(input$descendantsof) != 0){
        node_list[,fillcolor:= ifelse(label %in% marker_table[celltype_species %in% input$celltype]$celltype, "#FF8C69",
                                      ifelse(label %in% uniteDescendant()$cell_type,"#FFDB58","#E1E1E1"))]
      }
      else if (length(input$celltype)>1 & length(input$descendantsof) == 0){
        node_list[,fillcolor:= ifelse(label %in%  marker_table[celltype_species %in% input$celltype]$celltype, "#FF8C69","#E1E1E1")]
      }
      
      # node_list<-node_list[, color:= ifelse(label %in%  marker_table[celltype_species %in% input$descendantsof]$celltype, "#7B1B02",
      #                                       ifelse(label %in% marker_table[celltype_species %in% input$celltype]$celltype, "#FF8C69","#E1E1E1"))]
      
      node_list<-node_list[, fontcolor:="black"]
      
      if(input$cellid==TRUE){
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
        
        
        
        render_graph(i_graph_2)
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
        
        i_graph_tree 
      }
    }
  })
  
  output$scalableplot <- renderUI({ 
    tagList(
      div(grVizOutput('plot1',height = input$height, width = input$width)))
  })
  
  
  
  click_plot<-reactive ({
    if (length(input$celltype) >= 1) {
      onto_igraph<-graph_from_graphnel(Plot(), name = TRUE, weight = TRUE, unlist.attrs = TRUE)
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
        node_list[,fillcolor:= ifelse(label %in%  marker_table[celltype_species %in% input$celltype]$celltype, "#FF8C69","#E1E1E1")]
        node_list[,value:=0.8]
        
      }
      
      if(length(input$descendantsof) != 0){
        node_list[,fillcolor:= ifelse(label %in% marker_table[celltype_species %in% input$celltype]$celltype, "#FF8C69",
                                      ifelse(label %in% uniteDescendant()$cell_type,"#FFDB58","#E1E1E1"))]
      }
      else{
        node_list[,fillcolor:= ifelse(label %in%  marker_table[celltype_species %in% input$celltype]$celltype, "#FF8C69","#E1E1E1")]
      }
      
      # node_list<-node_list[, color:= ifelse(label %in%  marker_table[celltype_species %in% input$descendantsof]$celltype, "#7B1B02",
      #                                       ifelse(label %in% marker_table[celltype_species %in% input$celltype]$celltype, "#FF8C69","#E1E1E1"))]
      
      node_list<-node_list[, fontcolor:="black"]
      
      node_list<-merge(node_list,ontology_def[,c("cell_def","cell_type")],by.x="label",by.y="cell_type")
      node_list<-unique(node_list)
      
      
      if(input$cellid==TRUE){
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
        
        as.data.table(i_graph_2 %>% get_node_df())[,c("label","id_external","cell_def")]
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
        
        as.data.table(i_graph_2 %>% get_node_df())[,c("label","id_external","cell_def")]
      }
    }  
  })
  
  
  txt <- reactive({
    req(input$plot1_click)
    nodeval <- input$plot1_click$nodeValues[1:length(input$plot1_click$nodeValues)]
    nodeval<-as.data.table(paste(nodeval,sep="", collapse=" "))
    return(click_plot()[label %in% nodeval$V1]$cell_def)
    
  })
  output$cell_type_def <- renderText({
    req(txt())
    txt()
  })
  
  

  outputTable <- reactive({
    if (length(input$celltype)!=0){
      if (input$unitedescendant=="Yes" & length(input$descendantsof) != 0){
        descendantTable()
      }
      else{
        markerTable()
        
      }
    }
    
  })
  
  output$table1 <- renderDataTable({
    if(length(outputTable())!=0 & input$tabletype=="Complete"){
      outputTable()
    }
    else if(length(outputTable())!=0 & input$tabletype=="Simple"){
      outputTable()[,c("cell_type","CL_ID","marker","gene_description","species","EC_score","database_specificity","query_specificity")]
    } 
  })
  
  # Downloadable csv of selected dataset 
  
  output$downloadData <- downloadHandler(
    filename = function() {
      paste0("Marker_table", input$downloadType)
    },
    content = function(file) {
        if(input$downloadType == ".csv"){
          write.csv(outputTable(), file, row.names = FALSE, quote=FALSE)
        }
        else if(input$downloadType == ".xlsx") {
          write_xlsx(outputTable(), file)
        }
        else if(input$downloadType == ".tsv") {
          write.table(outputTable(), file, quote = FALSE, 
                      sep='\t', row.names = FALSE)
        }
        
      }
    
    
  )
  
######## server for marker search 
  observeEvent(input$helpM,{
    showModal(modalDialog(
      title = "Table Option Information",
      HTML("Marker genes can be ranked and selected by: <br>
             <ul><li> <strong> EC_score </strong>: evidence consistency score, measuring the agreement of different annotation sources </li>
             <li> <strong> database specificity </strong>: whether a gene is a marker for different cell types present in all the accordion database </li></ul>
             In addition: <br>
             <strong> Table type </strong> <br> <ul><li>simple: retrieves a compact and easy table format</li><li> complete: additional information are added</li></ul>")
    ))
  })
  
  
  #table based on selected genes-marker
  tableMarkerInput<-reactive ({
    marker_vec<-as.data.frame(unlist(strsplit(input$marker, "[\\|, +]+")))
    colnames(marker_vec)<-"marker_gene"
    marker_input<-tolower(marker_vec$marker_gene)
    marker_dt<-tolower(marker_table$marker)
    validate(need(marker_input %in% marker_dt, "Please insert valid marker genes!"))
    gene_marker<-marker_table[tolower(marker) %in% tolower(marker_vec$marker_gene)]

    gene_marker<-gene_marker[,c("celltype","cell_ID","marker","gene_description","species","times","specificity","original_celltype.CellMarker",
                                "original_celltype.PanglaoDB","original_celltype.GeneMarkeR","original_celltype.Azimuth","original_celltype.ASCTB",
                                "original_celltype.MSigDB")]
    
    colnames(gene_marker)<-c("cell_type","CL_ID","marker","gene_description","species","EC_score","database_specificity","original_celltype.CellMarker",
                             "original_celltype.PanglaoDB","original_celltype.GeneMarkeR","original_celltype.Azimuth","original_celltype.ASCTB",
                             "original_celltype.MSigDB")  
    gene_marker<-gene_marker[species %in% input$speciesM & database_specificity >= str_replace_all(input$database_specM, ">=|=","")]
    
    
    gene_marker
  })
  
  #table base on input file genes marker 
  tableFileMarker<-reactive ({
    # file<-input$markerfile
    # ext <- tools::file_ext(file$datapath)
    # req(file)
    # validate(need(ext == "csv", "Please upload a csv file!"))
    # marker_file<-as.data.table(fread(file$datapath,header=FALSE))
    # colnames(marker_file)<-"marker_gene"
    file_load<-input$markerfile
    fileName <- file_load$datapath    
    mark<-readChar(fileName, file.info(fileName)$size)
    marker_vec<-as.data.frame(unlist(strsplit(mark, "[\\|, \r\n+]+")))
    colnames(marker_vec)<-"marker_gene"
    table_marker_file<-marker_table[tolower(marker) %in% tolower(marker_vec$marker_gene)]

    table_marker_file<-table_marker_file[,c("celltype","cell_ID","marker","gene_description","species","times","database_specificity","original_celltype.CellMarker",
                                            "original_celltype.PanglaoDB","original_celltype.GeneMarkeR","original_celltype.Azimuth","original_celltype.ASCTB",
                                            "original_celltype.MSigDB")]
    
    colnames(table_marker_file)<-c("cell_type","CL_ID","marker","gene_description","species","EC_score","database_specificity","original_celltype.CellMarker",
                                   "original_celltype.PanglaoDB","original_celltype.GeneMarkeR","original_celltype.Azimuth","original_celltype.ASCTB",
                                   "original_celltype.MSigDB")  
    table_marker_file<-table_marker_file[species %in% input$speciesM & database_specificity >= str_replace_all(input$database_specM, ">=|=","")]
    table_marker_file  
    })
  
  PlotM <- reactive({
    if (nchar(input$marker)>1) {
      cell_matching<-unique(tableMarkerInput()$CL_ID)
      if(length(cell_matching)>1){
        ontosubplot<-onto_plot2(cell_onto,cell_matching ,cex=0.8)
        nodes<-as.data.table(ontosubplot@nodes)
        nodes<-nodes[,V1:=tstrsplit(nodes$V1,"CL", fixed = TRUE, keep = 1)]
        nodes<-nodes[, V1:=str_replace_all(V1,"\n"," ")]
        nodes<-nodes[, V1:=str_sub(V1,1,nchar(V1)-1)]
        ontosubplot@nodes<-nodes$V1
        ontosubplot
      } 
      else{
        ontosubplot<-onto_plot(cell_onto,cell_matching ,cex=0.8)
        ontosubplot2<-make_graphNEL_from_ontology_plot(ontosubplot)
        dt_onto2<-as.data.table(ontosubplot2@nodes)
        label<-merge(dt_onto2,ontology_celltype,by.x="V1",by.y="cell_ID")
        ontosubplot[["node_attributes"]][["label"]][[label$V1]]<-label$cell_type        
        ontosubplot2@nodes<-ontosubplot[["node_attributes"]][["label"]]
        ontosubplot2
        
      }
    }
    else if(!is.null(input$markerfile))  {
      cell_matching<-unique(tableFileMarker()$CL_ID)
      if(length(cell_matching)>1){
        ontosubplot<-onto_plot2(cell_onto,cell_matching ,cex=0.8)
        nodes<-as.data.table(ontosubplot@nodes)
        nodes<-nodes[,V1:=tstrsplit(nodes$V1,"CL", fixed = TRUE, keep = 1)]
        nodes<-nodes[, V1:=str_replace_all(V1,"\n"," ")]
        nodes<-nodes[, V1:=str_sub(V1,1,nchar(V1)-1)]
        ontosubplot@nodes<-nodes$V1
        ontosubplot
        
      }
      else{
        ontosubplot<-onto_plot(cell_onto,cell_matching ,cex=0.8)
        ontosubplot2<-make_graphNEL_from_ontology_plot(ontosubplot)
        dt_onto2<-as.data.table(ontosubplot2@nodes)
        label<-merge(dt_onto2,ontology_celltype,by.x="V1",by.y="cell_ID")
        ontosubplot[["node_attributes"]][["label"]][[label$V1]]<-label$cell_type        
        ontosubplot2@nodes<-ontosubplot[["node_attributes"]][["label"]]
        
        ontosubplot2
      }
    }
    
  })
  
  
  output$plot1M <- renderGrViz({ 
    if (nchar(input$marker)>1 | (!is.null(input$markerfile))) {
      onto_igraph<-graph_from_graphnel(PlotM(), name = TRUE, weight = TRUE, unlist.attrs = TRUE)
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
      if(nchar(input$marker)>0){
        node_list[,fillcolor:= ifelse(label %in%  marker_table[celltype %in% unique(tableMarkerInput()$cell_type)]$celltype, "#FF8C69","#E1E1E1")]
        
      }
      if((!is.null(input$markerfile))){
        node_list[,fillcolor:= ifelse(label %in%  marker_table[celltype %in% unique(tableFileMarker()$cell_type)]$celltype, "#FF8C69","#E1E1E1")]
        
      }
      # node_list<-node_list[, color:= ifelse(label %in%  marker_table[celltype_species %in% input$descendantsof]$celltype, "#7B1B02",
      #                                       ifelse(label %in% marker_table[celltype_species %in% input$celltype]$celltype, "#FF8C69","#E1E1E1"))]
      
      node_list<-node_list[, fontcolor:="black"]
      
      if(input$cellidM==TRUE){
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
        
        
        
        render_graph(i_graph_2)
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
        
        i_graph_tree 
      }
    }
  })
  
  output$scalableplotM <- renderUI({ 
    tagList(
      div(grVizOutput('plot1M',height = input$heightM, width = input$widthM)))
  })
  
  
  
  click_plotM<-reactive ({
    if (nchar(input$marker)>0 | (!is.null(input$markerfile))) {
      onto_igraph<-graph_from_graphnel(PlotM(), name = TRUE, weight = TRUE, unlist.attrs = TRUE)
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
      if((!is.null(input$markerfile))){
        node_list[,fillcolor:= ifelse(label %in%  marker_table[celltype %in% unique(tableFileMarker()$cell_type)]$celltype, "#FF8C69","#E1E1E1")]
        
      }
      # node_list<-node_list[, color:= ifelse(label %in%  marker_table[celltype_species %in% input$descendantsof]$celltype, "#7B1B02",
      #                                       ifelse(label %in% marker_table[celltype_species %in% input$celltype]$celltype, "#FF8C69","#E1E1E1"))]
      
      node_list<-node_list[, fontcolor:="black"]
      
      node_list<-merge(node_list,ontology_def[,c("cell_def","cell_type")],by.x="label",by.y="cell_type")
      node_list<-unique(node_list)
      
      
      if(input$cellidM==TRUE){
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
        
        as.data.table(i_graph_2 %>% get_node_df())[,c("label","id_external","cell_def")]
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
        
        as.data.table(i_graph_2 %>% get_node_df())[,c("label","id_external","cell_def")]
      }
    }  
  })

  
  txtM <- reactive({
    req(input$plot1M_click)
    nodeval <- input$plot1M_click$nodeValues[1:length(input$plot1M_click$nodeValues)]
    nodeval<-as.data.table(paste(nodeval,sep="", collapse=" "))
    return(  click_plotM()[label %in% nodeval$V1]$cell_def)
    
  })
  output$cell_type_defM <- renderText({
    req(txtM())
    txtM()
  })
  

  outputTableM<-reactive({
    if(nchar(input$marker)>1){
      
      tableMarkerInput()[EC_score>= str_replace_all(input$timesM, ">=","")]
    }
    else if(!is.null(input$markerfile)){
      tableFileMarker()[EC_score>= str_replace_all(input$timesM, ">=","")]
    }
  })
  
  output$table1M <- renderDataTable({
    if(length(outputTableM())!=0 & input$tabletypeM=="Complete"){
      outputTableM()
    }
    else if(length(outputTableM())!=0 & input$tabletypeM=="Simple"){
      outputTableM()[,c("cell_type","CL_ID","marker","gene_description","species","EC_score","database_specificity")]
    }

    
  })
  
  

  
  # Downloadable csv of selected dataset 
  
  output$downloadDataM <- downloadHandler(
    filename = function() {
      paste0("Marker_table", input$downloadTypeM)
    },
    content = function(file) {
      if(nchar(input$marker)>1 | !is.null(input$markerfile)) {
        if(input$downloadTypeM == ".csv"){ 
          if(input$tabletypeM=="Simple"){
            write.csv(outputTableM()[,c("cell_type","CL_ID","marker","gene_description","species","EC_score","database_specificity")], file, row.names = FALSE, quote=FALSE) 
          }
          else if (input$tabletypeM=="Complete"){
            write.csv(outputTableM(), file, row.names = FALSE, quote=FALSE) 
            
          }

        }
      else if(input$downloadTypeM == ".xlsx") {
        if(input$tabletypeM=="Simple"){
          write_xlsx(outputTableM()[,c("cell_type","CL_ID","marker","gene_description","species","EC_score","database_specificity")], file)
        }
        else if (input$tabletypeM=="Complete"){
          write_xlsx(outputTableM(), file)
          
        }
        
              }
      else if(input$downloadTypeM == ".tsv") {
        if(input$tabletypeM=="Simple"){
          write.table(outputTableM()[,c("cell_type","CL_ID","marker","gene_description","species","EC_score","database_specificity")], file, quote = FALSE, 
                      sep='\t', row.names = FALSE)
          }
        else if (input$tabletypeM=="Complete"){
          write.table(outputTableM(), file, quote = FALSE, 
                      sep='\t', row.names = FALSE)
          
        }
      }
      }
      
      
      
      
      
      
      
    }
    
    
    
  )
  
  
  
  
  
  
  
}

shinyApp(ui = ui, server = server)


