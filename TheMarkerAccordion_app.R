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

#source("packages.R")
source('helper_function.R')

#marker_table_orig<-read.table("data/Hema_Accordion_wideTable.txt",sep='\t',header=TRUE)
marker_table<-fread("data/Hema_Accordion_8db.txt",sep='\t',header=TRUE)
marker_table<-as.data.table(marker_table)

#update HGNC symbol 
# marker_table<-update_HGNC(in_list,anno_file="C:/Users/emmab/Desktop/PhD/Rscript_vari/hgnc/hgnc_complete_set_2022/hgnc_tables_2022.RData")
# hgnc_symbol_imm<-as.data.table(hgnc_symbol_imm)[!is.na(previous)][type!="orphan"]

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
  dashboardHeader(titleWidth  = 500,title = tagList(
    tags$span(
      class = "logo-mini", "CMA"
    ),
    tags$span(
      class = "logo-lg", "The Cell Marker Accordion"
    )
  )
  ),
  ## Sidebar content
  dashboardSidebar(width=300,
                   sidebarMenu(style = "position: relative; overflow: visible;",
                               menuItem("Homepage", tabName = "dashboard", icon = icon("home")),
                               menuItem("Search by cell types", tabName = "celltype_h", icon = icon("circle-notch")),
                               menuItem("Search by marker genes", tabName = "marker_h", icon = icon("dna"))
                   )
  ),
  dashboardBody(tags$head(
    tags$script(HTML("$('body').addClass('sidebar-mini');")),
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
      
      
      #HEMATOPOIETIC SYSTEM 
      # Second tab content: Search markers according to cell types 
      tabItem(tabName = "celltype_h",
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
                                                              conditionalPanel(condition= "input.descendantsof != ''", column(2,radioButtons("mergeDescendant","Merge subtypes", c("Yes","No"),selected="No"))))))),
                  
                  
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
                         tags$strong("-"),tags$a("Web Application",href = "http://www.gsea-msigdb.org/gsea/msigdb/collections.jsp#C8")),
                  tags$p(tags$strong("Blood Proteoform"), tags$a("The Blood Proteoform Atlas: A reference map of proteoforms in human hematopoietic cells,Science 2022",href="https://www.science.org/stoken/author-tokens/ST-317/full"),
                         tags$strong("-"),tags$a("Web Application",href = "https://blood-proteoform-atlas.org/")),
                  tags$p(tags$strong("Abcam"), tags$a(href="https://www.abcam.com/")))),
      
      
      
      
      tabItem(tabName="marker_h", 
              div(fluidRow(column(width=6,wellPanel(id="sidebar",checkboxGroupInput("speciesM", "Select species:",
                                                                                    choiceNames =
                                                                                      list(tags$img(src = "human.png"),tags$img(src = "mouse.png")),
                                                                                    choiceValues =
                                                                                      list("Human","Mouse"),selected = c("Human","Mouse"),inline=TRUE),
                                                    br(),
                                                    textInput("marker", "Insert marker genes", value = "CD34", width = NULL, placeholder = NULL),
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
                         tags$strong("-"),tags$a("Web Application",href = "http://www.gsea-msigdb.org/gsea/msigdb/collections.jsp#C8" )),
                  tags$p(tags$strong("Blood Proteoform"), tags$a("The Blood Proteoform Atlas: A reference map of proteoforms in human hematopoietic cells,Science 2022",href="https://www.science.org/stoken/author-tokens/ST-317/full"),
                         tags$strong("-"),tags$a("Web Application",href = "https://blood-proteoform-atlas.org/")),
                  tags$p(tags$strong("Abcam"), tags$a(href="https://www.abcam.com/")))),
      
      #NERVOUS SYSTEM PART
      tabItem(tabName = "celltype_n",
              div(fluidRow(column(width=6,wellPanel(id="sidebar_n",
                                                    checkboxGroupInput("species_n", "Select species:",
                                                                       choiceNames =
                                                                         list(tags$img(src = "human.png"),tags$img(src = "mouse.png")),
                                                                       choiceValues =
                                                                         list("Human","Mouse"),selected = c("Human","Mouse"),inline=TRUE),
                                                    br(),
                                                    pickerInput('celltype_n', 'Cell type', choices= NULL ,multiple=TRUE, options = list(`actions-box` = TRUE,`live-search`=TRUE)),
                                                    pickerInput('descendantsof_n', 'See subtypes of:', choices= NULL,multiple=TRUE, options = list(`actions-box` = TRUE,`live-search`=TRUE)),
                                                    checkboxInput("cellid_n","Plot CL_ID",value=FALSE))),
                           br(),
                           #change style sliderinput
                           tags$style(HTML(".js-irs-0 .irs-single, .js-irs-0 .irs-bar-edge, .js-irs-0 .irs-bar {background: #990000}")),
                           tags$style(HTML(".js-irs-1 .irs-single, .js-irs-1 .irs-bar-edge, .js-irs-1 .irs-bar {background: #990000}")),
                           titlePanel(shiny::span((icon("sliders",class = "about-icon fa-pull-left", lib = "font-awesome")), p(style="text-align: justify;", HTML("<h>Adjust plot size </h>")))),
                           column(width=6,offset=0, sliderInput(inputId = "height_n", label = "Height", min = 200, max = 6500, value = 400, step = 200,width="80%"),
                                  br(),
                                  sliderInput(inputId = "width_n", label = "Width", min = 200, max = 6500, value = 400, step=200,width="80%"))),
                  tags$head(tags$style(HTML('
         #sidebar_n {
            background-color: #ad000019;
            font-size: 16px;
        }'))),
                  #HTML(icon("hand-back-point-up"),"<h4> <strong> Click </strong> on a node to look at cell type description</h4>"),
                  titlePanel(shiny::span((icon("hand-pointer",class = "about-icon fa-pull-left", lib = "font-awesome")), p(style="text-align: justify;", HTML("<h> <strong> Click </strong> on a node to look at cell type description</h>")))),
                  fluidRow(column(12,align="center",div(style='width:100%;overflow-x: scroll;height:100%;overflow-y: scroll;', uiOutput('scalableplot_n')))),
                  #grVizOutput("plot1"),
                  tags$style(
                    '#test_n {
    cursor: grap;
    color: black;
    }'),
                  conditionalPanel(id="test_n",condition= "input.plot1_n_click",wellPanel(textOutput("cell_type_def_n"))),
                  br(),
                  br(),
                  
                  conditionalPanel(condition= "input.descendantsof_n != ''",titlePanel(shiny::span(p(style="text-align: justify;", HTML("<h>Table options</h>"),actionButton('help_n', 'Info',icon= icon("info"), align="left"))))),
                  conditionalPanel(condition= "input.descendantsof_n == ''",titlePanel(shiny::span(p(style="text-align: justify;", HTML("<h>Table options</h>"),actionButton('help_empty_n', 'Info',icon= icon("info"), align="left"))))),
                  
                  fluidRow(column(width=12,wellPanel(id="sidebar2_n",
                                                     fluidRow(column(2,radioButtons('times_n','EC_score', c(">=1",">=2",">=3",">=4"),selected = ">=1")),
                                                              column(3,radioButtons('database_spec_n','database_specificity', c(">=0",">=0.25",">=0.5","=1"),selected = ">=0")),
                                                              column(3,radioButtons('query_spec_n','query_specificity', c(">=0",">=0.25",">=0.5","=1"),selected = ">=0")),
                                                              column(2,radioButtons("tabletype_n","Table type",c("Simple","Complete"),selected="Simple")),
                                                              conditionalPanel(condition= "input.descendantsof_n != ''", column(2,radioButtons("mergeDescendant_n","Merge subtypes", c("Yes","No"),selected="No"))))))),
                  
                  
                  fluidRow(column(4,radioButtons("downloadType_n", "Download Format", choices = c("CSV" = ".csv",
                                                                                                  "XLSX" = ".xlsx",
                                                                                                  "TSV" = ".tsv"),inline = TRUE),
                                  column(4,downloadButton("downloadData_n", "Download")))),
                  
                  tags$head(tags$style(HTML('
         #sidebar2_n {
            background-color: #ad000019;
            font-size: 16px;
        }'))),
                  # bsTooltip("times", "Evidence consensus score (number of databases)", placement = "top", trigger = "hover",
                  #           options = NULL),
                  br(),
                  br(),
                  dataTableOutput('table1_n'),
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
                  tags$p(tags$strong("Blood Proteoform"), tags$a("The Blood Proteoform Atlas: A reference map of proteoforms in human hematopoietic cells,Science 2022",href="https://www.science.org/stoken/author-tokens/ST-317/full"),
                        tags$strong("-"),tags$a("Web Application",href = "https://blood-proteoform-atlas.org/")),
                  tags$p(tags$strong("Abcam"), tags$a(href="https://www.abcam.com/")))),
  
      
      
      
      tabItem(tabName="marker_n", 
              div(fluidRow(column(width=6,wellPanel(id="sidebar_n",checkboxGroupInput("speciesM_n", "Select species:",
                                                                                      choiceNames =
                                                                                        list(tags$img(src = "human.png"),tags$img(src = "mouse.png")),
                                                                                      choiceValues =
                                                                                        list("Human","Mouse"),selected = c("Human","Mouse"),inline=TRUE),
                                                    br(),
                                                    textInput("marker_n", "Insert marker genes", value = "", width = NULL, placeholder = NULL),
                                                    fileInput("markerfile_n", "Load text file with marker genes ",buttonLabel=list(icon("upload")),
                                                              multiple = FALSE),
                                                    checkboxInput("cellidM_n","Plot CL_ID",value=FALSE))),
                           br(),
                           #change style sliderinput
                           tags$style(HTML(".js-irs-2 .irs-single, .js-irs-2 .irs-bar-edge, .js-irs-2 .irs-bar {background: #990000}")),
                           tags$style(HTML(".js-irs-3 .irs-single, .js-irs-3 .irs-bar-edge, .js-irs-3 .irs-bar {background: #990000}")),
                           titlePanel(shiny::span((icon("sliders",class = "about-icon fa-pull-left", lib = "font-awesome")), p(style="text-align: justify;", HTML("<h>Adjust plot size </h>")))),
                           column(width=6,offset=0, sliderInput(inputId = "heightM_n", label = "Height", min = 200, max = 3500, value = 400, step = 200,width="80%"),
                                  br(),
                                  sliderInput(inputId = "widthM_n", label = "Width", min = 200, max = 3500, value = 400, step=200,width="80%"))),
                  tags$head(tags$style(HTML('
         #sidebarM_n {
            background-color: #ad000019;
            font-size: 16px;
        }'))),
                  #HTML(icon("hand-back-point-up"),"<h4> <strong> Click </strong> on a node to look at cell type description</h4>"),
                  titlePanel(shiny::span((icon("hand-pointer",class = "about-icon fa-pull-left", lib = "font-awesome")), p(style="text-align: justify;", HTML("<h> <strong> Click </strong> on a node to look at cell type description</h>")))),
                  fluidRow(column(12,align="center",div(style='width:100%;overflow-x: scroll;height:100%;overflow-y: scroll;', uiOutput('scalableplotM_n')))),
                  #grVizOutput("plot1"),
                  tags$style(
                    '#testM_n {
    cursor: grap;
    color: black;
    }'),
                  conditionalPanel(id="testM_n",condition= "input.plot1M_n_click",wellPanel(textOutput("cell_type_defM_n"))),
                  br(),
                  br(),
                  titlePanel(shiny::span(p(style="text-align: justify;", HTML("<h>Table options</h>"),actionButton('helpM_n', 'Info',icon= icon("info"), align="left")))),                  
                  fluidRow(column(width=8,wellPanel(id="sidebar2M_n",
                                                    fluidRow(column(3,radioButtons('timesM_n','EC_score', c(">=1",">=2",">=3",">=4"),selected = ">=1")),
                                                             column(3,radioButtons('database_specM_n','database_specificity', c(">=0",">=0.25",">=0.5","=1"),selected = ">=0")),
                                                             column(3,radioButtons("tabletypeM_n","Table type",c("Simple","Complete"),selected="Simple")))))),
                  fluidRow(column(4,radioButtons("downloadTypeM_n", "Download Format", choices = c("CSV" = ".csv",
                                                                                                   "XLSX" = ".xlsx",
                                                                                                   "TSV" = ".tsv"),inline = TRUE),
                                  column(4,downloadButton("downloadDataM_n", "Download")))),
                  
                  tags$head(tags$style(HTML('
         #sidebar2M_n {
            background-color: #ad000019;
            font-size: 16px;
        }'))),
                  # bsTooltip("times", "Evidence consensus score (number of databases)", placement = "top", trigger = "hover",
                  #           options = NULL),
                  br(),
                  br(),
                  dataTableOutput('table1M_n'),
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
                  tags$p(tags$strong("Blood Proteoform"), tags$a("The Blood Proteoform Atlas: A reference map of proteoforms in human hematopoietic cells,Science 2022",href="https://www.science.org/stoken/author-tokens/ST-317/full"),
                         tags$strong("-"),tags$a("Web Application",href = "https://blood-proteoform-atlas.org/")),
                  tags$p(tags$strong("Abcam"),tags$a(href="https://www.abcam.com/"))))
      
    )
  )
  
)
server <- function(input, output, session) {
  
  ############ HEMATOPOIETIC SYSTEM 
  
  ###### server for cell type search 
  
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
  
  descendantTable<-reactive ({
    if(length(input$descendantsof)>0){
      select_descendant(onto_plot, marker_table, input$descendantsof)
    }
    
  })
  
  mergeDescendantTable<-reactive ({
    if(length(input$descendantsof)>0 & input$mergeDescendant=="Yes"){
      table_merge_descendant(onto_plot, cell_onto, marker_table, input$descendantsof,input$species)
    }
  })
  
  markerTableOutput <- reactive ({
    #table with descandants merged
    if(length(input$descendantsof)!=0 & input$mergeDescendant=="Yes"){ 
      table_merged_desc_other(marker_table,input$celltype,mergeDescendantTable(),input$species)
    }
    #table with descendants NOT merged 
    else if (length(input$descendantsof)!=0 & input$mergeDescendant=="No"){
      table_desc_other(marker_table,input$celltype,descendantTable(),input$species)
    }
    else if(length(input$descendantsof)==0){
      table_input_celltypes(marker_table,input$celltype,input$species)
    }
    
  })
  
  markerTablePlot <- reactive ({
    #plot with descendants
    if (length(input$descendantsof)!=0){
      table_desc_other(marker_table,input$celltype,descendantTable(),input$species)
    }
    else if(length(input$descendantsof)==0){
      table_input_celltypes(marker_table,input$celltype,input$species)
    }
    
  })
  
  Plot <- reactive({
    if (length(input$celltype) > 1 | length(input$descendantsof)>=1)
    {  ontosubplot<-onto_plot2(cell_onto,markerTablePlot()$CL_ID ,cex=0.8)
    nodes<-as.data.table(ontosubplot@nodes)
    nodes<-nodes[,V1:=tstrsplit(nodes$V1,"CL", fixed = TRUE, keep = 1)]
    nodes<-nodes[, V1:=str_replace_all(V1,"\n"," ")]
    nodes<-nodes[, V1:=str_sub(V1,1,nchar(V1)-1)]
    ontosubplot@nodes<-nodes$V1  
    ontosubplot
    
    }
    else if(length(input$celltype) == 1 ){
      ontosubplot<-onto_plot(cell_onto,markerTablePlot()$CL_ID ,cex=0.8)
      ontosubplot2<-make_graphNEL_from_ontology_plot(ontosubplot)
      dt_onto2<-as.data.table(ontosubplot2@nodes)
      label<-merge(dt_onto2,ontology_celltype,by.x="V1",by.y="cell_ID")
      ontosubplot[["node_attributes"]][["label"]][[label$V1]]<-label$cell_type        
      ontosubplot2@nodes<-ontosubplot[["node_attributes"]][["label"]]
      
      ontosubplot2
    }
  })
  
  
  output$plot1 <- renderGrViz({
    if(length(input$descendantsof)>=1){
      hierac_plot1_desc(marker_table, ontology_celltype,Plot(),input$celltype,descendantTable(),input$cellid)
    }
    else{
      hierac_plot1(marker_table, ontology_celltype,Plot(),input$celltype,input$cellid)
    }
    
  })
  
  output$scalableplot <- renderUI({ 
    tagList(
      div(grVizOutput('plot1',height = input$height, width = input$width)))
  })
  
  
  
  click_plot<-reactive ({
    click_node1(marker_table, ontology_celltype,Plot(),input$celltype,input$descendantsof,mergeDescendantTable(),input$cellid,ontology_def) 
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
  
  
  outputTable<-reactive({
    if(length(input$celltype)!=0 & input$tabletype=="Complete"){
      markerTableOutput()[EC_score>= str_replace_all(input$times, ">=","") & database_specificity >= str_replace_all(input$database_spec, ">=|=","") & query_specificity >= str_replace_all(input$query_spec, ">=|=","")]
    }
    else if(length(input$celltype)!=0 & input$tabletype=="Simple"){
      if(input$mergeDescendant=="Yes"){
        markerTableOutput()[EC_score>= str_replace_all(input$times, ">=","") & database_specificity >= str_replace_all(input$database_spec, ">=|=","") & query_specificity >= str_replace_all(input$query_spec, ">=|=","")
        ][,c("cell_type_ancestor","cell_type","CL_ID","marker","gene_description","species","EC_score","database_specificity","query_specificity")]
        
      } 
      else{
        markerTableOutput()[EC_score>= str_replace_all(input$times, ">=","") & database_specificity >= str_replace_all(input$database_spec, ">=|=","") & query_specificity >= str_replace_all(input$query_spec, ">=|=","")
        ][,c("cell_type","CL_ID","marker","gene_description","species","EC_score","database_specificity","query_specificity")]
      }
    }
    
    
  })  
  
  output$table1 <- renderDataTable({
    outputTable()
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
    if(length(input$marker)!=0){
      marker_vec<-as.data.frame(unlist(strsplit(input$marker, "[\\|, +]+")))
      colnames(marker_vec)<-"marker_gene"
      marker_input<-tolower(marker_vec$marker_gene)
      marker_dt<-tolower(marker_table$marker)
      validate(need(marker_input %in% marker_dt, "Please insert valid marker genes!"))
      gene_marker<-marker_table[tolower(marker) %in% tolower(marker_vec$marker_gene)]
      
      gene_marker<-gene_marker[,c("celltype","cell_ID","marker","gene_description","marker_type","species","times","specificity","original_celltype.CellMarker",
                                  "original_celltype.PanglaoDB","original_celltype.GeneMarkeR","original_celltype.Azimuth","original_celltype.ASCTB",
                                  "original_celltype.MSigDB","original_celltype.CellTypist","original_celltype.Abcam")]
      
      colnames(gene_marker)<-c("cell_type","CL_ID","marker","gene_description","marker_type","species","EC_score","database_specificity","original_celltype.CellMarker",
                               "original_celltype.PanglaoDB","original_celltype.GeneMarkeR","original_celltype.Azimuth","original_celltype.ASCTB",
                               "original_celltype.MSigDB","original_celltype.CellTypist","original_celltype.Abcam")  
      gene_marker<-gene_marker[species %in% input$speciesM]
      
      
      gene_marker
    }
  })
  
  #table base on input file genes marker 
  tableFileMarker<-reactive ({
    
    file_load<-input$markerfile
    fileName <- file_load$datapath    
    mark<-readChar(fileName, file.info(fileName)$size)
    marker_vec<-as.data.frame(unlist(strsplit(mark, "[\\|, \r\n+]+")))
    colnames(marker_vec)<-"marker_gene"
    table_marker_file<-marker_table[tolower(marker) %in% tolower(marker_vec$marker_gene)]
    
    table_marker_file<-table_marker_file[,c("celltype","cell_ID","marker","gene_description","marker_type","species","times","specificity","original_celltype.CellMarker",
                                            "original_celltype.PanglaoDB","original_celltype.GeneMarkeR","original_celltype.Azimuth","original_celltype.ASCTB",
                                            "original_celltype.MSigDB","original_celltype.CellTypist","original_celltype.Abcam")]
    
    colnames(table_marker_file)<-c("cell_type","CL_ID","marker","gene_description","marker_type","species","EC_score","database_specificity","original_celltype.CellMarker",
                                   "original_celltype.PanglaoDB","original_celltype.GeneMarkeR","original_celltype.Azimuth","original_celltype.ASCTB",
                                   "original_celltype.MSigDB","original_celltype.CellTypist","original_celltype.Abcam")  
    table_marker_file <- table_marker_file[species %in% input$speciesM]
    table_marker_file
    
  })
  
  # Final table output: 
  #1. Marker insert by the user in the box 
  #2. Marker add through a marker file 
  #3. If both marker in the box and file marker are present add together 
  
  tableInputComplete<-reactive({
    if(nchar(input$marker)>1 & (!is.null(input$markerfile))){
      rbind(tableMarkerInput(),tableFileMarker())
    }
    else if(nchar(input$marker)>1  & is.null(input$markerfile)){
      tableMarkerInput()
    }
    else if (nchar(input$marker)==0 & !is.null(input$markerfile)) {
      tableFileMarker()
    }
    
  })
  
  PlotM <- reactive({
    if (nrow(tableInputComplete())>=1) {
      cell_matching<-unique(tableInputComplete()$CL_ID)
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
    hierac_plot2(marker_table, ontology_celltype, PlotM(), tableInputComplete(),input$cellidM) 
    
  })
  
  output$scalableplotM <- renderUI({ 
    tagList(
      div(grVizOutput('plot1M',height = input$heightM, width = input$widthM)))
  })
  
  
  
  click_plotM<-reactive ({
    click_node2(marker_table, ontology_celltype, PlotM(), tableInputComplete(),input$cellidM, ontology_def)
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
    if(nrow(tableInputComplete())!=0 & input$tabletypeM=="Complete"){
      tableInputComplete()[EC_score >= str_replace_all(input$timesM, ">=","") & database_specificity >= str_replace_all(input$database_specM, ">=|=","")]
    }
    else if(nrow(tableInputComplete())!=0 & input$tabletypeM=="Simple"){
      tableInputComplete()[EC_score>= str_replace_all(input$timesM, ">=","") & database_specificity >= str_replace_all(input$database_specM, ">=|=","")
      ][,c("cell_type","CL_ID","marker","gene_description","species","EC_score","database_specificity")]
    }
    
    
  })  
  
  output$table1M <- renderDataTable({
    outputTableM()
  })
  
  # Downloadable csv of selected dataset 
  
  output$downloadDataM <- downloadHandler(
    filename = function() {
      paste0("Marker_table", input$downloadTypeM)
    },
    content = function(file) {
      if(input$downloadTypeM == ".csv"){
        write.csv(outputTableM(), file, row.names = FALSE, quote=FALSE)
      }
      else if(input$downloadTypeM == ".xlsx") {
        write_xlsx(outputTableM(), file)
      }
      else if(input$downloadTypeM == ".tsv") {
        write.table(outputTableM(), file, quote = FALSE, 
                    sep='\t', row.names = FALSE)
      }
      
    }
    
    
  ) 

}







shinyApp(ui = ui, server = server)


