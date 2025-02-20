list.of.packages <- c("rstudioapi","shinydashboard","shiny","stringr","DiagrammeR","igraph",
                      "shinyWidgets","ontologyIndex","data.table","plyr","ontologyPlot",
                      "writexl", "dplyr", "shinyBS","shinyhelper",
                      "shinydashboardPlus", "readxl","rstatix", "tidyverse")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) suppressMessages(suppressWarnings({install.packages(new.packages, dependencies = T)}))

list.of.packages.bio<-c("ontoProc")
new.packages_bio <- list.of.packages.bio[!(list.of.packages.bio %in% installed.packages()[,"Package"])]
if(length(new.packages_bio)) suppressMessages(suppressWarnings({install.packages(new.packages_bio, dependencies = T)}))

library(shinydashboard)
library(shiny)
library(stringr)
library(DiagrammeR)
library(igraph)
library(shinyWidgets)
library(ontologyIndex)
library(plyr)
library(ontologyPlot)
library(ontoProc)
library(writexl)
library(dplyr)
library(shinyBS)
library(shinyhelper)
library(shinydashboardPlus)
library(readxl)
library(plyr)
library(rstatix)
library(tidyverse)
library(data.table)
library("rstudioapi")

source('helper_function.R')

### REMEMBER TO CHANGE WHEN LOAD ON GITHUB -----
setwd(dirname(getActiveDocumentContext()$path)) #locally
#setwd("/home/rdds/www/apps/CellMarkerAccordion/") #for github


load("data/accordion_complete.rda")

#load ontology 
cell_onto<-get_ontology("data/cl-basic.obo", propagate_relationships = c("is_a","develops_from"), extract_tags = "everything")
onto_plot<-onto_plot2(cell_onto, unique(accordion_complete$celltype_ID))
nodes<-as.data.table(onto_plot@nodes)
nodes<-nodes[,V1:=tstrsplit(nodes$V1,"CL", fixed = TRUE, keep = 1)]
nodes<-nodes[,V1:=tstrsplit(nodes$V1,"DOID", fixed = TRUE, keep = 1)]
nodes<-nodes[, V1:=str_replace_all(V1,"\n"," ")]
nodes<-nodes[, V1:=str_sub(V1,1,nchar(V1)-1)]
onto_plot@nodes<-nodes$V1


ontology_def<-unique(accordion_complete[,c("celltype_ID","cell_definition","celltype")])
ontology_celltype<-unique(accordion_complete[,c("celltype","celltype_ID")])




nmarker_values<-c("ALL")
disease_list<-unique(accordion_complete$DO_diseasetype)
names(disease_list)<-unique(accordion_complete$DO_diseasetype)

tissue_list<-unique(accordion_complete$Uberon_tissue)
names(tissue_list)<-unique(accordion_complete$Uberon_tissue)


celltype_list<-unique(accordion_complete$celltype)
names(celltype_list)<-unique(accordion_complete$celltype)

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
                               menuItem("Search by tissue and cell types", tabName = "celltype_h", icon = icon("circle-notch")),
                               menuItem("Search by marker genes", tabName = "marker_h", icon = icon("dna")),
                               menuItem("Cell types annotation", tabName = "anno", icon = icon("stack-overflow"))
                   )
  ),
  dashboardBody(tags$head(
    tags$style("#shiny-modal img { max-width: 100%; }"),
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
                p(style="text-align: justify;", HTML("<h>A critical step in single-cell and spatial data analysis is the accurate annotation of cell types. The inherent heterogeneity of single-cell data, combined with significant inconsistencies in annotation methodologies, often results in noisy and unreliable classifications. These discrepancies can hide biological insights and hinder reproducibility.
To address this issue, the Cell Marker Accordion approach was developed as a harmonization framework. By leveraging filtering, standardization, and integration, it systematically refines and balances the contributions of multiple gene marker databases. This process ensures a more consistent and reliable annotation, ultimately enhancing the clarity and interpretability of single-cell and spatial datasets. 
                                                     <br> <strong> The Cell Marker Accordion </strong> web interface allows to easily: </h> "))),
              
              titlePanel(shiny::span((icon("circle-notch",class = "about-icon fa-pull-left", lib = "font-awesome")), p(style="text-align: justify;", HTML("<h>  Search and download lists of marker genes by cell types in different tissues. </h>")))),          
              titlePanel(shiny::span((icon("dna",class = "about-icon fa-pull-left", lib = "font-awesome")), p(style="text-align: justify;", HTML("<h>  Search and download lists of cell types by marker genes. </h>")))),                                                                                                              
              titlePanel(shiny::span((icon("sitemap",class = "about-icon fa-pull-left", lib = "font-awesome")), p(style="text-align: justify;", HTML("<h>  Browse hierarchies of cell types following the Cell Ontology structure in order to obtain the desired level of specificity in the markers in both searches options. </h>")))),
              titlePanel(shiny::span((icon("arrow-down-short-wide",class ="about-icon fa-pull-left", lib = "font-awesome")), p(style="text-align: justify;", HTML("<h> Rank and select marker genes by their evidence consistency scores. </h>"))))),                                        
      
      
      # Second tab content: Search markers according to cell types 
      tabItem(tabName = "celltype_h",
              div(fluidRow(column(width=8,wellPanel(id="sidebar",
                                                    checkboxGroupInput("species", "Select species:",
                                                                       choiceNames =
                                                                         list(tags$img(src = "human.png"),tags$img(src = "mouse.png")),
                                                                       choiceValues =
                                                                         list("Human","Mouse"),selected = c("Human"),inline=TRUE),
                                                    br(),
                                                    #pickerInput('disease', 'Hematologic disorder', choices= NULL ,multiple=TRUE, options = list(`actions-box` = TRUE,`live-search`=TRUE)),
                                                    pickerInput('disease', 'Condition',  choices= disease_list, selected = "healthy", multiple=TRUE, options = list(`actions-box` = TRUE,`live-search`=TRUE, style="box-celltypes")),
                                                    pickerInput('tissue', 'Tissue',  choices= NULL,selected=tissue_list, multiple=TRUE, options = list(`actions-box` = TRUE,`live-search`=TRUE, style="box-celltypes")),
                                                    #checkboxInput("collapse_tissue","",value=FALSE))),
                                                    checkboxInput("tissue_aware","Tissue aware",value=TRUE),
                                                    pickerInput('celltype', 'Cell type', choices= NULL ,multiple=TRUE,selected=celltype_list, options = list(`actions-box` = TRUE,`live-search`=TRUE, style="box-celltypes")),
                                                    
                                                    splitLayout(cellWidths = c("75%", "25%"),fileInput("usermarker", "Load your custom annotation",buttonLabel=list(icon("upload")),multiple = FALSE),
                                                                actionButton('usermarkerinfo', 'InputFile',icon= icon("file-circle-question"), align="left",style='margin-top:30px')),
                                                    br(),
                                                    pickerInput('descendantsof', 'See subtypes of:', choices= NULL,multiple=TRUE, options = list(`actions-box` = TRUE,`live-search`=TRUE, style="box-celltypes"),choicesOpt = list(style = rep(("font-size: 18px; line-height: 1.6;"), 141))),
                                                    checkboxInput("cellid","Plot celltype_ID",value=FALSE))),
                           br(),
                           #change style sliderinput
                           tags$style(HTML(".js-irs-0 .irs-single, .js-irs-0 .irs-bar-edge, .js-irs-0 .irs-bar {background: #990000}")),
                           tags$style(HTML(".js-irs-1 .irs-single, .js-irs-1 .irs-bar-edge, .js-irs-1 .irs-bar {background: #990000}")),
                           titlePanel(shiny::span((icon("sliders",class = "about-icon fa-pull-left", lib = "font-awesome")), p(style="text-align: justify;", HTML("<h>Adjust plot size </h>")))),
                           column(width=4,offset=0, sliderInput(inputId = "height", label = "Height", min = 200, max = 6500, value = 400, step = 200,width="80%"),
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
                  conditionalPanel(id="test",condition= "input.plot1_click",wellPanel(textOutput("celltype_def"))),
                  br(),
                  br(),
                  
                  conditionalPanel(condition= "input.descendantsof != ''",titlePanel(shiny::span(p(style="text-align: justify;", HTML("<h>Table options</h>"),actionButton('help', 'Info',icon= icon("info"), align="left"))))),
                  conditionalPanel(condition= "input.descendantsof == ''",titlePanel(shiny::span(p(style="text-align: justify;", HTML("<h>Table options</h>"),actionButton('help_empty', 'Info',icon= icon("info"), align="left"))))),
                  
                  fluidRow(column(width=12,wellPanel(id="sidebar2",
                                                     fluidRow(column(2,radioButtons('EC_score','EC_score', c(">=1",">=2",">=3",">=4",">=5",">=6",">=7"),selected = ">=1")),
                                                              column(3,radioButtons('specificity','Specificity_score', c(">=0",">=0.25",">=0.5","=1"),selected = ">=0")),
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
                  # bsTooltip("EC_score", "Evidence consensus score (number of databases)", placement = "top", trigger = "hover",
                  #           options = NULL),
                  br(),
                  br(),
                  dataTableOutput('table1'),
                  br()
                  )),
      
  ###### search by markers -----    
      tabItem(tabName="marker_h", 
              div(fluidRow(column(width=6,wellPanel(id="sidebar",checkboxGroupInput("speciesM", "Select species:",
                                                                                    choiceNames =
                                                                                      list(tags$img(src = "human.png"),tags$img(src = "mouse.png")),
                                                                                    choiceValues =
                                                                                      list("Human","Mouse"),selected = c("Human"),inline=TRUE),
                                                    br(),
                                                    textInput("marker", "Insert marker genes", value = "CD34", width = NULL, placeholder = NULL),
                                                    fileInput("markerfile", "Load text file with marker genes ",buttonLabel=list(icon("upload")),
                                                              multiple = FALSE),
                                                    pickerInput('diseaseM', 'Condition', selected = "healthy", choices= disease_list, multiple=TRUE, options = list(`actions-box` = TRUE,`live-search`=TRUE, style="box-celltypes")),
                                                    pickerInput('tissueM', 'Tissue',  choices= NULL,selected=tissue_list, multiple=TRUE, options = list(`actions-box` = TRUE,`live-search`=TRUE, style="box-celltypes")),
                                                    checkboxInput("tissue_awareM","Tissue aware",value=TRUE),
                                                    
                                                    checkboxInput("cellidM","Plot celltype_ID",value=FALSE))),
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
                  conditionalPanel(id="testM",condition= "input.plot1M_click",wellPanel(textOutput("celltype_defM"))),
                  br(),
                  br(),
                  titlePanel(shiny::span(p(style="text-align: justify;", HTML("<h>Table options</h>"),actionButton('helpM', 'Info',icon= icon("info"), align="left")))),                  
                  fluidRow(column(width=12,wellPanel(id="sidebar2M",
                                                     fluidRow(column(3,radioButtons('EC_scoreM','EC_score', c(">=1",">=2",">=3",">=4",">=5",">=6",">=7"), selected = ">=1")),
                                                              column(3,radioButtons('specificityM','Specificity_score', c(">=0",">=0.25",">=0.5","=1"),selected = ">=0")),
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
                  # bsTooltip("EC_score", "Evidence consensus score (number of databases)", placement = "top", trigger = "hover",
                  #           options = NULL),
                  br(),
                  br(),
                  dataTableOutput('table1M'),
                  br()
                  )),
      
      
##### SEMI-AUTOMATIC ANNOTATION -----
      tabItem(tabName="anno", 
              div(fluidRow(column(width=6,wellPanel(id="sidebar",
                                                    #textInput("marker", "Insert marker genes", value = "CD34", width = NULL, placeholder = NULL),
                                                    splitLayout(cellWidths = c("75%", "25%"),fileInput("clusterfile", "Load file to annotate",buttonLabel=list(icon("upload")),multiple = FALSE),
                                                                actionButton('userclusterfileinfo', 'InputFile',icon= icon("file-circle-question"), align="left", style='margin-top:30px; margin-bottom:0px')),
                                                    checkboxGroupInput("speciesA", "Select species:",
                                                                       choiceNames =
                                                                         list(tags$img(src = "human.png"),tags$img(src = "mouse.png")),
                                                                       choiceValues =
                                                                         list("Human","Mouse"),selected = c("Human"),inline=TRUE),
                                                    pickerInput('diseaseA', 'Condition',  choices= disease_list, selected = "healthy", multiple=TRUE, options = list(`actions-box` = TRUE,`live-search`=TRUE, style="box-celltypes")),
                                                    pickerInput('tissueA', 'Tissue',  choices= tissue_list,selected="blood", multiple=TRUE, options = list(`actions-box` = TRUE,`live-search`=TRUE, style="box-celltypes")),
                                                    #checkboxInput("collapse_tissue","",value=FALSE))),
                                                    checkboxInput("tissue_awareA","Tissue aware",value=TRUE),
                                                    fluidRow(column(width=6,radioButtons("nmarkerpos", HTML("How many <em>positive</em> markers you want for each cluster?"), choices = nmarker_values)),
                                                             column(width=6, radioButtons("nmarkerneg", HTML("How many <em>negative</em> markers you want for each cluster?"), choices = nmarker_values))),
                                                    fluidRow(column(width=6,textInput("nmarkerotherpos", HTML("Type in number of <em>positive</em> markers"))),
                                                             column(width=6,textInput("nmarkerotherneg", HTML("Type in number of <em>negative</em> markers")))),
                                                    fluidRow(column(width=6,actionButton("addpos", HTML("Add value"))),
                                                             column(width=6,actionButton("addneg", HTML("Add value")))),
                                                    
                                                    style = "padding-bottom: 5px;")),
                           column(6, offset=0, uiOutput("example_panel"))),
                  br(),
                  
                  fluidRow(column(width=6, wellPanel(id="sidebar",fluidRow(shiny::span(p(HTML("<h7>Filters</h7>"),actionButton('filterhelpA', 'Info',icon= icon("info"), align="center"))),
                                                                  column(width=6,radioButtons('EC_scoreA','EC_score', c(">=1",">=2",">=3",">=4",">=5",">=6",">=7"),selected = ">=1")),
                                                                  column(width=6, radioButtons('specificityA','specificity_score', c(">=0",">=0.25",">=0.5","=1"),selected = ">=0")),
                                                                                                                                    ),
                                            style = "padding-bottom: 13px;")),
                  
                  column(6,div(img(src = "Logo.png",height="50%", width="50%"),style="text-align: center;"), 
                         br(),
                         actionButton("button", HTML("<strong>Click to annotate!"),style="font-size: 24px;",icon= icon("rocket")), align="center")),
                  
                  conditionalPanel(
                    condition= "output.table1A", fluidRow(column(4,radioButtons("downloadTypeA", "Download Format", choices = c("CSV" = ".csv",
                                                                                                                                "XLSX" = ".xlsx",
                                                                                                                                "TSV" = ".tsv"),inline = TRUE),
                                                                 column(4,downloadButton("downloadDataA", "Download"))))
                  ),
                  br(),          
                  dataTableOutput('table1A'),
                  br(),
                  br(), 
                  
                  #change style sliderinput
                  tags$style(HTML(".js-irs-4 .irs-single, .js-irs-4 .irs-bar-edge, .js-irs-4 .irs-bar {background: #990000}")),
                  tags$style(HTML(".js-irs-5 .irs-single, .js-irs-5 .irs-bar-edge, .js-irs-5 .irs-bar {background: #990000}")),
                  conditionalPanel(
                    condition= "output.table1A" ,titlePanel(shiny::span((icon("sliders",class = "about-icon fa-pull-left", lib = "font-awesome")), p(style="text-align: justify;", HTML("<h>Adjust plot size </h>")))),
                    fluidRow(column(width=6,sliderInput(inputId = "heightA", label = "Height", min = 200, max = 3500, value = 400, step = 200,width="80%")),
                             column(width=6,sliderInput(inputId = "widthA", label = "Width", min = 200, max = 3500, value = 400, step=200,width="80%"))),
                    checkboxInput("cellidA","Plot celltype_ID",value=FALSE),
                    #Ontology plot
                    titlePanel(shiny::span((icon("hand-pointer",class = "about-icon fa-pull-left", lib = "font-awesome")), p(style="text-align: justify;", HTML("<h> <strong> Click </strong> on a node to look at cell type description</h>")))),
                    fluidRow(column(12,align="center",div(style='width:100%;overflow-x: scroll;height:100%;overflow-y: scroll;', uiOutput('scalableplotA')))),
                  ),
                  #grVizOutput("plot1"),
                  tags$style(
                    '#testA {
                              cursor: grap;
                              color: black;
                            }'),
                  conditionalPanel(id="testA",condition= "input.plot1A_click",wellPanel(textOutput("celltype_defA"))),
                  br(),
                  tableOutput("checker"),
                  tags$head(tags$style(HTML('
         #sidebar {
            background-color: #ad000019;
            font-size: 16px;
        }'))),
                  
                  
                  
                  tags$head(tags$style(HTML('
         #sidebar2A {
            background-color: #ad000019;
            font-size: 16px;
        }'))),
                  # bsTooltip("EC_score", "Evidence consensus score (number of databases)", placement = "top", trigger = "hover",
                  #           options = NULL),
                  br(),
                  br()
        ))
    )
  )
  
)
server <- function(input, output, session) {
  
  ###### server for cell type search ----
  
  observeEvent(input$usermarkerinfo,{
    showModal(modalDialog(
      title = "Marker genes input file",
      HTML("Add your list of marker genes to be integrated with the Accordion. <br>
      The file must contain at least two columns:
             <ul><li> <strong> 1. celltype </strong>: cell types names </li>
             <li> <strong> 2. marker </strong>: marker genes </li>
             You can also provide additional columns:
             <li> <strong> 3. marker_type </strong>: marker type (default positive) <br> <ul><li> positive: whether the gene is a positive marker 
             </li><li> negative: whether the gene is a negative marker </li></ul>
             <li> <strong> 4. species </strong>: species (default Human. You can also select the species using the above box) <br> <ul><li> Human </li><li> Mouse </li></ul>
             <li> <strong> 5. tissue </strong>: tissue (by default tissues selected in the input box are used) </li>
             <li> <strong> 6. condition </strong>: condition (by default tissues selected in the input box are used) </li></ul>
             <br>
           <strong> Example </strong>"),
      HTML("<img src=input_file_example.jpg>"),
      easyClose = TRUE,
      footer = NULL
    ))
  })
  
  
  observeEvent(input$help,{
    showModal(modalDialog(
      title = "Table Option Information",
      HTML("Marker genes can be ranked and selected by: <br>
      <ul><li> <strong> ECs </strong>: evidence consistency score, measuring the agreement of different annotation sources </li>
       <li> <strong> database specificity score </strong>: whether a gene is a marker for different cell types present in all the accordion database </li>
        <li> <strong> query specificity score</strong> : whether a gene is a marker for different cell types present in the query input </li></ul>

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
  
  markerTableInput <-reactive ({
    if(!is.null(input$usermarker)){
      file_load_marker<-input$usermarker
      fileName_marker <- file_load_marker$datapath
      ext <-  tools::file_ext(fileName_marker)
      req(file_load_marker)
      validate(need(ext %in% c("csv", "xlsx","txt"), "Please upload a csv, txt or xlsx file"))
      if(ext=="xlsx"){
        user_inputfile<-read_excel(fileName_marker)
      }
      if(ext=="csv" | ext == "txt" | ext == "tsv"){
        user_inputfile<-fread(fileName_marker)
        
      }
      user_inputfile<-as.data.table(user_inputfile)
      validate(need(ncol(user_inputfile) >=2, "Insufficient number of columns! Need at least cell type and marker columns"))
      
      if(ncol(user_inputfile)==2){
        colnames(user_inputfile)<-c("celltype","marker")
        user_inputfile[,marker_type:="positive"]
        user_inputfile[,species:= input$species]
        user_inputfile[,Uberon_tissue:= input$tissue]
        user_inputfile[,DO_diseasetype:= input$disease]
        
      } else {
        if(!"marker_type" %in%colnames(user_inputfile)){
          user_inputfile[,marker_type:="positive"]
        } 
        if(!"species" %in%colnames(user_inputfile)){
          user_inputfile[,species:=input$species]
        }
        if(!"tissue" %in%colnames(user_inputfile)){
          user_inputfile[,Uberon_tissue:=input$tissue]
        }
        if(!"condition" %in%colnames(user_inputfile)){
          user_inputfile[,DO_diseasetype:=input$condition]
        }
      }
        user_inputfile<-user_inputfile[,c("species","DO_diseasetype","Uberon_tissue","celltype","marker","marker_type")]
       
      #merge user marker with ontology
      # user_inputfile<-merge(user_inputfile, ontology_celltype,by="celltype")
      # user_inputfile<-merge(user_inputfile,ontology_def[,-"celltype"],by="celltype_ID")
      # 
      # dt_hs<-merge(user_inputfile[species=="Human"],description_hs[,c("gene_name","gene_description")],by.x="marker",by.y="gene_name",all.x=TRUE)
      # dt_hs_na<-dt_hs[is.na(gene_description)]
      # dt_hs_na<-merge(dt_hs_na[,-("gene_description")],description_hs[,c("gene_synonym","gene_description")],by.x="marker",by.y="gene_synonym",all.x=TRUE)
      # dt_hs<-rbind(dt_hs[!(is.na(gene_description))],dt_hs_na)
      # 
      # dt_mm<-merge(user_inputfile[species=="Mouse"],description_mm[,c("gene_name","gene_description")],by.x="marker",by.y="gene_name",all.x=TRUE)
      # dt_mm_na<-dt_mm[is.na(gene_description)]
      # dt_mm_na<-merge(dt_mm_na[,-("gene_description")],description_mm[,c("gene_synonym","gene_description")],by.x="marker",by.y="gene_synonym",all.x=TRUE)
      # dt_mm<-rbind(dt_mm[!(is.na(gene_description))],dt_mm_na)
      # 
      user_inputfile<-unique(user_inputfile)
      
      #add column celltype_species (for app)
      double_species<-unique(user_inputfile[,c("species","celltype")])
      dup<-double_species[duplicated(celltype)]
      user_inputfile[, celltype_species:=ifelse(celltype %in% dup$celltype, paste0(celltype, " (Hs, Mm)"), 
                                              ifelse(species =="Human", paste0(celltype, " (Hs)"),
                                                     paste0(celltype, " (Mm)")))]
      
      user_inputfile[,original_diseasetype:=NA]
      user_inputfile[,DO_ID:=NA]
      user_inputfile[,DO_definition:=NA]
      user_inputfile[,original_tissue:=NA]
      user_inputfile[,Uberon_ID:=NA]
      user_inputfile[,original_celltype:=NA]
      user_inputfile[,celltype_ID:=NA]
      user_inputfile[,cell_definition:=NA]
      user_inputfile[,gene_description:=NA]
      user_inputfile[,resource:="custom_set"]
      user_inputfile[,log2FC:=NA]
      user_inputfile[,p.value:=NA]
      user_inputfile[,adjusted_p.value:=NA]
      user_inputfile[,pct1:=NA]
      user_inputfile[,EC_score_global:=NA]
      user_inputfile[,specificity_global:=NA]
      user_inputfile<-user_inputfile[,c("species","original_diseasetype","DO_diseasetype","DO_ID","DO_definition", "original_tissue","Uberon_tissue","Uberon_ID","original_celltype","celltype","celltype_species","celltype_ID","cell_definition", "marker","gene_description","marker_type", "resource", "log2FC", "p.value", "adjusted_p.value","pct1","EC_score_global","specificity_global")]
      
      #union of accordion markers and user marker
      combine_table_long<-rbind(user_inputfile,accordion_complete)
      combine_table_long<-unique(combine_table_long)
      combine_table_long
    }
    else {
      accordion_complete
    }
  }) 
  
  markerTableComplete <- reactive({
    validate(need(length(input$disease) >=1, "Please select at least one condition"))
    accordion_complete<-accordion_complete[DO_diseasetype %in% input$disease & species %in% input$species]
    accordion_complete

})
  
  toListen_tissue <- reactive({
    list(input$disease,input$species,input$usermarker, input$disease)
  })
  
  observeEvent(toListen_tissue(),{
    updatePickerInput(session,'tissue', selected=tissue_list,
                      choices=unique(accordion_complete[DO_diseasetype %in% input$disease & species %in% input$species]$Uberon_tissue),
                      option=list(`actions-box` = TRUE,style="box-celltypes"),
                      choicesOpt = list(style = rep(("font-size: 18px; line-height: 1.6;"), uniqueN(accordion_complete$Uberon_tissue))))
  })
  
  toListen <- reactive({
    list(input$species,input$usermarker, input$disease, input$tissue)
  })
  
  observeEvent(toListen(),{
    updatePickerInput(session,'celltype', selected=c("hematopoietic stem cell (Hs, Mm)", "hematopoietic multipotent progenitor cell (Hs, Mm)","hematopoietic lineage restricted progenitor cell (Hs, Mm)"),
                      choices=unique(accordion_complete[DO_diseasetype %in% input$disease & species %in% input$species & Uberon_tissue %in% input$tissue]$celltype_species),
                      option=list(`actions-box` = TRUE,style="box-celltypes"),
                      choicesOpt = list(style = rep(("font-size: 18px; line-height: 1.6;"), uniqueN(accordion_complete$celltype))))
  })

  
  controlDescendant<-reactive({
    if("healthy" %in% input$disease){
    onto_igraph<-graph_from_graphnel(onto_plot, name = TRUE, weight = TRUE, unlist.attrs = TRUE)
    node<-as.data.table(unique(markerTableComplete()[celltype_species %in% input$celltype & DO_diseasetype=="healthy"]$celltype))
    dt<- data.table(celltype=character(), distance=numeric())
    for (i in 1:nrow(node)){
      distan<-max(eccentricity(onto_igraph, vids = node[i]$V1, mode = c("out")))
      data<-as.data.table(t(c(node[i]$V1,distan)))
      colnames(data)<-c("celltype","distance")
      dt<-rbind(dt,data)
    }
    as.data.table(dt)
    }
    data.table(celltype="", distance =0)
  })

  
  observeEvent(input$celltype,{
    updatePickerInput(session,'descendantsof',
                      choices=unique(markerTableComplete()[species %in% input$species & Uberon_tissue %in% input$tissue & celltype %in% controlDescendant()[distance!=0]$celltype]$celltype_species),
                      option=list(`actions-box` = TRUE,style="box-celltypes"),
                      choicesOpt = list(style = rep(("font-size: 18px; line-height: 1.6;"), 105)))
  })

  descendantTable<-reactive ({
    if(length(input$descendantsof)>0){
      select_descendant(onto_plot, markerTableComplete(), input$descendantsof)
    }

  })

  mergeDescendantTable<-reactive ({
    if(length(input$descendantsof)>0 & input$mergeDescendant=="Yes"){
      table_merge_descendant(onto_plot, cell_onto, markerTableComplete(), input$descendantsof,input$species)
    }
  })

  markerTablePreOutput <- reactive ({
    
    #table with descandants merged
    if(length(input$descendantsof)!=0 & input$mergeDescendant=="Yes"){
      table_merged_desc_other(markerTableComplete(),input$celltype,mergeDescendantTable(),input$species,colnames(accordion_complete))
    }
    #table with descendants NOT merged
    else if (length(input$descendantsof)!=0 & input$mergeDescendant=="No"){
      table_desc_other(markerTableComplete(),input$celltype,descendantTable(),input$species,colnames(accordion_complete))
    } #table without descendant
    else if(length(input$descendantsof)==0){
      table_input_celltypes(markerTableComplete(),input$celltype,input$species,input$tissue, input$disease)
    }

  })
  
  markerTableOutput <- reactive ({
    if(!is.null(markerTablePreOutput())){
        
      #keep tissue separated 
      if(input$tissue_aware == TRUE){
        #compute EC and specificity condition&tissue specific
        
        #EC score
        st_table_tissue_specific<-unique(markerTablePreOutput()[,c("DO_diseasetype","DO_ID","Uberon_tissue","Uberon_ID","species","celltype","celltype_ID","marker","marker_type","resource")])
        st_table_tissue_specific<-st_table_tissue_specific[!is.na(celltype)]
        ec_score_tissue_specific<-ddply(st_table_tissue_specific,.(species,DO_diseasetype,DO_ID,Uberon_tissue,Uberon_ID,celltype,celltype_ID,marker,marker_type),nrow)
        colnames(ec_score_tissue_specific)<-c("species","DO_diseasetype","DO_ID","Uberon_tissue","Uberon_ID","celltype","celltype_ID","marker", "marker_type","EC_score")
        accordion_ec_table<-merge(markerTablePreOutput(),ec_score_tissue_specific,by=c("species","DO_diseasetype","DO_ID","Uberon_tissue","Uberon_ID","celltype","celltype_ID","marker", "marker_type"), all.x = TRUE)
        
        #specificity
        st_table_tissue_specific<-unique(accordion_ec_table[,c("DO_diseasetype","DO_ID","Uberon_tissue","Uberon_ID","species","celltype","celltype_ID","marker","marker_type","marker","marker_type")])
        st_table_tissue_specific<-st_table_tissue_specific[!is.na(celltype)]
        
        mark_spec<-ddply(st_table_tissue_specific,.(species, DO_diseasetype,DO_ID,Uberon_tissue,Uberon_ID, marker,marker_type),nrow)
        colnames(mark_spec)<-c("species","DO_diseasetype","DO_ID","Uberon_tissue","Uberon_ID","marker","marker_type", "specificity_score")
        
        final_table<-merge(accordion_ec_table,mark_spec,by=c("species","DO_diseasetype","DO_ID","Uberon_tissue","Uberon_ID","marker","marker_type"),all.x = TRUE)
        final_table[,specificity_score:=format(round(1/specificity_score,2), nsmall=2)]
        
        final_table<-final_table[,c("species","original_diseasetype","DO_diseasetype","DO_ID","DO_definition", "original_tissue","Uberon_tissue","Uberon_ID","original_celltype","celltype","celltype_ID","cell_definition", "marker","gene_description","marker_type", "resource", "log2FC", "p.value", "adjusted_p.value","pct1", "EC_score","specificity_score")]
      } else{ #not consider tissue
        #EC_score
        st_table_tissue_specific<-unique(markerTablePreOutput()[,c("DO_diseasetype","DO_ID","species","celltype","celltype_ID","marker","marker_type","resource")])
        st_table_tissue_specific<-st_table_tissue_specific[!is.na(celltype)]
        
        ec_score_tissue_specific<-ddply(st_table_tissue_specific,.(species,DO_diseasetype,DO_ID,celltype,celltype_ID,marker,marker_type),nrow)
        
        colnames(ec_score_tissue_specific)<-c("species","DO_diseasetype","DO_ID","celltype","celltype_ID","marker", "marker_type","EC_score")
        accordion_ec_table<-merge(markerTablePreOutput(),ec_score_tissue_specific,by=c("species","DO_diseasetype","DO_ID","celltype","celltype_ID","marker", "marker_type"), all.x = TRUE)
        
        #specificity
        st_table_tissue_specific<-unique(accordion_ec_table[,c("DO_diseasetype","DO_ID","species","celltype","celltype_ID","marker","marker_type","marker","marker_type")])
        st_table_tissue_specific<-st_table_tissue_specific[!is.na(celltype)]
        mark_spec<-ddply(st_table_tissue_specific,.(species, DO_diseasetype,DO_ID, marker,marker_type),nrow)
        
        colnames(mark_spec)<-c("species","DO_diseasetype","DO_ID","marker","marker_type", "specificity_score")
        
        final_table<-merge(accordion_ec_table,mark_spec,by=c("species","DO_diseasetype","DO_ID","marker","marker_type"),all.x = TRUE)
        final_table[,specificity_score:=format(round(1/specificity_score,2), nsmall=2)]
        final_table<-final_table[,c("species","original_diseasetype","DO_diseasetype","DO_ID","DO_definition","original_celltype","celltype","celltype_ID","cell_definition", "marker","gene_description","marker_type", "resource", "log2FC", "p.value", "adjusted_p.value","pct1", "EC_score","specificity_score")]
      }
    }
  })
  
  markerTablePlot <- reactive ({
    if(!is.null(markerTableComplete())){
      #plot with descendants
      if (length(input$descendantsof)!=0){
        table_desc_other(markerTableComplete(),input$celltype,descendantTable(),input$species,colnames(accordion_complete))
      }
      else if(length(input$descendantsof)==0){
        table_input_celltypes(markerTableComplete(),input$celltype,input$species,input$tissue, input$disease)
      }
    }
  })

  Plot <- reactive({
    if(!is.null(markerTablePlot())){
       healthy_ct<-markerTablePlot()[DO_diseasetype=="healthy"]$celltype_ID
      if("healthy" %in% input$disease){
      if (length(healthy_ct) > 1 | length(input$descendantsof)>=1){  
        ontosubplot<-onto_plot2(cell_onto,markerTablePlot()[DO_diseasetype=="healthy"]$celltype_ID ,cex=0.8)
        nodes<-as.data.table(ontosubplot@nodes)
        nodes<-nodes[,V1:=tstrsplit(nodes$V1,"CL", fixed = TRUE, keep = 1)]
        nodes<-nodes[,V1:=tstrsplit(nodes$V1,"DOID", fixed = TRUE, keep = 1)]
        
        nodes<-nodes[, V1:=str_replace_all(V1,"\n"," ")]
        nodes<-nodes[, V1:=str_sub(V1,1,nchar(V1)-1)]
        ontosubplot@nodes<-nodes$V1
        ontosubplot   
      } else if(length(healthy_ct) == 1 & length(input$tissue) > 0){
        ontosubplot<-onto_plot(cell_onto,markerTablePlot()[DO_diseasetype=="healthy"]$celltype_ID ,cex=0.8)
        ontosubplot2<-make_graphNEL_from_ontology_plot(ontosubplot)
        dt_onto2<-as.data.table(ontosubplot2@nodes)
        label<-merge(dt_onto2,ontology_celltype,by.x="V1",by.y="celltype_ID")
        ontosubplot[["node_attributes"]][["label"]][[label$V1]]<-label$celltype
        ontosubplot2@nodes<-ontosubplot[["node_attributes"]][["label"]]
  
        ontosubplot2
        
      }
      }
    }
  })


  output$plot1 <- renderGrViz({
    if(!is.null(markerTablePlot())){
      healthy_ct<-markerTablePlot()[DO_diseasetype=="healthy"]$celltype_ID
      if(length(healthy_ct>=1)){
        if(length(input$descendantsof)>=1){
          hierac_plot1_desc(markerTableComplete(),Plot(),healthy_ct,descendantTable(),input$cellid, input$disease)
        }else{
          hierac_plot1(markerTableComplete(),Plot(),healthy_ct,input$cellid, input$disease)
        }
      } 
    }
  })

  output$scalableplot <- renderUI({
    tagList(
      div(grVizOutput('plot1',height = input$height, width = input$width)))
  })



  click_plot<-reactive ({
    click_node(accordion_complete,ontology_celltype,Plot(),markerTableComplete(),input$cellid, ontology_def, input$disease)
  })


  txt <- reactive({
    req(input$plot1_click)
    nodeval <- input$plot1_click$nodeValues[1:length(input$plot1_click$nodeValues)]
    nodeval<-as.data.table(paste(nodeval,sep="", collapse=" "))
    return(click_plot()[label %in% nodeval$V1]$cell_definition)

  })
  output$celltype_def <- renderText({
    req(txt())
    txt()
  })


  outputTable<-reactive({
    if(!is.null(markerTableOutput())){
    if(length(input$celltype)!=0 & input$tabletype=="Complete" & input$tissue_aware==TRUE){
      if(input$mergeDescendant=="Yes"){
        markerTableOutput()[EC_score>= str_replace_all(input$EC_score, ">=","") & specificity_score >= str_replace_all(input$specificity, ">=|=","")][,
        c("species","original_diseasetype","DO_diseasetype","DO_ID","DO_definition","original_tissue","Uberon_tissue","Uberon_ID","original_celltype","celltype_ancestor","celltype","celltype_ID","cell_definition","marker", "marker_type","gene_description","EC_score","specificity_score","resource","log2FC","p.value","adjusted_p.value","pct1")]
      }else{
        markerTableOutput()[EC_score>= str_replace_all(input$EC_score, ">=","") & specificity_score >= str_replace_all(input$specificity, ">=|=","")][,
                                                                                                                                                      c("species","original_diseasetype","DO_diseasetype","DO_ID","DO_definition","original_tissue","Uberon_tissue","Uberon_ID","original_celltype","celltype","celltype_ID","cell_definition","marker", "marker_type","gene_description","EC_score","specificity_score","resource","log2FC","p.value","adjusted_p.value","pct1")]
        }
    } else if(length(input$celltype)!=0 & input$tabletype=="Complete" & input$tissue_aware==FALSE){
      if(input$mergeDescendant=="Yes"){
        markerTableOutput()[EC_score>= str_replace_all(input$EC_score, ">=","") & specificity_score >= str_replace_all(input$specificity, ">=|=","")][,
                                                                                                                                                      c("species","original_diseasetype","DO_diseasetype","DO_ID","DO_definition","original_celltype","celltype_ancestor","celltype","celltype_ID","cell_definition","marker", "marker_type","gene_description","EC_score","specificity_score","resource","log2FC","p.value","adjusted_p.value","pct1")]
      }else{
        markerTableOutput()[EC_score>= str_replace_all(input$EC_score, ">=","") & specificity_score >= str_replace_all(input$specificity, ">=|=","")][,
                                                                                                                                                      c("species","original_diseasetype","DO_diseasetype","DO_ID","DO_definition","original_celltype","celltype","celltype_ID","cell_definition","marker", "marker_type","gene_description","EC_score","specificity_score","resource","log2FC","p.value","adjusted_p.value","pct1")]
      }
    } else if(length(input$celltype)!=0 & input$tabletype=="Complete" & input$tissue_aware==TRUE){
      if(input$mergeDescendant=="Yes"){
        markerTableOutput()[EC_score>= str_replace_all(input$EC_score, ">=","") & specificity_score >= str_replace_all(input$specificity, ">=|=","")][,
        c("species","original_diseasetype","DO_diseasetype","DO_ID","DO_definition","original_tissue","Uberon_tissue","Uberon_ID","original_celltype","celltype_ancestor","celltype","celltype_ID","cell_definition","marker", "marker_type","gene_description","EC_score","specificity_score","resource","log2FC","p.value","adjusted_p.value","pct1")]
      }else{
        markerTableOutput()[EC_score>= str_replace_all(input$EC_score, ">=","") & specificity_score >= str_replace_all(input$specificity, ">=|=","")][,
                                                                                                                                                      c("species","original_diseasetype","DO_diseasetype","DO_ID","DO_definition","original_tissue","Uberon_tissue","Uberon_ID","original_celltype","celltype","celltype_ID","cell_definition","marker", "marker_type","gene_description","EC_score","specificity_score","resource","log2FC","p.value","adjusted_p.value","pct1")]
        }
    } else if(length(input$celltype)!=0 & input$tabletype=="Complete" & input$tissue_aware==FALSE){
      if(input$mergeDescendant=="Yes"){
        markerTableOutput()[EC_score>= str_replace_all(input$EC_score, ">=","") & specificity_score >= str_replace_all(input$specificity, ">=|=","")][,
                                                                                                                                                      c("species","original_diseasetype","DO_diseasetype","DO_ID","DO_definition","original_celltype","celltype_ancestor","celltype","celltype_ID","cell_definition","marker", "marker_type","gene_description","EC_score","specificity_score","resource","log2FC","p.value","adjusted_p.value","pct1")]
      }else{
        markerTableOutput()[EC_score>= str_replace_all(input$EC_score, ">=","") & specificity_score >= str_replace_all(input$specificity, ">=|=","")][,
                                                                                                                                                      c("species","original_diseasetype","DO_diseasetype","DO_ID","DO_definition","original_celltype","celltype","celltype_ID","cell_definition","marker", "marker_type","gene_description","EC_score","specificity_score","resource","log2FC","p.value","adjusted_p.value","pct1")]
      }
    }  else if(length(input$celltype)!=0 & input$tabletype=="Simple" & input$tissue_aware==TRUE){
      if(input$mergeDescendant=="Yes"){
        unique(markerTableOutput()[EC_score>= str_replace_all(input$EC_score, ">=","") & specificity_score >= str_replace_all(input$specificity, ">=|=","")][,
                                                                                                                                                      c("species","DO_diseasetype","DO_ID","Uberon_tissue","Uberon_ID","celltype_ancestor","celltype","celltype_ID","marker", "marker_type","EC_score","specificity_score")])
      }else{
        unique(markerTableOutput()[EC_score>= str_replace_all(input$EC_score, ">=","") & specificity_score >= str_replace_all(input$specificity, ">=|=","")][,
                                                                                                                                                      c("species","DO_diseasetype","DO_ID","Uberon_tissue","Uberon_ID","celltype","celltype_ID","marker", "marker_type","EC_score","specificity_score")])
      }
    } else if(length(input$celltype)!=0 & input$tabletype=="Simple" & input$tissue_aware==FALSE){
      if(input$mergeDescendant=="Yes"){
        unique(markerTableOutput()[EC_score>= str_replace_all(input$EC_score, ">=","") & specificity_score >= str_replace_all(input$specificity, ">=|=","")][,
                                                                                                                                                      c("species","DO_diseasetype","DO_ID","celltype_ancestor","celltype","celltype_ID","marker", "marker_type","EC_score","specificity_score")])
      }else{
        unique(markerTableOutput()[EC_score>= str_replace_all(input$EC_score, ">=","") & specificity_score >= str_replace_all(input$specificity, ">=|=","")][,
                                                                                                                                                      c("species","DO_diseasetype","DO_ID","celltype","celltype_ID","marker", "marker_type","EC_score","specificity_score")])
      }
    } 
    }

  })

  output$table1 <- renderDataTable({
    if(!is.null(outputTable())){
    outputTable()
    }
  })


  # Downloadable csv of selected dataset

  output$downloadData <- downloadHandler(
    filename = function() {
      paste0("accordion_complete", input$downloadType)
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





  #### server for marker search ----
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
      marker_dt<-tolower(accordion_complete$marker)
      validate(need(marker_input %in% marker_dt, "Please insert valid marker genes!"))
      gene_marker<-accordion_complete[tolower(marker) %in% tolower(marker_vec$marker_gene)]
      gene_marker<-gene_marker[species %in% input$speciesM]
      gene_marker
      if(length(input$diseaseM>0)){
        gene_marker<- gene_marker[DO_diseasetype %in% input$diseaseM]
        gene_marker
        }
      
    }
  })
  
  #table base on input file genes marker 
  tableFileMarker<-reactive ({
    file_load<-input$markerfile
    fileName <- file_load$datapath    
    mark<-readChar(fileName, file.info(fileName)$size)
    marker_vec<-as.data.frame(unlist(strsplit(mark, "[\\|, \r\n+]+")))
    colnames(marker_vec)<-"marker_gene"
    table_marker_file<-accordion_complete[tolower(marker) %in% tolower(marker_vec$marker_gene)]
    table_marker_file <- table_marker_file[species %in% input$speciesM]
    if(length(input$diseaseM>0)){
      table_marker_file<- table_marker_file[DO_diseasetype %in% input$diseaseM]

      }
    table_marker_file
  })
  
  # Final table output: 
  #1. Marker insert by the user in the box 
  #2. Marker add through a marker file 
  #3. If both marker in the box and file marker are present add together 
  
  tableInputComplete_notissue<-reactive({
    if(nchar(input$marker)>1 & (!is.null(input$markerfile))){
      table_tot<-rbind(tableMarkerInput(),tableFileMarker())
      if(length(input$diseaseM)>0){
      table_tot<-table_tot[DO_diseasetype %in% input$diseaseM]
      
      }
      table_tot
    }else if(nchar(input$marker)>1  & is.null(input$markerfile)){
      table_tot<-tableMarkerInput()
      if(length(input$diseaseM)>0){
      table_tot<-table_tot[DO_diseasetype %in% input$diseaseM]
      }
      table_tot
    }else if (nchar(input$marker)==0 & !is.null(input$markerfile)) {
      table_tot<-tableFileMarker()
      if(length(input$diseaseM)>0){
      table_tot<-table_tot[DO_diseasetype %in% input$diseaseM]
      
      }
      table_tot
    }
  })
  


  toListen_tissueM <- reactive({
    list(input$speciesM, input$diseaseM, tableInputComplete_notissue())
  })

  observeEvent(toListen_tissueM(),{
    updatePickerInput(session,'tissueM',selected=tableInputComplete_notissue()$Uberon_tissue,
                      choices=unique(tableInputComplete_notissue()$Uberon_tissue),
                      option=list(`actions-box` = TRUE,style="box-celltypes"),
                      choicesOpt = list(style = rep(("font-size: 18px; line-height: 1.6;"), uniqueN(tableInputComplete_notissue()$Uberon_tissue))))
  })

  
  tableInputComplete <- reactive ({
    if(!is.null(tableInputComplete_notissue())){
      tableInputComplete_notissue()[Uberon_tissue %in% input$tissueM]
    }
  })
  
  PlotM <- reactive({
    healthy_ct<-tableInputComplete()[DO_diseasetype =="healthy"]$celltype_ID
    if (length(healthy_ct)>=1 & "healthy" %in% input$diseaseM) {
      cell_matching<-unique(tableInputComplete()$celltype_ID)
      if(length(cell_matching)>1){
        ontosubplot<-onto_plot2(cell_onto,cell_matching ,cex=0.8)
        nodes<-as.data.table(ontosubplot@nodes)
        nodes<-nodes[,V1:=tstrsplit(nodes$V1,"CL", fixed = TRUE, keep = 1)]
        nodes<-nodes[,V1:=tstrsplit(nodes$V1,"DOID", fixed = TRUE, keep = 1)]
        nodes<-nodes[, V1:=str_replace_all(V1,"\n"," ")]
        nodes<-nodes[, V1:=str_sub(V1,1,nchar(V1)-1)]
        ontosubplot@nodes<-nodes$V1
        ontosubplot
      } else{
        ontosubplot<-onto_plot(cell_onto,cell_matching ,cex=0.8)
        ontosubplot2<-make_graphNEL_from_ontology_plot(ontosubplot)
        dt_onto2<-as.data.table(ontosubplot2@nodes)
        label<-merge(dt_onto2,ontology_celltype,by.x="V1",by.y="celltype_ID")
        ontosubplot[["node_attributes"]][["label"]][[label$V1]]<-label$celltype        
        ontosubplot2@nodes<-ontosubplot[["node_attributes"]][["label"]]
        ontosubplot2
        
      }
      
    }
    
  })
  
  
  
  
  output$plot1M <- renderGrViz({ 
    if(!is.null(tableInputComplete())){
      healthy_ct<-tableInputComplete()[DO_diseasetype=="healthy"]$celltype_ID
      if(length(healthy_ct>=1)){
        hierac_plot2(accordion_complete, PlotM(), tableInputComplete(),input$cellidM, input$diseaseM) 
      }
    }
    
  })
  
  output$scalableplotM <- renderUI({ 
    tagList(
      div(grVizOutput('plot1M',height = input$heightM, width = input$widthM)))
  })
  
  
  
  click_plotM<-reactive ({
    click_node(accordion_complete, ontology_celltype, PlotM(), tableInputComplete(),input$cellidM, ontology_def, input$diseaseM)
  })
  
  
  txtM <- reactive({
    req(input$plot1M_click)
    nodeval <- input$plot1M_click$nodeValues[1:length(input$plot1M_click$nodeValues)]
    nodeval<-as.data.table(paste(nodeval,sep="", collapse=" "))
    return(  click_plotM()[label %in% nodeval$V1]$cell_definition)
    
  })
  output$celltype_defM <- renderText({
    req(txtM())
    txtM()
  })
  
  
  
  
  markerTableOutputM <- reactive ({
    if(!is.null(tableInputComplete())){
      
    #keep tissue separated 
    if(input$tissue_awareM == TRUE){
      #compute EC and specificity condition&tissue specific
      
      #EC score
      st_table_tissue_specific<-unique(tableInputComplete()[,c("DO_diseasetype","DO_ID","Uberon_tissue","Uberon_ID","species","celltype","celltype_ID","marker","marker_type","resource")])
      st_table_tissue_specific<-st_table_tissue_specific[!is.na(celltype)]
      ec_score_tissue_specific<-ddply(st_table_tissue_specific,.(species,DO_diseasetype,DO_ID,Uberon_tissue,Uberon_ID,celltype,celltype_ID,marker,marker_type),nrow)
      colnames(ec_score_tissue_specific)<-c("species","DO_diseasetype","DO_ID","Uberon_tissue","Uberon_ID","celltype","celltype_ID","marker", "marker_type","EC_score")
      accordion_ec_table<-merge(tableInputComplete(),ec_score_tissue_specific,by=c("species","DO_diseasetype","DO_ID","Uberon_tissue","Uberon_ID","celltype","celltype_ID","marker", "marker_type"), all.x = TRUE)
      
      #specificity
      st_table_tissue_specific<-unique(accordion_ec_table[,c("DO_diseasetype","DO_ID","Uberon_tissue","Uberon_ID","species","celltype","celltype_ID","marker","marker_type")])
      st_table_tissue_specific<-st_table_tissue_specific[!is.na(celltype)]
      
      
      mark_spec<-ddply(st_table_tissue_specific,.(species, DO_diseasetype,DO_ID,Uberon_tissue,Uberon_ID, marker,marker_type),nrow)
      colnames(mark_spec)<-c("species","DO_diseasetype","DO_ID","Uberon_tissue","Uberon_ID","marker","marker_type", "specificity_score")
      
      final_table<-merge(accordion_ec_table,mark_spec,by=c("species","DO_diseasetype","DO_ID","Uberon_tissue","Uberon_ID","marker","marker_type"),all.x = TRUE)
      final_table[,specificity_score:=format(round(1/specificity_score,2), nsmall=2)]
      
      final_table<-final_table[,c("species","original_diseasetype","DO_diseasetype","DO_ID","DO_definition", "original_tissue","Uberon_tissue","Uberon_ID","original_celltype","celltype","celltype_ID","cell_definition", "marker","gene_description","marker_type", "resource", "log2FC", "p.value", "adjusted_p.value","pct1", "EC_score","specificity_score")]
    } else{ #not consider tissue
      #EC_score
      st_table_tissue_specific<-unique(tableInputComplete()[,c("DO_diseasetype","DO_ID","species","celltype","celltype_ID","marker","marker_type","resource")])
      st_table_tissue_specific<-st_table_tissue_specific[!is.na(celltype)]
      
      ec_score_tissue_specific<-ddply(st_table_tissue_specific,.(species,DO_diseasetype,DO_ID,celltype,celltype_ID,marker,marker_type),nrow)
      
      colnames(ec_score_tissue_specific)<-c("species","DO_diseasetype","DO_ID","celltype","celltype_ID","marker", "marker_type","EC_score")
      accordion_ec_table<-merge(tableInputComplete(),ec_score_tissue_specific,by=c("species","DO_diseasetype","DO_ID","celltype","celltype_ID","marker", "marker_type"), all.x = TRUE)
      
      #specificity
      st_table_tissue_specific<-unique(accordion_ec_table[,c("DO_diseasetype","DO_ID","species","celltype","celltype_ID","marker","marker_type")])
      st_table_tissue_specific<-st_table_tissue_specific[!is.na(celltype)]
      mark_spec<-ddply(st_table_tissue_specific,.(species, DO_diseasetype,DO_ID, marker,marker_type),nrow)
      
      colnames(mark_spec)<-c("species","DO_diseasetype","DO_ID","marker","marker_type", "specificity_score")
      
      final_table<-merge(accordion_ec_table,mark_spec,by=c("species","DO_diseasetype","DO_ID","marker","marker_type"),all.x = TRUE)
      final_table[,specificity_score:=format(round(1/specificity_score,2), nsmall=2)]
      final_table<-final_table[,c("species","original_diseasetype","DO_diseasetype","DO_ID","DO_definition","original_celltype","celltype","celltype_ID","cell_definition", "marker","gene_description","marker_type", "resource", "log2FC", "p.value", "adjusted_p.value","pct1", "EC_score","specificity_score")]
    }
    final_table<-unique(final_table)
    final_table
    }
  })
  
  
  FinalTableM<-reactive({
    if(!is.null(markerTableOutputM())){
      
      if(nrow(markerTableOutputM())!=0 & input$tabletypeM=="Complete"){
        unique(markerTableOutputM()[EC_score >= str_replace_all(input$EC_scoreM, ">=","") & specificity_score >= str_replace_all(input$specificityM, ">=|=","")])
      } else if(nrow(markerTableOutputM())!=0 & input$tabletypeM=="Simple" & input$tissue_awareM == TRUE){
        unique(markerTableOutputM()[EC_score>= str_replace_all(input$EC_scoreM, ">=","") & specificity_score >= str_replace_all(input$specificityM, ">=|=","")
        ][,c("DO_diseasetype","Uberon_tissue","celltype","celltype_ID","marker","marker_type","species","EC_score","specificity_score")])
      } else if(nrow(markerTableOutputM())!=0 & input$tabletypeM=="Simple" & input$tissue_awareM == FALSE){
        unique(markerTableOutputM()[EC_score>= str_replace_all(input$EC_scoreM, ">=","") & specificity_score >= str_replace_all(input$specificityM, ">=|=","")
        ][,c("DO_diseasetype","celltype","celltype_ID","marker","marker_type","species","EC_score","specificity_score")])
        
      }
    }
    
    
  })  
  
  output$table1M <- renderDataTable({
    if(!is.null(FinalTableM())){
      FinalTableM() 
    }
  })
  
  # Downloadable csv of selected dataset 
  
  output$downloadDataM <- downloadHandler(
    filename = function() {
      paste0("accordion_complete", input$downloadTypeM)
    },
    content = function(file) {
      if(input$downloadTypeM == ".csv"){
        write.csv(FinalTableM(), file, row.names = FALSE, quote=FALSE)
      }
      else if(input$downloadTypeM == ".xlsx") {
        write_xlsx(FinalTableM(), file)
      }
      else if(input$downloadTypeM == ".tsv") {
        write.table(FinalTableM(), file, quote = FALSE, 
                    sep='\t', row.names = FALSE)
      }
      
    }
    
  ) 
  
  
  
  ###### server for annotation
  
  observeEvent(input$userclusterfileinfo,{
    showModal(modalDialog(
      title = "Marker genes input file",
      HTML("Load your marker genes file to annotate. <br>
      The file could be of two types: 
      <ul><li> <strong> The output file from the <em>FindAllMarkers </em> function from Seurat package </strong>, as txt, xlsx, csv or tsv file</li>
           <strong> Example </strong>"),
      HTML("<img src=findallmarker_ex.png>"),
      HTML("<br>"),
      HTML("<li> Or the annotation file could be provided as a <strong> custom table </strong> with at least one column contaning marker genes (one per row).
      All genes are considered as positive markers as default and related to a unique identity class. </li>
      \nYou can also provide addition columns:
             <ul><li> <strong> 1. </strong> cluster id </li> (optional)
             <li> <strong> 2. </strong> marker genes </li>
              <li> <strong> 3. </strong> marker type (optional) </li> <br> <ul><li> positive: whether the gene is a positive marker 
             </li><li> negative: whether the gene is a negative marker </li></ul>"), 
      HTML("<br>"),
      HTML("<strong> Example </strong>"),
      HTML("<img src=CustomTableAnno.jpg>"),
      easyClose = TRUE,
      footer = NULL
    ))
  })
  
  
  observeEvent(input$filterhelpA,{
    showModal(modalDialog(
      title = "Filters Information",
      HTML("Marker genes employed to annotate your data can be filtered by: <br>
      <ul><li> <strong> EC_score </strong>: evidence consistency score, measuring the agreement of different annotation sources </li>
       <li> <strong> database specificity </strong>: whether a gene is a marker for different cell types present in all the accordion database </li>")    ))
  })
  
  observeEvent(input$addpos, {
    req(input$nmarkerotherpos)
    otherVal <- "nmarkerotherpos"
    names(otherVal) <- input$nmarkerotherpos
    updatedValues <- c(nmarker_values, otherVal)
    updateRadioButtons(session, "nmarkerpos", choices = updatedValues)
  })
  
  observeEvent(input$addneg, {
    req(input$nmarkerotherneg)
    otherVal <- "nmarkerotherneg"
    names(otherVal) <- input$nmarkerotherneg
    updatedValues <- c(nmarker_values, otherVal)
    updateRadioButtons(session, "nmarkerneg", choices = updatedValues)
  })
  
  inputTable <-reactive ({
    if(is.null(input$clusterfile)) {
      # Read the default example file
      table<-fread("./data/FindAllMarkers_out.tsv", verbose = F)
      table<-table[,-1]
      if(!is.na(as.numeric(input$nmarkerpos))){
        positive_marker<-table %>%
          group_by(cluster) %>%
          slice_max(n = as.numeric(input$nmarkerpos), order_by = avg_log2FC)
        positive_marker<-as.data.table(positive_marker)
        positive_marker<-positive_marker[avg_log2FC > 0]
        
      } else { #ALL
        positive_marker<-as.data.table(table)
        positive_marker<-positive_marker[avg_log2FC > 0]
      }
      
      positive_marker[,gene %in% accordion_complete[species %in% input$speciesA]$marker]
      positive_marker[,pos_marker:= paste(gene, collapse=" "), by= cluster]
      positive_marker<-unique(positive_marker[,c("cluster","pos_marker")])
      
      if(!is.na(as.numeric(input$nmarkerneg))){ #select number of markers
        negative_marker<-table %>%
          group_by(cluster) %>%
          slice_max(n = as.numeric(input$nmarkerneg), order_by = -avg_log2FC)
        negative_marker<-as.data.table(negative_marker)
        negative_marker<-negative_marker[avg_log2FC < 0]
        
      } else { #ALL
        negative_marker<-as.data.table(table)
        negative_marker<-negative_marker[avg_log2FC < 0]
      }
      
      negative_marker[,gene %in% accordion_complete[species %in% input$speciesA]$marker]
      negative_marker[,neg_marker:= paste(gene, collapse=" "), by= cluster]
      negative_marker<-unique(negative_marker[,c("cluster","neg_marker")])
      
      input_marker<-merge(positive_marker,negative_marker, by="cluster",all.x=TRUE, all.y=TRUE)
      input_marker
      
    }else{
      if(!is.null(input$clusterfile)){
        file_load_cluster<-input$clusterfile
        fileName_cluster <- file_load_cluster$datapath
        ext <-  tools::file_ext(fileName_cluster)
        req(file_load_cluster)
        validate(need(ext %in% c("csv", "xlsx","txt","tsv"), "Please upload a csv, txt, tsv or xlsx file"))
        if(ext=="xlsx"){
          user_inputfile<-read_excel(fileName_cluster)
        }
        if(ext=="csv" | ext == "txt" | ext == "tsv"){
          user_inputfile<-fread(fileName_cluster)
          
        }
        user_inputfile<-as.data.table(user_inputfile)
        if(ncol(user_inputfile)>7){
          user_inputfile<-user_inputfile[,-1] #remove first column which is original rownames (gene)
        }
        col_names_findmarkers<-c("p_val","avg_log2FC","pct.1","pct.2","p_val_adj","cluster","gene") #colnames obtained from FindMarker
        
        if(identical(colnames(user_inputfile), col_names_findmarkers )){  #if the input is a FindMarker output
          if(!is.na(as.numeric(input$nmarkerpos))){
            positive_marker<-user_inputfile %>%
              group_by(cluster) %>%
              slice_max(n = as.numeric(input$nmarkerpos), order_by = avg_log2FC)
            positive_marker<-as.data.table(positive_marker)
            positive_marker<-positive_marker[avg_log2FC > 0]
            
          } else { #ALL
            positive_marker<-as.data.table(user_inputfile)
            positive_marker<-positive_marker[avg_log2FC > 0]
          }
          
          positive_marker[,gene %in% accordion_complete[species %in% input$speciesA]$marker]
          positive_marker[,pos_marker:= paste(gene, collapse=" "), by= cluster]
          positive_marker<-unique(positive_marker[,c("cluster","pos_marker")])
          
          if(!is.na(as.numeric(input$nmarkerneg))){ #select number of markers
            negative_marker<-user_inputfile %>%
              group_by(cluster) %>%
              slice_max(n = as.numeric(input$nmarkerneg), order_by = -avg_log2FC)
            negative_marker<-as.data.table(negative_marker)
            negative_marker<-negative_marker[avg_log2FC < 0]
            
          } else { #ALL
            negative_marker<-as.data.table(user_inputfile)
            negative_marker<-negative_marker[avg_log2FC < 0]
          }
          
          negative_marker[,gene %in% accordion_complete[species %in% input$speciesA]$marker]
          negative_marker[,neg_marker:= paste(gene, collapse=" "), by= cluster]
          negative_marker<-unique(negative_marker[,c("cluster","neg_marker")])
          
          input_marker<-merge(positive_marker,negative_marker, by="cluster",all.x=TRUE, all.y=TRUE)
          input_marker
        } else { #input different from findMarkers output 
          user_inputfile<-as.data.table(user_inputfile)
          validate(need(ncol(user_inputfile) >=1, "At least one column of marker genes is required"))
          if(ncol(user_inputfile) ==1){
            colnames(user_inputfile)<-"pos_marker"
            user_inputfile[,cluster:="0"]
            user_inputfile[,pos_marker:= paste(pos_marker, collapse=" "), by= cluster]
            
            
          } else if(ncol(user_inputfile) == 2){
            colnames(user_inputfile)<-c("cluster","pos_marker")
            user_inputfile[,pos_marker:= paste(pos_marker, collapse=" "), by= cluster]
            
          } else if(ncol(user_inputfile) == 3){
            #test<-read_excel("costumex.xlsx")
            colnames(user_inputfile)<-c("cluster","marker","marker_type")
            pos_marker<-user_inputfile[marker_type=="positive"][,pos_marker:= paste(marker, collapse=" "), by= cluster]
            pos_marker<-unique(pos_marker[,c("cluster","pos_marker")])
            
            neg_marker<-user_inputfile[marker_type=="negative"][,neg_marker:= paste(marker, collapse=" "), by= cluster]
            neg_marker<-unique(neg_marker[,c("cluster","neg_marker")])
            user_inputfile<-merge(pos_marker,neg_marker, by="cluster",all.x=TRUE, all.y=TRUE)
            user_inputfile
            
          }
          return(user_inputfile)
          
        }
        
      }  
    }
  })  
  
  
  output$table_example <- renderDataTable({
    if(is.null(input$clusterfile)){
      table<-fread("./data/FindAllMarkers_out.tsv", verbose = F)
      table<-table[,-1]
      }
      
  })
  
  output$example_panel <- renderUI({
    if (is.null(input$clusterfile)) {
      column(12,
        HTML("<h5 style='color: black;'>An example file (output from FindAllMarkers function) has been automatically loaded.<br>You can proceed with the analysis or upload your own file.</h5>"),
        dataTableOutput('table_example')
      )
    } else {
      return(NULL)  # Ensures UI is removed when file is uploaded
    }
  })
  
  
  
  
  
  markerTable<-reactive ({
    table<-accordion_complete[DO_diseasetype %in% input$diseaseA & species %in% input$speciesA]
    table
  })
  
  
  toListen_tissueA <- reactive({
    list(input$speciesA, input$diseaseA)
  })
  
  observeEvent(toListen_tissueA(),{
    updatePickerInput(session,'tissueA', selected = "blood",
                      choices=unique(markerTable()$Uberon_tissue),
                      option=list(`actions-box` = TRUE,style="box-celltypes"),
                      choicesOpt = list(style = rep(("font-size: 18px; line-height: 1.6;"), uniqueN(markerTable()$Uberon_tissue))))
  })
  
  
  markerTableTissue <- reactive ({
    #keep tissue separated 
    if(input$tissue_awareA == TRUE){
      #compute EC and specificity condition&tissue specific
      
      #EC score
      st_table_tissue_specific<-unique(markerTable()[,c("DO_diseasetype","DO_ID","Uberon_tissue","Uberon_ID","species","celltype","celltype_ID","marker","marker_type","resource")])
      st_table_tissue_specific<-st_table_tissue_specific[!is.na(celltype)]
      ec_score_tissue_specific<-ddply(st_table_tissue_specific,.(species,DO_diseasetype,DO_ID,Uberon_tissue,Uberon_ID,celltype,celltype_ID,marker,marker_type),nrow)
      colnames(ec_score_tissue_specific)<-c("species","DO_diseasetype","DO_ID","Uberon_tissue","Uberon_ID","celltype","celltype_ID","marker", "marker_type","EC_score")
      accordion_ec_table<-merge(markerTable(),ec_score_tissue_specific,by=c("species","DO_diseasetype","DO_ID","Uberon_tissue","Uberon_ID","celltype","celltype_ID","marker", "marker_type"), all.x = TRUE)
      
      #specificity
      st_table_tissue_specific<-unique(accordion_ec_table[,c("DO_diseasetype","DO_ID","Uberon_tissue","Uberon_ID","species","celltype","celltype_ID","marker","marker_type","marker","marker_type")])
      st_table_tissue_specific<-st_table_tissue_specific[!is.na(celltype)]
      
      mark_spec<-ddply(st_table_tissue_specific,.(species, DO_diseasetype,DO_ID,Uberon_tissue,Uberon_ID, marker,marker_type),nrow)
      colnames(mark_spec)<-c("species","DO_diseasetype","DO_ID","Uberon_tissue","Uberon_ID","marker","marker_type", "specificity_score")
      
      final_table<-merge(accordion_ec_table,mark_spec,by=c("species","DO_diseasetype","DO_ID","Uberon_tissue","Uberon_ID","marker","marker_type"),all.x = TRUE)
      final_table[,specificity_score:=format(round(1/specificity_score,2), nsmall=2)]
      
      final_table<-final_table[,c("species","original_diseasetype","DO_diseasetype","DO_ID","DO_definition", "original_tissue","Uberon_tissue","Uberon_ID","original_celltype","celltype","celltype_ID","cell_definition", "marker","gene_description","marker_type", "resource", "log2FC", "p.value", "adjusted_p.value","pct1", "EC_score","specificity_score")]
    } else{ #not consider tissue
      #EC_score
      st_table_tissue_specific<-unique(markerTable()[,c("DO_diseasetype","DO_ID","species","celltype","celltype_ID","marker","marker_type","resource")])
      st_table_tissue_specific<-st_table_tissue_specific[!is.na(celltype)]
      
      ec_score_tissue_specific<-ddply(st_table_tissue_specific,.(species,DO_diseasetype,DO_ID,celltype,celltype_ID,marker,marker_type),nrow)
      
      colnames(ec_score_tissue_specific)<-c("species","DO_diseasetype","DO_ID","celltype","celltype_ID","marker", "marker_type","EC_score")
      accordion_ec_table<-merge(markerTable(),ec_score_tissue_specific,by=c("species","DO_diseasetype","DO_ID","celltype","celltype_ID","marker", "marker_type"), all.x = TRUE)
      
      #specificity
      st_table_tissue_specific<-unique(accordion_ec_table[,c("DO_diseasetype","DO_ID","species","celltype","celltype_ID","marker","marker_type","marker","marker_type")])
      st_table_tissue_specific<-st_table_tissue_specific[!is.na(celltype)]
      mark_spec<-ddply(st_table_tissue_specific,.(species, DO_diseasetype,DO_ID, marker,marker_type),nrow)
      
      colnames(mark_spec)<-c("species","DO_diseasetype","DO_ID","marker","marker_type", "specificity_score")
      
      final_table<-merge(accordion_ec_table,mark_spec,by=c("species","DO_diseasetype","DO_ID","marker","marker_type"),all.x = TRUE)
      final_table[,specificity_score:=format(round(1/specificity_score,2), nsmall=2)]
      final_table<-final_table[,c("species","original_diseasetype","DO_diseasetype","DO_ID","DO_definition","original_celltype","celltype","celltype_ID","cell_definition", "marker","gene_description","marker_type", "resource", "log2FC", "p.value", "adjusted_p.value","pct1", "EC_score","specificity_score")]
    }
  })
  
  
  accMarkerAnnoTable <- reactive ({
    
    accordion_complete_filt<-markerTableTissue()[ EC_score >= str_replace_all(input$EC_scoreA, ">=","") & specificity_score >= str_replace_all(input$specificityA, ">=|=","")]
    positive_marker_acc<-accordion_complete_filt[marker_type=="positive" & species %in% input$speciesA][,pos_marker_acc:= paste(marker, collapse=" "), by= celltype]
    positive_marker_acc<-unique(positive_marker_acc[,c("celltype","pos_marker_acc")])
    negative_marker_acc<-accordion_complete_filt[marker_type=="negative" & species %in% input$speciesA][,neg_marker_acc:= paste(marker, collapse=" "), by= celltype]
    negative_marker_acc<-unique(negative_marker_acc[,c("celltype","neg_marker_acc")])
    
    accordion_marker<-merge(positive_marker_acc,negative_marker_acc, by="celltype",all.x=TRUE, all.y=TRUE)
    accordion_marker
  })
  
  
  accMarkerAnnoTableLong <- reactive ({
    do.call("rbind", replicate(nrow(inputTable()), accMarkerAnnoTable(), simplify = FALSE))

  })
  
  inputTableLong <- reactive ({
    input_marker_long<-do.call("rbind", replicate(nrow(accMarkerAnnoTable()), inputTable(), simplify = FALSE))
    input_marker_long<-input_marker_long[order(cluster)]
    input_marker_long

  })
  
  combineTable <- reactive({ #create table to perform Fisher Test
    dt_combine<-cbind(inputTableLong(),accMarkerAnnoTableLong())
    dt_combine<-as.data.table(dt_combine)
    #IY overlap between input and anno
    dt_combine[,positive_intersect:= mapply(function(x, y) paste0(str_sort(intersect(x, y)), collapse = " "), strsplit(dt_combine$pos_marker, ' '), strsplit(dt_combine$pos_marker_acc, ' '))]
    dt_combine[,negative_intersect:= mapply(function(x, y) paste0(str_sort(intersect(x, y)), collapse = " "), strsplit(dt_combine$neg_marker, ' '), strsplit(dt_combine$neg_marker_acc, ' '))]
    
    dt_combine[,tot_overlap:= paste0(positive_intersect, " ", negative_intersect)]
    dt_combine[,tot_overlap := str_trim(tot_overlap)]
    
    #IN  in input, not in anno
    dt_combine[,in_input_not_anno_pos := mapply(function(x, y) paste0(setdiff(x, y), collapse = " "), strsplit(dt_combine$pos_marker, ' '), strsplit(dt_combine$pos_marker_acc, ' '))]
    dt_combine[,in_input_not_anno_neg:= mapply(function(x, y) paste0(setdiff(x, y), collapse = " "), strsplit(dt_combine$neg_marker, ' '), strsplit(dt_combine$neg_marker_acc, ' '))]
    dt_combine[,in_input_not_anno_tot:= paste0(in_input_not_anno_pos, " ", in_input_not_anno_neg)]
    dt_combine[,in_input_not_anno_tot := str_trim(in_input_not_anno_tot)]
    
    # BY not in input, in anno
    dt_combine[,not_input_in_anno_pos := mapply(function(x, y) paste0(setdiff(x, y), collapse = " "), strsplit(dt_combine$pos_marker_acc, ' '), strsplit(dt_combine$pos_marker, ' '))]
    dt_combine[,not_input_in_anno_neg:= mapply(function(x, y) paste0(setdiff(x, y), collapse = " "), strsplit(dt_combine$neg_marker_acc, ' '), strsplit(dt_combine$neg_marker, ' '))]
    dt_combine[,not_input_in_anno_tot:= paste0(not_input_in_anno_pos, " ", not_input_in_anno_neg)]
    dt_combine[,not_input_in_anno_tot := str_trim(not_input_in_anno_tot)]
    dt_combine
    
  })
  
  observeEvent(input$button, {
    annoResultsTable <- reactive({
      out_df<-tibble()
      inputseppos<-separate_rows(as.data.frame(inputTable()), pos_marker, sep=" ")
      inputsepneg<-separate_rows(as.data.frame(inputTable()), neg_marker, sep=" ")
      univ<- uniqueN(unique(rbind(accordion_complete$marker, inputseppos$pos_marker, inputsepneg$neg_marker)))
      withProgress(message = 'Making cluster identification', value = 0, {
        for (cl in 1:nrow(inputTable())-1){
          sub_dt<-combineTable()[cluster==cl]
          # Increment the progress bar, and update the detail text.
          incProgress(1/nrow(inputTable())-1, detail = paste("Doing cluster", cl))
          # Pause for 0.1 seconds to simulate a long computation.
          Sys.sleep(0.1)
          for (ct in (accMarkerAnnoTable()$celltype)){
            #print(ct)
            sub_ct_dt<-sub_dt[celltype==ct]
            IY<- count.fields(textConnection(sub_ct_dt$tot_overlap), sep = " ") # in input, in anno (overlap)
            IN<- count.fields(textConnection(sub_ct_dt$in_input_not_anno_tot), sep = " ") # in input, not in anno
            BY<- count.fields(textConnection(sub_ct_dt$not_input_in_anno_tot), sep = " ") # not in input, in anno
            BN<- univ - (IY+IN+BY) # not in input, not in anno 
            fisher_data <- fisher.test(matrix(c(IY, BY, IN, BN), 2, 2), alternative="greater") 
            # generation of the output (a tibble with 1 row)
            out_df <- rbind(out_df, tibble("cluster"=cl, # name of the input
                                           "celltype"=ct, # name of the annotation
                                           #"anno_class"=anno_class,
                                           "overlap_size"=IY, # size of the overlap 
                                           "p_value"=fisher_data$p.value, # p-value of the enrichment (Fisher test)
                                           "odds_ratio"=fisher_data$estimate, # conditional MLE estimate from fisher.test function
                                           "combined_score"=0, # default combined score (to match enrichr enrichment results)
                                           #"odds_ratio_sample"= (IY/IN)/(BY/BN), # sample odds ratio
                                           #"fold_enrichment"= (IY/(IY+IN))/((IY+BY)/(BY+IY+BY+BN)), # fold enrichment formula
                                           "input_size"=IY+IN, # size of processed input 
                                           "anno_size"=IY+BY, # size of processed annotation
                                           "background_size"= IY+IN+BY+BN, # size of processed background
                                           "overlap_input_ratio"= IY/(IY+IN), # overlap ratio with respect to input size
                                           "overlap_anno_ratio"= IY/(IY+BY), # overlap ratio with respect to annotation size
                                           "positive_overlap_ids"= sub_ct_dt$positive_intersect, # string with positive overlap markers ids
                                           "negative_overlap_ids" = sub_ct_dt$negative_intersect, # string with negative overlap markers ids
                                           #"overlap_ids"=  sub_ct_dt$tot_overlap # string with overlap ids
            ))
          }
        }
        out_dt<-as.data.table(out_df)
        out_dt<-out_dt[order(cluster,p_value)]
        out_dt_max<-out_dt[!duplicated(out_dt$cluster)]
        out_dt_max
      })
    })
    
    output$table1A <- renderDataTable({
      annoResultsTable()
    })
    
    output$downloadDataA <- downloadHandler(
      filename = function() {
        paste0("Annotation_table", input$downloadTypeA)
      },
      content = function(file) {
        if(input$downloadTypeA == ".csv"){
          write.csv(annoResultsTable(), file, row.names = FALSE, quote=FALSE)
        }
        else if(input$downloadTypeA == ".xlsx") {
          write_xlsx(annoResultsTable(), file)
        }
        else if(input$downloadTypeA == ".tsv") {
          write.table(annoResultsTable(), file, quote = FALSE, 
                      sep='\t', row.names = FALSE)
        }
        
      }
      
      
    )
    
    
    tablePlotOntology <- reactive ({
      dt_plot<-merge(annoResultsTable(),ontology_celltype, by="celltype")
      dt_plot<-dt_plot[,c("celltype","celltype_ID")]
      dt_plot
    })
    
    PlotA <- reactive({
      if (nrow(tableInputComplete())>=1) {
        cell_matching<-unique(tablePlotOntology()$celltype_ID)
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
          label<-merge(dt_onto2,ontology_celltype,by.x="V1",by.y="celltype_ID")
          ontosubplot[["node_attributes"]][["label"]][[label$V1]]<-label$celltype        
          ontosubplot2@nodes<-ontosubplot[["node_attributes"]][["label"]]
          ontosubplot2
          
        }
        
      }
      
    })
    
    
    output$plot1A <- renderGrViz({ 
      hierac_plot2(accordion_complete, PlotA(), tablePlotOntology(),input$cellidA) 
      
    })
    
    output$scalableplotA <- renderUI({ 
      tagList(
        div(grVizOutput('plot1A',height = input$heightA, width = input$widthA)))
    })
    
    
    
    click_plotA<-reactive ({
      click_node(accordion_complete, ontology_celltype, PlotA(), tablePlotOntology(),input$cellidA, ontology_def, input$diseaseA)
    })
    
    
    txtA <- reactive({
      req(input$plot1A_click)
      nodeval <- input$plot1A_click$nodeValues[1:length(input$plot1A_click$nodeValues)]
      nodeval<-as.data.table(paste(nodeval,sep="", collapse=" "))
      return(  click_plotA()[label %in% nodeval$V1]$cell_definition)
      
    })
    output$celltype_defA <- renderText({
      req(txtA())
      txtA()
    })
    
  })
  
  
  
}



shinyApp(ui = ui, server = server, options=list(port=8200))


