
list.of.packages <- c("shinydashboard","shiny","stringr","DiagrammeR","igraph",
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
library(data.table)
library(ontologyPlot)
library(ontoProc)
library(writexl)
library(dplyr)
library(shinyBS)
library(shinyhelper)
library(shinydashboardPlus)
library(readxl)
library(rstatix)
library(tidyverse)

source('helper_function2023.R')

marker_table<-fread("data/Hema_Accordion_2023.txt",sep='\t',header=TRUE)


marker_table_long<- melt(marker_table, id.vars = c("cell_type","celltype_species","marker",                        
                                                   "marker_type","EC_score","cell_ID",                       
                                                   "cell_def","species","gene_description","database_specificity"))
marker_table_long<-marker_table_long[!is.na(value)]

marker_table_collapse<-marker_table_long[,reference:=tstrsplit(variable,".", fixed = TRUE, keep = 2)]
marker_table_collapse<-as.data.table(marker_table_collapse[,-c("variable","value")] %>% aggregate(reference ~ ., FUN = paste, collapse="; "))

marker_table_long<-marker_table_long[,-c("EC_score","database_specificity","reference")]
colnames(marker_table_long)<-c("cell_type","celltype_species","marker","marker_type",           
                               "cell_ID", "cell_def", "species","gene_description","database","orig.name")



marker_table_collapse[,disease_type:="healthy"]
marker_table_collapse[,disease_id:=NA]

#load disease accordion marker
marker_disease<-fread("data/dt_accordion_disease_collapse_Feb23.tsv",header = TRUE)
colnames(marker_disease)[13] <- "reference"

marker_table_complete<-rbind(marker_table_collapse[,c("disease_type","disease_id","cell_type","celltype_species","cell_ID","cell_def","marker",                        
                                             "marker_type","database_specificity","EC_score",                       
                                             "species","gene_description","reference")],marker_disease[,c("disease_type","disease_id","cell_type","celltype_species","cell_ID","cell_def","marker",                        
                                                                                              "marker_type","EC_score", "database_specificity",                      
                                                                                              "species","gene_description","reference")])

colnames(marker_table_complete)[1] <- "condition"

#compute specificity 
mark_spec<-ddply(marker_table_complete,.(marker),nrow)
colnames(mark_spec)<-c("marker","disease_specificity")
marker_table_complete<-merge(marker_table_complete,mark_spec,by="marker",all.x = TRUE)
marker_table_complete[,disease_specificity:=format(round(1/disease_specificity,2), nsmall=2)]

marker_table_complete<-marker_table_complete[,c("condition","disease_id","cell_type","celltype_species","cell_ID","cell_def","marker",                        
  "marker_type","EC_score", "database_specificity",                      
  "species","gene_description","disease_specificity","reference")]

#load ontology 
cell_onto<-get_ontology("data/cl-basic.obo", propagate_relationships = c("is_a","develops_from"), extract_tags = "everything")

parents<-c(cell_onto[["parents"]],list(`DOID:9119-CL:0000037`="CL:0000037",
                                       `DOID:9538-CL:0000786`="CL:0000786",
                                       `DOID:8552-CL:0000037`= "CL:0000037",
                                       `DOID:1240-CL:0000037`="CL:0000037",
                                       `DOID:1040-CL:0000236`= "CL:0000236",
                                       `DOID:9952-CL:0000037`="CL:0000037",
                                       `DOID:0080638-CL:0000037`="CL:0000037"))

name_id<-c(cell_onto[["name"]], list(`DOID:9119-CL:0000037`="leukemia stem cell",`DOID:9538-CL:0000786`="myeloma plasma cell",
                                     `DOID:8552-CL:0000037`="leukemia stem cell", `DOID:1240-CL:0000037` = "leukemia stem cell",
                                     `DOID:1040-CL:0000236`= "CD5+ B cell",`DOID:9952-CL:0000037`="leukemia stem cell",
                                     `DOID:0080638-CL:0000037`="leukemia stem cell"))

def<-c(cell_onto[["def"]], list(`DOID:9119-CL:0000037`= unique(marker_disease[cell_type=="leukemia stem cell"]$cell_def),
                                `DOID:9538-CL:0000786`= unique(marker_disease[cell_type=="myeloma plasma cell"]$cell_def),
                                 `DOID:8552-CL:0000037`= unique(marker_disease[cell_type=="leukemia stem cell"]$cell_def),
                                  `DOID:1240-CL:0000037`=unique(marker_disease[cell_type=="leukemia stem cell"]$cell_def),
                                `DOID:1040-CL:0000236`=unique(marker_disease[cell_type=="CD5+ B cell"]$cell_def),
                                `DOID:9952-CL:0000037`=unique(marker_disease[cell_type=="leukemia stem cell"]$cell_def),
                                `DOID:0080638-CL:0000037`=unique(marker_disease[cell_type=="leukemia stem cell"]$cell_def)))
cell_onto<-ontology_index(
  parents = parents,
  id = names(parents),
  name = name_id,
  obsolete = setNames(nm = name_id, rep(FALSE, length(name_id))),
  version = NULL)

cell_onto$def <- def
onto_plot<-onto_plot2(cell_onto, marker_table_complete$cell_ID)
nodes<-as.data.table(onto_plot@nodes)
nodes<-nodes[,V1:=tstrsplit(nodes$V1,"CL", fixed = TRUE, keep = 1)]
nodes<-nodes[,V1:=tstrsplit(nodes$V1,"DOID", fixed = TRUE, keep = 1)]
nodes<-nodes[, V1:=str_replace_all(V1,"\n"," ")]
nodes<-nodes[, V1:=str_sub(V1,1,nchar(V1)-1)]
onto_plot@nodes<-nodes$V1

#ontology cell types
ontology_celltype<-as.data.frame(unlist(cell_onto[["name"]]))
colnames(ontology_celltype)<-"cell_type"
ontology_celltype$cell_ID<-rownames(ontology_celltype)
ontology_celltype<-as.data.table(ontology_celltype)

#definition of cell types
ontology_def<-as.data.frame(unlist(cell_onto[["def"]]))
colnames(ontology_def)<-"cell_def"
ontology_def$cell_ID<-names(cell_onto[["def"]])
ontology_def<-as.data.table(ontology_def)
ontology_def<-merge(ontology_def,ontology_celltype,by="cell_ID")
ontology_def[,cell_def:=str_remove_all(cell_def, '"')]
ontology_def[,cell_def:=str_remove_all(cell_def, "\\$")]
ontology_def[,cell_def:=tstrsplit(cell_def, "[", fixed = TRUE, keep = 1)]

#gene description
#humanz
description_hs<-read_excel("data/ensembl_gene_description/Ensembl_Hs_38.p13.xlsx",col_names = T)
colnames(description_hs)<-c("gene_name","gene_description","gene_synonym")
description_hs<-as.data.table(description_hs)
description_hs[,gene_description:=tstrsplit(gene_description,"[",fixed=TRUE,keep=1)]
description_hs<-description_hs[!gene_name==""]
description_hs[,gene_name:= str_trim(gene_name)]
description_hs[,gene_synonym:= str_trim(gene_synonym)]
#mouse
description_mm<-read_excel("data/ensembl_gene_description/Ensembl_Mm_m39.xlsx",col_names=T)
colnames(description_mm)<-c("gene_name","gene_description","gene_synonym")
description_mm<-as.data.table(description_mm)
description_mm[,gene_description:=tstrsplit(gene_description,"[",fixed=TRUE,keep=1)]
description_mm<-description_mm[!gene_name==""]
description_mm[,gene_name:= str_trim(gene_name)]
description_mm[,gene_synonym:= str_trim(gene_synonym)]

nmarker_values<-c("ALL")
disease_list<-unique(marker_table_complete$condition)
names(disease_list)<-unique(marker_table_complete$condition)

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
                p(style="text-align: justify;", HTML("<h>A crucial and challenging step in single-cell and spatial data analysis is the annotation of cell types. The Cell Marker Accordion adresses the need for robust and reproducible cell type identification through standardization and integration of multiple published gene marker databases. <br> The Cell Marker Accordion web interface allows to easily: </h> "))),
              
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
                                                                         list("Human","Mouse"),selected = c("Human"),inline=TRUE),
                                                    br(),
                                                    #pickerInput('disease', 'Hematologic disorder', choices= NULL ,multiple=TRUE, options = list(`actions-box` = TRUE,`live-search`=TRUE)),
                                                    selectizeInput('disease', 'Condition',  choices= disease_list , selected = "healthy", multiple=TRUE, options = list(`actions-box` = TRUE,`live-search`=TRUE)),
                                                    splitLayout(cellWidths = c("75%", "25%"),fileInput("usermarker", "Load your custom annotation",buttonLabel=list(icon("upload")),multiple = FALSE),
                                                                actionButton('usermarkerinfo', 'InputFile',icon= icon("file-circle-question"), align="left",style='margin-top:30px')),
                                                    pickerInput('celltype', 'Cell type', choices= NULL ,multiple=TRUE, options = list(`actions-box` = TRUE,`live-search`=TRUE)),
                                                    br(),
                                                    pickerInput('descendantsof', 'See subtypes of:', choices= NULL,multiple=TRUE, options = list(`actions-box` = TRUE,`live-search`=TRUE, style="box-celltypes"),choicesOpt = list(style = rep(("font-size: 18px; line-height: 1.6;"), 141))),
                                                    checkboxInput("cellid","Plot cell_ID",value=FALSE))),
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
                                                     fluidRow(column(2,radioButtons('EC_score','EC_score', c(">=1",">=2",">=3",">=4"),selected = ">=1")),
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
                  # bsTooltip("EC_score", "Evidence consensus score (number of databases)", placement = "top", trigger = "hover",
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
                  tags$p(tags$strong("ThermoFisher"), tags$a("ThermoFisher",href = "http://assets.thermofisher.com/TFS-Assets/LSG/brochures/immune-cell-guide.pdf")),
                  tags$p(tags$strong("Abcam"), tags$a("Abcam", href="https://www.abcam.com/primary-antibodies/human-cd-antigen-guide")))),
      
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
                                                    selectizeInput('diseaseM', 'Condition', selected = "healthy", choices= disease_list, multiple=TRUE, options = list(`actions-box` = TRUE,`live-search`=TRUE)),
                                                    checkboxInput("cellidM","Plot cell_ID",value=FALSE))),
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
                  fluidRow(column(width=12,wellPanel(id="sidebar2M",
                                                     fluidRow(column(3,radioButtons('EC_scoreM','EC_score', c(">=1",">=2",">=3",">=4"),selected = ">=1")),
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
                  # bsTooltip("EC_score", "Evidence consensus score (number of databases)", placement = "top", trigger = "hover",
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
                  tags$p(tags$strong("ThermoFisher"), tags$a("ThermoFisher",href = "http://assets.thermofisher.com/TFS-Assets/LSG/brochures/immune-cell-guide.pdf")),
                  tags$p(tags$strong("Abcam"), tags$a("Abcam",href="https://www.abcam.com/primary-antibodies/human-cd-antigen-guide")))),
      
      
##### SEMI-AUTOMATIC ANNOTATION -----
      tabItem(tabName="anno", 
              div(fluidRow(column(width=6,wellPanel(id="sidebar",checkboxGroupInput("speciesA", "Select species:",
                                                                                    choiceNames =
                                                                                      list(tags$img(src = "human.png"),tags$img(src = "mouse.png")),
                                                                                    choiceValues =
                                                                                      list("Human","Mouse"),selected = c("Human"),inline=TRUE),
                                                    #textInput("marker", "Insert marker genes", value = "CD34", width = NULL, placeholder = NULL),
                                                    splitLayout(cellWidths = c("75%", "25%"),fileInput("clusterfile", "Load file to annotate",buttonLabel=list(icon("upload")),multiple = FALSE),
                                                                actionButton('userclusterfileinfo', 'InputFile',icon= icon("file-circle-question"), align="left", style='margin-top:30px; margin-bottom:0px')),
                                                    fluidRow(column(width=6,radioButtons("nmarkerpos", HTML("How many <em>positive</em> markers you want for each cluster?"), choices = nmarker_values)),
                                                             column(width=6, radioButtons("nmarkerneg", HTML("How many <em>negative</em> markers you want for each cluster?"), choices = nmarker_values))),
                                                    fluidRow(column(width=6,textInput("nmarkerotherpos", HTML("Type in number of <em>positive</em> markers"))),
                                                             column(width=6,textInput("nmarkerotherneg", HTML("Type in number of <em>negative</em> markers")))),
                                                    fluidRow(column(width=6,actionButton("addpos", HTML("Add value"))),
                                                             column(width=6,actionButton("addneg", HTML("Add value")))),
                                                    
                                                    style = "padding-bottom: 5px;")),
                           column(width=6, wellPanel(id="sidebar",fluidRow(shiny::span(p(HTML("<h7>Filters</h7>"),actionButton('filterhelpA', 'Info',icon= icon("info"), align="center"))),
                                                                           column(width=6,radioButtons('EC_scoreA','EC_score', c(">=1",">=2",">=3",">=4"),selected = ">=1")),
                                                                           column(width=6, radioButtons('database_specA','database_specificity', c(">=0",">=0.25",">=0.5","=1"),selected = ">=0"))),
                                                     
                                                     style = "padding-bottom: 13px;")),
                           br(),
                           column(6,offset=0,div(img(src = "Logo.png",height="30%", width="30%"),style="text-align: center;"), 
                                  br(),
                                  actionButton("button", "Click to annotate!",icon= icon("rocket")), align="center")),
                  
                  br(),
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
                    checkboxInput("cellidA","Plot cell_ID",value=FALSE),
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
                  conditionalPanel(id="testA",condition= "input.plot1A_click",wellPanel(textOutput("cell_type_defA"))),
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
                  tags$p(tags$strong("ThermoFisher"), tags$a("ThermoFisher",href = "http://assets.thermofisher.com/TFS-Assets/LSG/brochures/immune-cell-guide.pdf")),
                  tags$p(tags$strong("Abcam"), tags$a("Abcam",href="https://www.abcam.com/primary-antibodies/human-cd-antigen-guide"))))
    )
  )
  
)
server <- function(input, output, session) {
  
  ############ HEMATOPOIETIC SYSTEM 
  
  ###### server for cell type search 
  
  observeEvent(input$usermarkerinfo,{
    showModal(modalDialog(
      title = "Marker genes input file",
      HTML("Add your list of marker genes to be integrated with the Accordion. <br>
      The file must contain at least two columns:
             <ul><li> <strong> 1. </strong>: cell types based on the Cell Ontology nomenclature </li>
             <li> <strong> 2. </strong>: marker genes </li>
             You can also provide additional columns:
             <li> <strong> 3. </strong>: marker type (default positive) <br> <ul><li> positive: whether the gene is a positive marker 
             </li><li> negative: whether the gene is a negative marker </li></ul>
             <li> <strong> 4. </strong>: species (default Human. You can also select the species using the above box) <br> <ul><li> Human </li><li> Mouse </li></ul>
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
        colnames(user_inputfile)<-c("cell_type","marker")
        user_inputfile[,marker_type:="positive"]
        user_inputfile[,species:= input$species]
        
      } else if (ncol(user_inputfile)==3){
        if(toupper("positive") %in% toupper(as.character(as.data.frame(user_inputfile)[,3])) | toupper("negative") %in% toupper(as.character(as.data.frame(user_inputfile)[,3]))){
          colnames(user_inputfile)<-c("cell_type","marker","marker_type")
          user_inputfile[,species:= input$species]
          user_inputfile[,marker_type:=tolower(marker_type)]
          
        } else if(toupper("Human") %in% toupper(as.character(as.data.frame(user_inputfile)[,3])) | toupper("Mouse") %in% toupper(as.character(as.data.frame(user_inputfile)[,3]))){
          colnames(user_inputfile)<-c("cell_type","marker","species")
          user_inputfile[,marker_type:="positive"]
          user_inputfile[,species:=tolower(species)]
          user_inputfile[,species:=str_to_title(species)]
          user_inputfile<-user_inputfile[,c("cell_type","marker","marker_type","species")]
          
        }
      } else if (ncol(user_inputfile)==4){
        colnames(user_inputfile)<-c("cell_type","marker","marker_type","species")
      }
      
      #merge user marker with ontology
      user_inputfile<-merge(user_inputfile, ontology_celltype,by="cell_type")
      user_inputfile<-merge(user_inputfile,ontology_def[,-"cell_type"],by="cell_ID")
      
      dt_hs<-merge(user_inputfile[species=="Human"],description_hs[,c("gene_name","gene_description")],by.x="marker",by.y="gene_name",all.x=TRUE)
      dt_hs_na<-dt_hs[is.na(gene_description)]
      dt_hs_na<-merge(dt_hs_na[,-("gene_description")],description_hs[,c("gene_synonym","gene_description")],by.x="marker",by.y="gene_synonym",all.x=TRUE)
      dt_hs<-rbind(dt_hs[!(is.na(gene_description))],dt_hs_na)
      
      dt_mm<-merge(user_inputfile[species=="Mouse"],description_mm[,c("gene_name","gene_description")],by.x="marker",by.y="gene_name",all.x=TRUE)
      dt_mm_na<-dt_mm[is.na(gene_description)]
      dt_mm_na<-merge(dt_mm_na[,-("gene_description")],description_mm[,c("gene_synonym","gene_description")],by.x="marker",by.y="gene_synonym",all.x=TRUE)
      dt_mm<-rbind(dt_mm[!(is.na(gene_description))],dt_mm_na)
      
      user_inputfile<-rbind(dt_hs,dt_mm)
      user_inputfile<-unique(user_inputfile)
      
      #add column celltype_species (for app)
      celltype_hs<-unique(user_inputfile[species=="Human",c("cell_type","species")])
      celltype_mm<-unique(user_inputfile[species=="Mouse",c("cell_type","species")])
      celltype<-merge(celltype_hs,celltype_mm,by="cell_type",all=TRUE)
      celltype[,V1:= paste0(species.x,", ",species.y)]
      celltype[,celltype_species:=str_replace(V1, "Human, Mouse","Hs, Mm")]
      celltype[,celltype_species:=str_replace(celltype_species, "Human, NA","Hs")]
      celltype[,celltype_species:=str_replace(celltype_species, "NA, Mouse","Mm")]
      celltype_species<-paste0(celltype$cell_type," (",celltype$celltype_species,")")
      celltype_species<-cbind(celltype$cell_type,celltype_species)
      colnames(celltype_species)<-c("cell_type","celltype_species")
      user_inputfile<-merge(celltype_species,user_inputfile,by="cell_type")
      user_inputfile<-as.data.table(user_inputfile)
      user_inputfile<-user_inputfile[,c("cell_type","celltype_species","marker","marker_type","cell_ID","cell_def","species","gene_description")]
      user_inputfile[,database:="original_celltype.custom"]
      user_inputfile[,orig.name:=cell_type]
      
      #union of accordion markers and user marker
      combine_table_long<-rbind(user_inputfile,marker_table_complete_long)
      combine_table_long<-unique(combine_table_long)
      
      #compute specificity 
      mark_spec<-ddply(combine_table_long,.(marker),nrow)
      colnames(mark_spec)<-c("marker","database_specificity")
      combine_table_long<-merge(combine_table_long,mark_spec,by="marker",all.x = TRUE)
      combine_table_long[,database_specificity:=format(round(1/database_specificity,2), nsmall=2)]
      
      
      count_duplicated<-ddply(combine_table_long,.(cell_type,  marker, species),nrow)
      colnames(count_duplicated)<-c("cell_type","marker","species","EC_score")
      combine_table_long<-merge(combine_table_long,count_duplicated,by=c("cell_type","marker","species"))
      
      combine_table<-dcast(combine_table_long, cell_type + celltype_species + marker +                        
                             marker_type +EC_score+cell_ID+cell_def+species+gene_description+database_specificity ~ database, value.var = "orig.name")
      
      combine_table
    }
    else {
      marker_table_complete
    }
  }) 
  
  markerTableComplete <- reactive({
    validate(need(length(input$disease) >=1, "Please select at least one condition"))
    marker_table_complete<-marker_table_complete[condition %in% input$disease]
    marker_table_complete

})
  
  toListen <- reactive({
    list(input$species,input$usermarker, input$disease)
  })
  
  observeEvent(toListen(),{
    updatePickerInput(session,'celltype', selected=c("hematopoietic stem cell (Hs, Mm)", "hematopoietic multipotent progenitor cell (Hs, Mm)","hematopoietic lineage restricted progenitor cell (Hs, Mm)"),
                      choices=unique(markerTableComplete()[condition %in% input$disease & species %in% input$species]$celltype_species),
                      option=list(`actions-box` = TRUE,style="box-celltypes"),
                      choicesOpt = list(style = rep(("font-size: 18px; line-height: 1.6;"), uniqueN(markerTableComplete()$cell_type))))
  })

  
  controlDescendant<-reactive({
    onto_igraph<-graph_from_graphnel(onto_plot, name = TRUE, weight = TRUE, unlist.attrs = TRUE)
    node<-as.data.table(unique(markerTableComplete()[celltype_species %in% input$celltype]$cell_type))
    dt<- data.table(cell_type=character(), distance=numeric())
    for (i in 1:nrow(node)){
      distan<-max(eccentricity(onto_igraph, vids = node[i]$V1, mode = c("out")))
      data<-as.data.table(t(c(node[i]$V1,distan)))
      colnames(data)<-c("cell_type","distance")
      dt<-rbind(dt,data)
    }
    as.data.table(dt)
  })


  observeEvent(input$celltype,{
    updatePickerInput(session,'descendantsof',
                      choices=unique(markerTableComplete()[species %in% input$species & cell_type %in% controlDescendant()[distance!=0]$cell_type]$celltype_species),
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

  markerTableOutput <- reactive ({
    #table with descandants merged
    if(length(input$descendantsof)!=0 & input$mergeDescendant=="Yes"){
      table_merged_desc_other(markerTableComplete(),input$celltype,mergeDescendantTable(),input$species,colnames(marker_table_complete))
    }
    #table with descendants NOT merged
    else if (length(input$descendantsof)!=0 & input$mergeDescendant=="No"){
      table_desc_other(markerTableComplete(),input$celltype,descendantTable(),input$species,colnames(marker_table_complete))
    } #table without descendant
    else if(length(input$descendantsof)==0){
      table_input_celltypes(markerTableComplete(),input$celltype,input$species,colnames(marker_table_complete))
    }

  })

  markerTablePlot <- reactive ({
    #plot with descendants
    if (length(input$descendantsof)!=0){
      table_desc_other(markerTableComplete(),input$celltype,descendantTable(),input$species,colnames(marker_table_complete))
    }
    else if(length(input$descendantsof)==0){
      table_input_celltypes(markerTableComplete(),input$celltype,input$species,colnames(markerTableComplete()))
    }

  })

  Plot <- reactive({
    if (length(input$celltype) > 1 | length(input$descendantsof)>=1)
    {  ontosubplot<-onto_plot2(cell_onto,markerTablePlot()$cell_ID ,cex=0.8)
    nodes<-as.data.table(ontosubplot@nodes)
    nodes<-nodes[,V1:=tstrsplit(nodes$V1,"CL", fixed = TRUE, keep = 1)]
    nodes<-nodes[,V1:=tstrsplit(nodes$V1,"DOID", fixed = TRUE, keep = 1)]
    
    nodes<-nodes[, V1:=str_replace_all(V1,"\n"," ")]
    nodes<-nodes[, V1:=str_sub(V1,1,nchar(V1)-1)]
    ontosubplot@nodes<-nodes$V1
    ontosubplot

    }
    else if(length(input$celltype) == 1 ){
      ontosubplot<-onto_plot(cell_onto,markerTablePlot()$cell_ID ,cex=0.8)
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
      hierac_plot1_desc(markerTableComplete(),Plot(),input$celltype,descendantTable(),input$cellid)
    }
    else{
      hierac_plot1(markerTableComplete(),Plot(),input$celltype,input$cellid)
    }

  })

  output$scalableplot <- renderUI({
    tagList(
      div(grVizOutput('plot1',height = input$height, width = input$width)))
  })



  click_plot<-reactive ({
    click_node1(markerTableComplete(),Plot(),input$celltype,input$descendantsof,mergeDescendantTable(),input$cellid)
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
      markerTableOutput()[EC_score>= str_replace_all(input$EC_score, ">=","") & database_specificity >= str_replace_all(input$database_spec, ">=|=","") & query_specificity >= str_replace_all(input$query_spec, ">=|=","")][,
       c("condition","disease_id","cell_type","cell_ID","cell_def","marker", "marker_type","EC_score","gene_description","species", "disease_specificity","database_specificity","query_specificity","reference")]
    }
    else if(length(input$celltype)!=0 & input$tabletype=="Simple"){
      if(input$mergeDescendant=="Yes"){
        markerTableOutput()[EC_score>= str_replace_all(input$EC_score, ">=","") & database_specificity >= str_replace_all(input$database_spec, ">=|=","") & query_specificity >= str_replace_all(input$query_spec, ">=|=","")
        ][,c("condition","cell_type_ancestor","cell_type","cell_ID","marker","gene_description","species","EC_score","database_specificity","query_specificity")]

      }
      else{
        markerTableOutput()[EC_score>= str_replace_all(input$EC_score, ">=","") & database_specificity >= str_replace_all(input$database_spec, ">=|=","") & query_specificity >= str_replace_all(input$query_spec, ">=|=","")
        ][,c("condition","cell_type","cell_ID","marker","gene_description","species","EC_score","database_specificity","query_specificity")]
      }
    }


  })

  output$table1 <- renderDataTable({
    outputTable()
  })


  # Downloadable csv of selected dataset

  output$downloadData <- downloadHandler(
    filename = function() {
      paste0("marker_table_complete", input$downloadType)
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
      marker_dt<-tolower(marker_table_complete$marker)
      validate(need(marker_input %in% marker_dt, "Please insert valid marker genes!"))
      gene_marker<-marker_table_complete[tolower(marker) %in% tolower(marker_vec$marker_gene)]
      gene_marker<-gene_marker[species %in% input$speciesM]
    
      if(length(input$diseaseM>0)){
        gene_marker<- gene_marker[condition %in% input$diseaseM]
        gene_marker
        }
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
    table_marker_file<-marker_table_complete[tolower(marker) %in% tolower(marker_vec$marker_gene)]
    table_marker_file <- table_marker_file[species %in% input$speciesM]
    if(length(input$diseaseM>0)){
      table_marker_file<- table_marker_file[condition %in% input$diseaseM]
      table_marker_file
      
      }
    table_marker_file
  })
  
  # Final table output: 
  #1. Marker insert by the user in the box 
  #2. Marker add through a marker file 
  #3. If both marker in the box and file marker are present add together 
  
  tableInputComplete<-reactive({
    if(nchar(input$marker)>1 & (!is.null(input$markerfile))){
      table_tot<-rbind(tableMarkerInput(),tableFileMarker())
      if(length(input$diseaseM)>0){
      table_tot<-table_tot[condition %in% input$diseaseM]
      }
      table_tot
    }
    else if(nchar(input$marker)>1  & is.null(input$markerfile)){
      table_tot<-tableMarkerInput()
      if(length(input$diseaseM)>0){
      table_tot<-table_tot[condition %in% input$diseaseM]
      }
      table_tot
    }
    else if (nchar(input$marker)==0 & !is.null(input$markerfile)) {
      table_tot<-tableFileMarker()
      if(length(input$diseaseM)>0){
      table_tot<-table_tot[condition %in% input$diseaseM]
      }
      table_tot
    }
    
  })
  
  PlotM <- reactive({
    if (nrow(tableInputComplete())>=1) {
      cell_matching<-unique(tableInputComplete()$cell_ID)
      if(length(cell_matching)>1){
        ontosubplot<-onto_plot2(cell_onto,cell_matching ,cex=0.8)
        nodes<-as.data.table(ontosubplot@nodes)
        nodes<-nodes[,V1:=tstrsplit(nodes$V1,"CL", fixed = TRUE, keep = 1)]
        nodes<-nodes[,V1:=tstrsplit(nodes$V1,"DOID", fixed = TRUE, keep = 1)]
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
    hierac_plot2(marker_table_complete, PlotM(), tableInputComplete(),input$cellidM) 
    
  })
  
  output$scalableplotM <- renderUI({ 
    tagList(
      div(grVizOutput('plot1M',height = input$heightM, width = input$widthM)))
  })
  
  
  
  click_plotM<-reactive ({
    click_node2(marker_table_complete, ontology_celltype, PlotM(), tableInputComplete(),input$cellidM, ontology_def)
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
      tableInputComplete()[EC_score >= str_replace_all(input$EC_scoreM, ">=","") & database_specificity >= str_replace_all(input$database_specM, ">=|=","")]
    }
    else if(nrow(tableInputComplete())!=0 & input$tabletypeM=="Simple"){
      tableInputComplete()[EC_score>= str_replace_all(input$EC_scoreM, ">=","") & database_specificity >= str_replace_all(input$database_specM, ">=|=","")
      ][,c("condition","cell_type","cell_ID","marker","gene_description","species","EC_score","database_specificity")]
    }
    
    
  })  
  
  output$table1M <- renderDataTable({
    outputTableM()
  })
  
  # Downloadable csv of selected dataset 
  
  output$downloadDataM <- downloadHandler(
    filename = function() {
      paste0("marker_table_complete", input$downloadTypeM)
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
  
  
  
  ###### server for annotation
  
  observeEvent(input$userclusterfileinfo,{
    showModal(modalDialog(
      title = "Marker genes input file",
      HTML("Load your marker genes file to annotate. <br>
      The file could be: 
      <ul><li> <strong> <em> FindAllMarkers </em> output </strong> as txt, xlsx, csv or tsv file</li>
           <strong> Example </strong>"),
      HTML("<img src=findallmarker_ex.png>"),
      HTML("<br>"),
      HTML("<li> <strong> Custom table </strong> with at least one column contaning marker genes (one per row).
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
        
        positive_marker[,gene %in% marker_table_complete[species %in% input$speciesA]$marker]
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
        
        negative_marker[,gene %in% marker_table_complete[species %in% input$speciesA]$marker]
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
  })  
  
  
  
  accMarkerAnnoTable <- reactive ({
    marker_table_complete_filt<-marker_table_complete[EC_score>= str_replace_all(input$EC_scoreA, ">=","") & database_specificity >= str_replace_all(input$database_specA, ">=|=","")]
    positive_marker_acc<-marker_table_complete_filt[marker_type=="positive" & species %in% input$speciesA][,pos_marker_acc:= paste(marker, collapse=" "), by= cell_type]
    positive_marker_acc<-unique(positive_marker_acc[,c("cell_type","pos_marker_acc")])
    negative_marker_acc<-marker_table_complete_filt[marker_type=="negative" & species %in% input$speciesA][,neg_marker_acc:= paste(marker, collapse=" "), by= cell_type]
    negative_marker_acc<-unique(negative_marker_acc[,c("cell_type","neg_marker_acc")])
    
    accordion_marker<-merge(positive_marker_acc,negative_marker_acc, by="cell_type",all.x=TRUE, all.y=TRUE)
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
      univ<- uniqueN(unique(rbind(marker_table_complete$marker, inputseppos$pos_marker, inputsepneg$neg_marker)))
      withProgress(message = 'Making cluster identification', value = 0, {
        for (cl in 1:nrow(inputTable())-1){
          sub_dt<-combineTable()[cluster==cl]
          # Increment the progress bar, and update the detail text.
          incProgress(1/nrow(inputTable())-1, detail = paste("Doing cluster", cl))
          # Pause for 0.1 seconds to simulate a long computation.
          Sys.sleep(0.1)
          for (ct in (accMarkerAnnoTable()$cell_type)){
            #print(ct)
            sub_ct_dt<-sub_dt[cell_type==ct]
            IY<- count.fields(textConnection(sub_ct_dt$tot_overlap), sep = " ") # in input, in anno (overlap)
            IN<- count.fields(textConnection(sub_ct_dt$in_input_not_anno_tot), sep = " ") # in input, not in anno
            BY<- count.fields(textConnection(sub_ct_dt$not_input_in_anno_tot), sep = " ") # not in input, in anno
            BN<- univ - (IY+IN+BY) # not in input, not in anno 
            fisher_data <- fisher.test(matrix(c(IY, BY, IN, BN), 2, 2), alternative="greater") 
            # generation of the output (a tibble with 1 row)
            out_df <- rbind(out_df, tibble("cluster"=cl, # name of the input
                                           "cell_type"=ct, # name of the annotation
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
      dt_plot<-merge(annoResultsTable(),ontology_celltype, by="cell_type")
      dt_plot<-dt_plot[,c("cell_type","cell_ID")]
      dt_plot
    })
    
    PlotA <- reactive({
      if (nrow(tableInputComplete())>=1) {
        cell_matching<-unique(tablePlotOntology()$cell_ID)
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
    
    
    output$plot1A <- renderGrViz({ 
      hierac_plot2(marker_table_complete, PlotA(), tablePlotOntology(),input$cellidA) 
      
    })
    
    output$scalableplotA <- renderUI({ 
      tagList(
        div(grVizOutput('plot1A',height = input$heightA, width = input$widthA)))
    })
    
    
    
    click_plotA<-reactive ({
      click_node2(marker_table_complete, ontology_celltype, PlotA(), tablePlotOntology(),input$cellidA, ontology_def)
    })
    
    
    txtA <- reactive({
      req(input$plot1A_click)
      nodeval <- input$plot1A_click$nodeValues[1:length(input$plot1A_click$nodeValues)]
      nodeval<-as.data.table(paste(nodeval,sep="", collapse=" "))
      return(  click_plotA()[label %in% nodeval$V1]$cell_def)
      
    })
    output$cell_type_defA <- renderText({
      req(txtA())
      txtA()
    })
    
  })
  
  
  
}



shinyApp(ui = ui, server = server, options=list(port=8200))


