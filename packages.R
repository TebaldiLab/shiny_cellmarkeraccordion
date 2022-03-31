if(!require("rstudioapi",quietly=TRUE)){
  install.packages("rstudioapi")
}

if (!require("devtools"))
  install.packages("devtools")

if(!require("shiny",quietly=TRUE))
devtools::install_github("shiny", "rstudio")

if(!require("stringr"))
  install.packages("stringr")

if(!require("DiagrammeR"))
  install.packages("DiagrammeR")

if(!require("igraph"))
  install.packages("igraph")

if(!require("shinyWidgets"))
  install.packages("shinyWidgets") 

if(!require("ontologyIndex"))
  install.packages("ontologyIndex")

if(!require("plyr"))
  install.packages("plyr")

if(!require("data.table"))
  install.packages("data.table")

if(!require("ontologyPlot"))
  install.packages("ontologyPlot")

if (!require("BiocManager", quietly = TRUE))
  install.packages("BiocManager")

if (!require("ontoProc", quietly = TRUE))
  BiocManager::install("ontoProc")

if(!require("writexl"))
  install.packages("writexl")

if(!require("dplyr"))
  install.packages("dplyr")

if(!require("shinyBS"))
  install.packages("shinyBS")

if(!require("shinyhelper"))
  install.packages("shinyhelper")

if(!require("shinydashboardPlus"))
  install.packages("shinydashboardPlus")