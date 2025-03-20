#The application utilizes Python for certain functionalities. Ensure you have Python installed (version 3.6 or higher).
#You can download it from the official Python website (https://www.python.org/).
#Next, set up the Python environment using the reticulate package:

if (!"reticulate" %in% installed.packages()[,"Package"]) {
  install.packages("reticulate", dependencies = T)
}
if (!py_available(initialize = TRUE)) {
  install_miniconda()
}
#if OntoProc not already installed create python environment:

if(!("ontoProc" %in% installed.packages()[,"Package"])){

  library(reticulate)
  # Create a virtual environment
  venv_path <- file.path("HOME", ".virtualenvs", "r-reticulate")
  if (!dir.exists(venv_path)) {
    virtualenv_create(venv_path)
  }
  # Use the virtual environment
  use_virtualenv(venv_path, required = TRUE)
  # Install necessary Python packages
  py_install("owlready2", envname = venv_path)
}

#Install Required R Packages
list.of.packages <- c("rstudioapi","shinydashboard","shiny","stringr","DiagrammeR","igraph",
                      "shinyWidgets","ontologyIndex","data.table","plyr","ontologyPlot",
                      "writexl", "dplyr", "shinyBS","shinyhelper","DT","shinyjs",
                      "shinydashboardPlus", "readxl","rstatix", "tidyverse","rsvg","DiagrammeRsvg")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) suppressMessages(suppressWarnings({install.packages(new.packages, dependencies = T)}))


if (!require("BiocManager", quietly = TRUE))
  install.packages("BiocManager")
list.of.packages.bio<-c("ontoProc")
new.packages_bio <- list.of.packages.bio[!(list.of.packages.bio %in% installed.packages()[,"Package"])]
if(length(new.packages_bio))
  suppressMessages(suppressWarnings({BiocManager::install(new.packages_bio,dependencies = TRUE)}))

