# The Cell Marker Accordion 
A Web tool for single-cell and spatial RNA-seq cell types annotation: https://rdds.it/CellMarkerAccordion/
![Logo](https://user-images.githubusercontent.com/68125242/161058801-e3a83d1b-f12f-4cde-89e7-2a1207e99149.png)

A critical step in single-cell and spatial data analysis is the accurate annotation of cell types. The inherent heterogeneity of single-cell data, combined with significant inconsistencies in annotation methodologies, often results in noisy and unreliable classifications. These discrepancies can hide biological insights and hinder reproducibility.
To address this issue, we developed Cell Marker Accordion, a user-friendly platform that includes both an R package (available at https://github.com/TebaldiLab/cellmarkeraccordion) and a Shiny app (online version available at: https://rdds.it/CellMarkerAccordion/), designed as a harmonization framework. By leveraging filtering, standardization, and integration, it systematically refines the contributions of multiple gene marker databases and cell sorting marker sources, distinguishing positive from negative markers. This process ensures a more consistent and reliable annotation, ultimately enhancing the clarity and interpretability of single-cell and spatial datasets. 
The Cell Marker Accordion database includes thousand of markers associated with both human and mouse cell types from different tissues, in physiological and pathological conditions. 
The Cell Marker Accordion web interface allows users to easily explore the integrated built-in database of consistency-weighted markers. 

## Citing the cellmarkeraccordion
Please cite the following article when using the cellmarkeraccordion:

<strong>Cell Marker Accordion: interpretable single-cell and spatial omics annotation in health and disease</strong>

Emma Busarello, Giulia Biancon, Ilaria Cimignolo, Fabio Lauria, Zuhairia Ibnat, Christian Ramirez, Gabriele Tomè, Marianna Ciuffreda, Giorgia Bucciarelli, Alessandro Pilli, Stefano Maria Marino, Vittorio Bontempi, Kristin R. Aass, Jennifer VanOudenhove, Maria Caterina Mione, Therese Standal, Paolo Macchi, Gabriella Viero, Stephanie Halene, Toma Tebaldi

bioRxiv 2024.03.08.584053; doi: https://doi.org/10.1101/2024.03.08.584053 

## Download the Accordion database
To download the Accordion database as an excel file you can click on the download button on the shiny app (https://rdds.it/CellMarkerAccordion/). 
Otherwise downalod the "AccordionDB.xlsb" file sotred in this repository.

## Local installation 
### Prerequisites

Before installing the Cell Marker Accordion shiny web app, ensure you have the following:
- R (Version 4.0 or higher) – Download from CRAN (https://cran.r-project.org/).
- RStudio (Recommended) – Download from RStudio (https://posit.co/downloads/).
- Python (Version 3.6 or higher) – Download from Python.org (https://www.python.org/).

 ### 1. Clone the Repository:   
```bash
git clone https://github.com/TebaldiLab/shiny_cellmarkeraccordion.git
```
Alternatively, download the ZIP file and extract it.

### 2. Run the Installation Script:
Open R or RStudio, navigate to the cloned repository folder, and execute:
```bash
source("install_dependencies.R")
```
This script will:
- Install required R packages
- Set up the necessary Python environment
- Install Python dependencies

### 3. Run the Shiny application
```
library(shiny)
runApp("path/to/shiny_cellmarkeraccordion")
```

## Usage
The Cell Marker Accordion web interface allows to easily:  <br />

<img src="https://github.com/TebaldiLab/shiny_cellmarkeraccordion/assets/68125242/a2d0501b-2b59-48f4-b220-be7871b57b95" width="25" height="25"> Search and download lists of marker genes by cell types across different tissues, both in healthy and pathological conditions. The app allows the user to upload its custom sets of positive and/or negative set marker genes to be integrated into the Accordion repository.  <br />

<img src=https://github.com/user-attachments/assets/9e719956-ad4b-435f-b5db-609e4f186144 style="width:50%; height:50%;"> <br />

 <img src="https://github.com/TebaldiLab/shiny_cellmarkeraccordion/assets/68125242/12884ed0-0d8b-47cd-9e48-624ecbee467b" width="25" height="25"> Search and download lists of cell types by marker genes.  <br />

<img src=https://github.com/user-attachments/assets/23399d3f-8a45-41ed-ba81-8890abe51aa9 style="width:50%; height:50%;"> <br />

<img src= "https://github.com/TebaldiLab/shiny_cellmarkeraccordion/assets/68125242/69e25ec8-75e9-42bd-8154-b446830e52a4" width="25" height="25"> Browse hierarchies of cell types following the Cell Ontology structure in order to obtain the desired level of specificity in the markers in both searches options.  <br /> 
 <img src=https://github.com/TebaldiLab/shiny_cellmarkeraccordion/assets/68125242/2b87c289-1159-45c6-a855-54528a168393 style="width:25%; height:25%;">  <br />

<img src="https://github.com/TebaldiLab/shiny_cellmarkeraccordion/assets/68125242/3b01fd45-6bbc-4c2d-96a8-eb455fec72e9" width="25" height="25"> Rank and select marker genes by query or
database specificity and by their evidence consistency scores.  <br />

 <img src=https://github.com/user-attachments/assets/cac2b4b5-00d0-4d24-869c-35d892d9039f style="width:80%; height:80%;">  <br />

 <img src= "![gear-solid](https://github.com/user-attachments/assets/c7f589a9-be28-4713-b86f-40ef58507bcf)" width="25" height="25"> Integrate custom set of marker genes with the Cell Marker Accordion database

<img src="https://github.com/TebaldiLab/shiny_cellmarkeraccordion/assets/68125242/4030d4a7-a365-4c6c-b6fa-0acefaceeb9d" width="25" height="35"> User can upload a file containing markers for every cluster, or just related to one entity, and the tool retrieves the respective cell type with the highest correlation.  <br />

 <img src=https://github.com/user-attachments/assets/941a29a7-e0e7-4cf8-8144-51904ae92ddf style="width:50%; height:50%;">  <br />

