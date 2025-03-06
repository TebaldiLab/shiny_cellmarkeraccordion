# TheCellMarkerAccordion 
A Web tool for single-cell and spatial RNA-seq cell types annotation
![Logo](https://user-images.githubusercontent.com/68125242/161058801-e3a83d1b-f12f-4cde-89e7-2a1207e99149.png)

A critical step in single-cell and spatial data analysis is the accurate annotation of cell types. The inherent heterogeneity of single-cell data, combined with significant inconsistencies in annotation methodologies, often results in noisy and unreliable classifications. These discrepancies can hide biological insights and hinder reproducibility.
To address this issue, the Cell Marker Accordion approach was developed as a harmonization framework. By leveraging filtering, standardization, and integration, it systematically refines and balances the contributions of multiple gene marker databases. This process ensures a more consistent and reliable annotation, ultimately enhancing the clarity and interpretability of single-cell and spatial datasets. 
The Cell Marker Accordion database includes thousand of markers associated with both human and mouse cell types from different tissues, in physiological and pathological conditions.

## Running the app
**In order to run the App you have to download the entire repository "shiny_cellmarkeraccordion" e run the "app.R" script (the necessary packages and their compatible versions are listed in the *Description.txt").** Otherwise you can use the online platform at: https://rdds.it/CellMarkerAccordion/

## Download the Accordion database
To download the Accordion database as an excel file you can click on the download button on the shiny app or directly here: https://rdds.it/CellMarkerAccordion/session/d02bff5678393a96699abc3160dde89e/download/downloadAccordionDB?w=
Otherwise downalod the "AccordionDB.xlsb" file sotred in this repository.

## The Cell Marker Accordion R package
The R package of the Cell Marker Accordion is available at: https://github.com/TebaldiLab/cellmarkeraccordion

## Shiny app
The Cell Marker Accordion web interface allows to easily:  <br />

<img src="https://github.com/TebaldiLab/shiny_cellmarkeraccordion/assets/68125242/a2d0501b-2b59-48f4-b220-be7871b57b95" width="25" height="25"> Search and download lists of marker genes by cell types across different tissues, both in healthy and pathological conditions. The app allows the user to upload its custom sets of positive and/or negative set marker genes to be integrated into the Accordion repository.  <br />

<img src=https://github.com/user-attachments/assets/96363569-7b09-4eaf-b267-aea3558dea9a style="width:50%; height:50%;">

 <br />

 <img src="https://github.com/TebaldiLab/shiny_cellmarkeraccordion/assets/68125242/12884ed0-0d8b-47cd-9e48-624ecbee467b" width="25" height="25"> Search and download lists of cell types by marker genes.  <br />

<img src=https://github.com/user-attachments/assets/23399d3f-8a45-41ed-ba81-8890abe51aa9 style="width:50%; height:50%;">

 <br />
<img src= "https://github.com/TebaldiLab/shiny_cellmarkeraccordion/assets/68125242/69e25ec8-75e9-42bd-8154-b446830e52a4" width="25" height="25"> Browse hierarchies of cell types following the Cell Ontology structure in order to obtain the desired level of specificity in the markers in both searches options.  <br />
 <br />
 
 <img src=https://github.com/TebaldiLab/shiny_cellmarkeraccordion/assets/68125242/2b87c289-1159-45c6-a855-54528a168393 style="width:25%; height:25%;">

 <br />
<img src="https://github.com/TebaldiLab/shiny_cellmarkeraccordion/assets/68125242/3b01fd45-6bbc-4c2d-96a8-eb455fec72e9" width="25" height="25"> Rank and select marker genes by query or
database specificity and by their evidence consistency scores.  <br />
<br />

 <img src=https://github.com/user-attachments/assets/cac2b4b5-00d0-4d24-869c-35d892d9039f style="width:80%; height:80%;">

 <br />
<img src="https://github.com/TebaldiLab/shiny_cellmarkeraccordion/assets/68125242/4030d4a7-a365-4c6c-b6fa-0acefaceeb9d" width="25" height="35"> User can upload a file containing markers for every cluster, or just related to one entity, and the tool retrieves the respective cell type with the highest correlation.  <br />
<br />

 <img src=https://github.com/user-attachments/assets/941a29a7-e0e7-4cf8-8144-51904ae92ddf style="width:50%; height:50%;">

