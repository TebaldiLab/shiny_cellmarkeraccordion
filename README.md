# The Cell Marker Accordion 
A Web tool for single-cell and spatial RNA-seq cell types annotation: https://rdds.it/CellMarkerAccordion/
![Logo](https://user-images.githubusercontent.com/68125242/161058801-e3a83d1b-f12f-4cde-89e7-2a1207e99149.png)

A critical step in single-cell and spatial data analysis is the accurate annotation of cell types. The inherent heterogeneity of single-cell data, combined with significant inconsistencies in annotation methodologies, often results in noisy and unreliable classifications. These discrepancies can hide biological insights and hinder reproducibility.
To address this issue, we developed Cell Marker Accordion, a user-friendly platform that includes both an [R package](https://github.com/TebaldiLab/cellmarkeraccordion) and a [Shiny app](https://rdds.it/CellMarkerAccordion/), designed as a harmonization framework. By leveraging filtering, standardization, and integration, it systematically refines the contributions of multiple gene marker databases and cell sorting marker sources, distinguishing positive from negative markers. This process ensures a more consistent and reliable annotation, ultimately enhancing the clarity and interpretability of single-cell and spatial datasets. 
The Cell Marker Accordion database includes thousand of markers associated with both human and mouse cell types from different tissues, in physiological and pathological conditions. 
The Cell Marker Accordion web interface allows users to easily explore the integrated built-in database of consistency-weighted markers. 

## Citing the cellmarkeraccordion
Please cite the following article when using the <strong>cellmarkeraccordion</strong>:

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
If Git is not installed, you can:
- Download and install Git from https://git-scm.com/.
- Alternatively, download the ZIP file and extract it.

### 2. Run the Installation Script:
Open R or RStudio, navigate to the cloned repository folder, and execute:
```bash
setwd("path/to/shiny_cellmarkeraccordion")
source("install_dependencies.R")
```
This script will:
- Install required R packages
- Set up the necessary Python environment
- Install Python dependencies

### 3. Run the Shiny application
```
library(shiny)
runApp()
```

## Usage
The Cell Marker Accordion web interface allows to easily:  <br />

### <img src="https://github.com/TebaldiLab/shiny_cellmarkeraccordion/assets/68125242/a2d0501b-2b59-48f4-b220-be7871b57b95" width="25" height="25"> <strong>Search and download lists of marker genes by cell types across different tissues, both in healthy and pathological conditions.</strong> <br />

Inputs: 
- Select species: currently Human and/or Mouse.
- Condition: healthy or multiple diseases.
- Tissue: select one or multiple tissues from the list. When the tissue_aware button is enabled, tissue specificity is maintained (i.e., tissues remain separate). Otherwise tissues selected will be combined and analyzed together.
- Cell type: select one or multiple cell types from the list.
- See subtypes of: displays the list of cell type descendants of the previously selected cell types. Users can select one or more subtypes to visualize in the Ontology tree and the output table.

<img src= https://github.com/user-attachments/assets/e63315f0-7711-4cc5-b774-e0cfb619e7db style="width:80%; height:80%;"> <br />

Outputs:
- Hierarchies of input cell types following the Cell Ontology structure

<img src= https://github.com/user-attachments/assets/a61cdc52-d5a3-45b3-ac1d-bfcf0a072a60 style="width:20%; height:20%;"> <br />  <br />

- Interactive and downloadable table with lists of markers genes associated to the input cell types

<img src= https://github.com/user-attachments/assets/7a81788a-f3e6-4410-bbb6-1165e54de993  style="width:100%; height:100%;"> <br />

### <img src="https://github.com/TebaldiLab/shiny_cellmarkeraccordion/assets/68125242/12884ed0-0d8b-47cd-9e48-624ecbee467b" width="25" height="25"> <strong>Search and download lists of cell types associate with input marker genes across different tissues in health and disease.</strong> <br />

<strong>Inputs: </strong> 
- Select species: currently Human and/or Mouse.
- Inser marker genes: enter a list of marker genes, using | , : ; ! ? as delimiters to separate them.
- Upload file with marker genes: provide a .txt, .csv, .xlsx, or .tsv file containing a list of marker genes. Use | , : ; ! ? as delimiters to separate them. Both the markers entered in the "Insert marker genes" box and those in the uploaded file will be considered. 
- Condition: healthy or multiple diseases.
- Tissue: select one or multiple tissues from the list. When the tissue_aware button is enabled, tissue specificity is maintained (i.e., tissues remain separate). Otherwise tissues selected will be combined and analyzed together.

<img src=https://github.com/user-attachments/assets/23399d3f-8a45-41ed-ba81-8890abe51aa9 style="width:30%; height:30%;"> <br />

<strong>Outputs:</strong>
- Hierarchies of input cell types following the Cell Ontology structure
- Interactive and downloadable table with lists of cell types associated to the input markers


### <img src= "https://github.com/TebaldiLab/shiny_cellmarkeraccordion/assets/68125242/69e25ec8-75e9-42bd-8154-b446830e52a4" width="25" height="25"> <strong>Browse hierarchies of cell types following the Cell Ontology structure in order to obtain the desired level of specificity in the markers in both searches options.</strong>  <br /> 
Users can also explore the subtypes of a selected cell type.
For example, to view all subtypes of the monocyte population:

<ol>
  <li>Select <strong>"monocyte"</strong> in the <strong>Cell type</strong> box.</li>
  <li>Select <strong>"monocyte"</strong> in the <strong>See subtypes of</strong> box.</li>
  <li>The <strong>Ontology Tree</strong> will display all monocyte subtypes.</li>
  <li>Click on any node to explore detailed definitions of cell types based on the <strong>Cell Ontology Tree</strong>.</li>
</ol>

<img src= https://github.com/user-attachments/assets/753224d1-d0dd-4463-8691-ab201a68693c style="width:50%; height:50%;"> <br />

### <img src="https://github.com/TebaldiLab/shiny_cellmarkeraccordion/assets/68125242/3b01fd45-6bbc-4c2d-96a8-eb455fec72e9" width="25" height="25"><strong> Rank and select marker genes by SPs (specificity score) and by their ECs (evidence consistency scores) </strong>.  <br />
<strong>Filters:</strong>
- ECs: evidence consistency score, measuring the agreement of different annotation sources. Filter marker genes with an evidence consistency score >= the selected value.
- SPs: specificity score, indicating whether a gene is a marker for different cell types present in all the accordion database. Filter marker genes with a specificity score >= the selected value.
Table type:
  - Simple: provides a compact table with fewer columns for easier viewing
  - Complete: displays the full database, including detailed mapping relationships for diseases, tissues, and cell types to the Disease Ontology, Uberon Ontology, Cell Ontology and NCIT.<br />
  
<img src= https://github.com/user-attachments/assets/aca61132-4bd7-47f8-b0fd-bb0280c8bdcb style="width:80%; height:80%;">  <br />

<img src=https://github.com/user-attachments/assets/cac2b4b5-00d0-4d24-869c-35d892d9039f style="width:80%; height:80%;">  <br />

### <img src= "https://github.com/user-attachments/assets/c7f589a9-be28-4713-b86f-40ef58507bcf" width="25" height="25"><strong> Integrate custom set of marker genes with the Cell Marker Accordion database.</strong>

<strong>Inputs:</strong>
- Custom marker genes: upload a custom set of marker genes to be integrate with the Cell Marker Accordion database.
 The file must contain at least two columns:
  - <strong>cell_type</strong>: specifies the cell type.<br>
    To ensure proper integration, cell types nomenclature should be standardized on the Cell Ontology or the NCI Thesaurus.
    If non-standardized cell types are provided, they will be added as new cell types in the database.
  - <strong>marker</strong>: lists the marker genes<br>
  
  Additional columns can also be included:
  - <strong>species</strong>: Specifies the species (default: Human).
  - <strong>tissue</strong>: Specifies the related tissue. Standardization with Uberon Ontology is recommended for effective integration.<br>
    Non-standardized tissues will be added as new tissues. If omitted, integration will ignore tissue specificity.<br>
  - <strong>marker_type</strong>: Defines marker type (positive or negative; default: positive).
  - <strong>resource</strong>: Indicates the data source. If omitted, markers are labeled as custom_set.
  - <strong>disease</strong>: Required if the integration is performed with the disease database. Standardization with Disease Ontology is recommended.<br>
    Non-standardized diseases will be added as new diseases. If omitted, disease specificity is ignored.

- Select the Accordion database: select the Accordion database to integrate with your custom set of marker genes, either healthy or disease. <br>

 <img src= https://github.com/user-attachments/assets/90f3aa3b-b9fa-4cf0-b524-aab1dc698ceb style="width:80%; height:80%;">  <br />

<strong>Outputs</strong>
- Interactive and downloadable integrated table  

### <img src="https://github.com/TebaldiLab/shiny_cellmarkeraccordion/assets/68125242/4030d4a7-a365-4c6c-b6fa-0acefaceeb9d" width="25" height="35"><strong> Annotate cell populations in health and disease.</strong>  <br />
User can upload a file containing markers for every cluster, or just related to one entity, and the Cell Marker Accordion will retrieves the respective cell type with the highest correlation. <br>
<strong> IMPORTANT!</strong> Annotation is performed using Fisher's exact test to identify significant associations between the input gene list and cell type-specific markers in the Cell Marker Accordion database.
For full access to the Cell Marker Accordion algorithm, we recommend using our R package.

<strong>Inputs:</strong>
- Load file to annotate: upload a file for automatic annotation. The file can be one of the following types:
  - The Output table generated by the FindAllMarkers function in the Seurat package, as txt, xlsx, csv or tsv file
    ### Example of FindAllMarkers output table:
  
    <table>
      <tr>
        <th>cluster</th>
        <th>p_val</th>
        <th>avg_log2FC</th>
        <th>pct.1</th>
        <th>pct.2</th>
        <th>p_val_adj</th>
        <th>gene</th>
      </tr>
      <tr>
        <td>0</td> <td>0</td> <td>1.196</td> <td>0.327</td> <td>0.135</td> <td>0</td> <td>CCR7</td>
      </tr>
      <tr>
        <td>0</td> <td>0</td> <td>1.178</td> <td>0.320</td> <td>0.122</td> <td>0</td> <td>LEF1</td>
      </tr>
      <tr>
        <td>0</td> <td>0</td> <td>0.999</td> <td>0.447</td> <td>0.275</td> <td>0</td> <td>SELL</td>
      </tr>
      <tr>
        <td>0</td> <td>0</td> <td>0.990</td> <td>0.789</td> <td>0.612</td> <td>0</td> <td>TMEM66</td>
      </tr>
      <tr>
        <td>0</td> <td>0</td> <td>0.985</td> <td>0.544</td> <td>0.306</td> <td>0</td> <td>CD27</td>
      </tr>
      <tr>
        <td>0</td> <td>0</td> <td>0.972</td> <td>0.765</td> <td>0.466</td> <td>0</td> <td>CD3E</td>
      </tr>
      <tr>
        <td>0</td> <td>0</td> <td>0.957</td> <td>0.359</td> <td>0.200</td> <td>0</td> <td>PIK3IP1</td>
      </tr>
      <tr>
        <td>0</td> <td>0</td> <td>0.948</td> <td>0.620</td> <td>0.411</td> <td>0</td> <td>GIMAP7</td>
      </tr>
      <tr>
        <td>0</td> <td>0</td> <td>0.921</td> <td>0.275</td> <td>0.133</td> <td>0</td> <td>MAL</td>
      </tr>
      <tr>
        <td>0</td> <td>0</td> <td>0.894</td> <td>0.641</td> <td>0.447</td> <td>0</td> <td>NOSIP</td>
      </tr>
      <tr>
        <td>1</td> <td>0</td> <td>2.879</td> <td>0.727</td> <td>0.037</td> <td>0</td> <td>CD8B</td>
      </tr>
      <tr>
        <td>1</td> <td>0</td> <td>2.114</td> <td>0.390</td> <td>0.009</td> <td>0</td> <td>RP11-291B21.2</td>
      </tr>
      <tr>
        <td>1</td> <td>0</td> <td>1.997</td> <td>0.316</td> <td>0.020</td> <td>0</td> <td>S100B</td>
      </tr>
      <tr>
        <td>1</td> <td>0</td> <td>1.568</td> <td>0.339</td> <td>0.032</td> <td>0</td> <td>CD8A</td>
      </tr>
      <tr>
        <td>1</td> <td>0</td> <td>0.905</td> <td>0.513</td> <td>0.255</td> <td>0</td> <td>RGS10</td>
      </tr>
      <tr>
        <td>1</td> <td>0</td> <td>0.633</td> <td>0.808</td> <td>0.460</td> <td>0</td> <td>CD3D</td>
      </tr>
      <tr>
        <td>1</td> <td>0</td> <td>0.623</td> <td>0.699</td> <td>0.438</td> <td>0</td> <td>NOSIP</td>
      </tr>
      <tr>
        <td>1</td> <td>0</td> <td>0.593</td> <td>1.000</td> <td>0.989</td> <td>0</td> <td>RPL31</td>
      </tr>
      <tr>
        <td>1</td> <td>0</td> <td>0.591</td> <td>0.999</td> <td>0.973</td> <td>0</td> <td>RPL34</td>
      </tr>
      <tr>
        <td>1</td> <td>0</td> <td>0.583</td> <td>0.999</td> <td>0.981</td> <td>0</td> <td>RPS25</td>
      </tr>
    </table>
   
  - Alternatively, user can provide a custom table with at least one column (<strong>gene column</strong>) containing the list of genes (one per row). <br>
    By default, all genes are considered positive (high expression) and associated with a single identity class (one cluster only).<br>
    User may also include additional columns:
       - <strong>cluster</strong>: indicate the identity class of the markers
       - <strong>gene_type</strong>: positive, whether the gene is positive (high expression) or negative, whether the gene is negative (low expression) </li>

    ### Example of custom table:  
  
     <table>
       <tr>
         <th>cluster</th>
         <th>cene</th>
         <th>gene_type</th>
       </tr>
       <tr>
         <td>0</td> <td>CCR7</td> <td>positive</td>
       </tr>
       <tr>
         <td>0</td> <td>TMEM66</td> <td>positive</td>
       </tr>
       <tr>
         <td>0</td> <td>HLA-DRA</td> <td>negative</td>
       </tr>
       <tr>
         <td>0</td> <td>CD74</td> <td>negative</td>
       </tr>
       <tr>
         <td>1</td> <td>S100B</td> <td>positive</td>
       </tr>
       <tr>
         <td>1</td> <td>RGS10</td> <td>positive</td>
       </tr>
       <tr>
         <td>1</td> <td>KLF6</td> <td>negative</td>
       </tr>
       <tr>
         <td>1</td> <td>CLIC1</td> <td>negative</td>
       </tr>
       <tr>
         <td>2</td> <td>VIM</td> <td>positive</td>
       </tr>
       <tr>
         <td>2</td> <td>GSTK1</td> <td>positive</td>
       </tr>
       <tr>
         <td>2</td> <td>CTSW</td> <td>negative</td>
       </tr>
       <tr>
         <td>2</td> <td>ZFA51</td> <td>negative</td>
       </tr>
     </table>
 

The number of positive and negative genes to retain for each cluster can be specified by entering the desired number in the box and clicking the 'Add value' button. <br/>
In the FindAllMarkers output, genes are classified as positive if log2FC > 0 and negative if log2FC < 0. <br/>
In a custom table, this classification can be defined in the 'gene_type' column. <br/>

 <img src= https://github.com/user-attachments/assets/84ce4132-3030-461b-a41f-f27fd0e87dbf style="width:30%; height:30%;">  <br />


Once the custom file is loaded you can specificy some filters for the Cell Marker Accordion database prior annotation.
In particular:
- Select species: currently Human and/or Mouse.
- Condition: healthy or multiple diseases.
- Tissue: select one or multiple tissues from the list. If no tissue is selected, the annotation will be performed using all tissues combined. If multiple tissues are selected, the annotation will be performed by combining the selected tissues.
- Cell type: select one or multiple cell types from the list. If no cell type is selected, the annotation will be performed using all cell types.
- ECs: evidence consistency score, measuring the agreement of different annotation sources. Filter marker genes from the Cell Marker Accordion Accordion database with an evidence consistency score >= the selected value.
- SPs: specificity score, indicating whether a gene is a marker for different cell types present in all the accordion database. Filter marker genes from the Accordion database with a specificity score >= the selected value.
- Maximum number of markers to keep for each cell type: indicate the top N marker genes to keep for each cell type to perform automatic annotation. Markers are ordered according to their ECs and SPs. Default is 50. 

 <img src=https://github.com/user-attachments/assets/1ae6fdba-d0f8-497b-b035-f5290d63deb7  style="width:40%; height:40%;">  <br />

<strong>Outputs:</strong>
- Interactive and downloadable Annotation table: for each cluster identity the cell type with the highest association is reported. The table also includes significance information and overlapping genes.

 <img src= https://github.com/user-attachments/assets/2cb1fa00-fc5a-46ff-8883-deb7827b1ad3   style="width:100%; height:100%;">  <br />
