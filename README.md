# ClimaticSpace
This repository includes the material (scripts in R language) used for processing, analysing and creating the figures of the study 'Climatic space representation of (occurrence records for) vascular plants based on their geographical distribution' written by Ronquillo et al. (2024)
---
# Workflow
The following **diagram** describes the workflow in which the scripts are organised to reproduce the methods sections of the paper.

```mermaid
flowchart TD
    A[[Download data from GBIF]] --> |DownloadGBIF_data.R|BB[[Processing records]]
    BB --> |OCCUR_DataCuration.R|C[(Database <br> *see Figshare repository)]
    AA[[Create Global Climatic Space]] --> |EnvSpace1.R|CC{Climatic Spaces of Orders}
    C --> CC[[Climatic Spaces of Orders and species]]
    C --> D[[Assign Köppen-Geiger Class]]
    D --> |Script_Koppen.R| E(Visualisations)
    CC --> |EnvSpace2.R|E>OUTPUTS <br> *see Figshare repository]
    E --> |Figures.R|F(Visualisations)

  style A fill:dodgerblue, stroke:#333,stroke-width:2px,color:#fff;
  style AA fill:gold, stroke:#333,stroke-width:2px,color:#333;
  style BB fill:dodgerblue, stroke:#333,stroke-width:2px,color:#fff;
  style CC fill:gold, stroke:#333,stroke-width:2px,color:#333;
  style C fill:lightgrey, stroke:#333,stroke-width:2px,color:#333;
  style D fill:#fee0b6, stroke:#333,stroke-width:2px,color:#333;
  style E fill:#35978f, stroke:#333,stroke-width:2px,color:#fff;
  style F fill:#35978f, stroke:#333,stroke-width:2px,color:#fff;
```
