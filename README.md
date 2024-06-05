# ClimaticSpace
This repository includes the material (R codes) used for processing, analysing and create the figures of the study 'Assessing the climatic space representation of vascular plants occurrence records based on their publicly available distributional information' written by Ronquillo et al (2024)

---
title: Workflow
---
flowchart TD

    A[[Download data from GBIF]] --> |DownloadGBIF_data.R|BB[[Processing records]]
    BB --> |Script2.R|C[(Database)]
    AA[[Create Global Climatic Space]] --> |EnvSpace1.R|CC{Climatic Spaces of Orders}
    C --> CC[[Climatic Spaces of Orders and species]]
    C --> D[[Assign Köopen-Geiger Class]]
    D --> |Script4_Koppen.R| E(Visualisations)
    CC --> |EnvSpace2.R|E>Visualisations]
    E --> |Figures.R|F(OUTPUTS)
