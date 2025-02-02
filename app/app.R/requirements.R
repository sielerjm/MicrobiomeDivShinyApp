# CRAN packages
install.packages(c(
  "shiny",
  "tidyverse",
  "phyloseq",
  "vegan",
  "DT",
  "cowplot"
))

# Bioconductor packages
if (!require("BiocManager", quietly = TRUE))
    install.packages("BiocManager")

BiocManager::install("phyloseq")
