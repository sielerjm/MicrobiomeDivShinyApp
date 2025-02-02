# Microbiome Diversity Shiny App

Interactive visualization and statistical analysis of microbiome diversity metrics using R Shiny.

## Features
- Phyloseq object import
- Alpha/Beta diversity calculations
- Interactive visualizations
- Statistical testing (LM/PERMANOVA)
- Metadata-based plot customization

## Installation
```r
# Install required packages
source("app/requirements.R")

# Run app locally
shiny::runApp("app/app.R")
```

## Usage
1. Upload a phyloseq object (.rds)
2. Select diversity metrics
3. Customize visualizations
4. Perform statistical analyses

[Demo](#) | [Documentation](docs/user_guide.md)
