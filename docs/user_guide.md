# Microbiome Diversity Shiny App User Guide 

## Table of Contents
1. [Installation](#installation)
2. [Launching the App](#launching-the-app)
3. [Data Preparation](#data-preparation)
4. [Step-by-Step Guide](#step-by-step-guide)
5. [Example Analyses](#example-analyses)
6. [Troubleshooting](#troubleshooting)
7. [FAQs](#faqs)

## Installation <a name="installation"></a>

### Requirements
- R (≥ 4.0.0)
- RStudio (recommended)

### Package Installation
Run in R console:
```r
# Install CRAN packages
install.packages(c("shiny", "tidyverse", "vegan", "DT", "cowplot"))

# Install Bioconductor packages
if (!require("BiocManager", quietly = TRUE))
    install.packages("BiocManager")
BiocManager::install("phyloseq")
```

## Launching the App <a name="launching-the-app"></a>
```r
# From GitHub repository
shiny::runGitHub("yourusername/microbiome-diversity-shiny", subdir = "app")
```

## Data Preparation <a name="data-preparation"></a>
1. Prepare your phyloseq object containing:
   - OTU table
   - Sample data
   - Taxonomy table (optional)
   - Phylogenetic tree (optional)
   
2. Save as RDS file:
```r
saveRDS(physeq_object, "my_physeq.rds")
```

## Step-by-Step Guide <a name="step-by-step-guide"></a>

### 1. Upload & Setup
- **Upload Phyloseq Object**: Use the sidebar to upload your .rds file
- **Select Metrics**:
  - Alpha Diversity: Choose ≥1 metric (Shannon/Simpson/Observed)
  - Beta Diversity: Choose ≥1 metric (Bray-Curtis/Canberra)

### 2. Figures Tab
#### Alpha Diversity
- **Color By**: Select metadata variable for boxplot colors
- **Shape By**: Optional shape aesthetic
- Visualization: Interactive boxplots with jittered points

#### Beta Diversity
- **Color By**: Select metadata variable for PCoA colors
- **Shape By**: Optional shape aesthetic
- Visualization: PCoA plots using selected metrics

### 3. Tables Tab
1. **Select Analysis Type**: Alpha or Beta diversity
2. **Choose Variables**:
   - Response: Selected diversity metric
   - Explanatory: Metadata variable
3. Click "Run Analysis" to perform:
   - Alpha: Linear model ANOVA
   - Beta: PERMANOVA (adonis2)

## Example Analyses <a name="example-analyses"></a>

### Example 1: Alpha Diversity Comparison
1. Upload example phyloseq object
2. Select: Shannon + Simpson metrics
3. Color by "TreatmentGroup"
4. In Tables tab:
   - Response: Shannon
   - Explanatory: TreatmentGroup
   
**Output**: ANOVA table showing treatment effects

### Example 2: Beta Diversity Analysis
1. Select Bray-Curtis metric
2. Color by "TimePoint"
3. In Tables tab:
   - Response: BrayCurtis
   - Explanatory: TimePoint
   
**Output**: PERMANOVA table with F-statistics

## Troubleshooting <a name="troubleshooting"></a>

| Issue                         | Solution                                      |
| ----------------------------- | --------------------------------------------- |
| Phyloseq object not loading   | Verify file is saved with `saveRDS()`         |
| Missing metrics in Tables tab | Select metrics first in sidebar               |
| Visualization errors          | Check metadata variables exist in sample_data |
| Package installation failures | Update R/Bioconductor versions                |

**Common Errors:**
```r
# Typical phyloseq structure requirements
physeq <- phyloseq(
  otu_table(OTU, taxa_are_rows = TRUE),
  sample_data(SAMPLE),
  tax_table(TAX)
)
```

## FAQs <a name="faqs"></a>

**Q: What data formats are supported?**  
A: Only phyloseq objects saved as .rds files

**Q: Can I use different alpha diversity metrics?**  
A: Currently supports Shannon, Simpson, and Observed richness

**Q: How to handle large datasets?**  
A: Consider pre-filtering low-abundance OTUs before import

**Q: Can I save my results?**  
A: Use RStudio's export options for plots/tables

**Q: Is phylogenetic beta diversity supported?**  
A: Not in current version - uses presence/abundance metrics only

---

**Need more help?**  
Open an issue on [GitHub repository](https://github.com/yourusername/microbiome-diversity-shiny)