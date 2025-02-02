# Microbiome Diversity Shiny App User Guide 

## Table of Contents
1. [Installation](#installation)
2. [Launching the App](#launching-the-app)
3. [Data Preparation](#data-preparation)
4. [Step-by-Step Guide](#step-by-step-guide)
5. [Example Analyses](#example-analyses)
6. [Troubleshooting](#troubleshooting)
7. [FAQs](#faqs)
8. [AI Interpretation](#ai-interpretation)

## Installation <a name="installation"></a>

### Requirements
- R (≥ 4.0.0)
- RStudio (recommended)

### Package Installation
Run in R console:
```r
# Install CRAN packages
install.packages(c("shiny", "tidyverse", "vegan", "DT", "cowplot", "shinyjs", "httr", "jsonlite"))

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
  - Normalization: Option to show normalized values (0-1 scale)

### 2. Figures Tab
#### Alpha Diversity
- **Color By**: Select metadata variable for boxplot colors
- **Shape By**: Optional shape aesthetic
- **Facet By**: Split plots by one or more metadata variables
- **Advanced Options**:
  - Custom plot titles and axis labels
  - Control font sizes
  - Adjust legend position and appearance
  - Add plot captions
  - Set facet layout (rows/columns)
- Visualization: Interactive boxplots with jittered points
- **Download Options**:
  - PNG or PDF format
  - Customizable dimensions and DPI

### 3. Statistics Tab
1. **Select Analysis Type**: 
   - Normalized or raw values
   - Linear model or quasibinomial GLM
2. **Choose Variables**:
   - Response: Selected diversity metric
   - Explanatory: One or more metadata variables
   - Interaction terms: Test interactions between variables
3. Click "Run Analysis" to perform:
   - Model fitting and summary statistics
   - P-values with significance stars
4. **Download Results**:
   - CSV format with full statistical output

## AI Interpretation <a name="ai-interpretation"></a>
1. After running statistical analysis, click "Show AI Interpretation"
2. Enter your Hugging Face API token
3. (Optional) Provide experimental context for more relevant interpretation
4. Click "Interpret Results" to get:
   - Plain language explanation of statistical results
   - Key findings and their significance
   - Practical implications of the results
   - Context-specific insights (if context provided)

**Note**: API tokens are only used for the current session and are not stored.

## Example Analyses <a name="example-analyses"></a>

### Example 1: Normalized Alpha Diversity Comparison
1. Upload example phyloseq object
2. Select: Shannon + Simpson metrics
3. Enable normalization
4. Color by "TreatmentGroup"
5. In Statistics tab:
   - Response: Shannon (normalized)
   - Explanatory: TreatmentGroup
   
**Output**: Quasibinomial GLM results showing treatment effects

### Example 2: Interaction Analysis
1. Select Observed richness
2. Choose explanatory variables: Treatment and TimePoint
3. Add interaction term: Treatment × TimePoint
4. Run analysis
5. Get AI interpretation with experimental context

**Output**: Model results showing main effects and interaction, with AI-generated explanation

## Troubleshooting <a name="troubleshooting"></a>

| Issue                         | Solution                                      |
| ----------------------------- | --------------------------------------------- |
| Phyloseq object not loading   | Verify file is saved with `saveRDS()`         |
| Missing metrics in Tables tab | Select metrics first in sidebar               |
| Visualization errors          | Check metadata variables exist in sample_data |
| Package installation failures | Update R/Bioconductor versions                |
| AI interpretation not working | Verify API token is valid and active          |

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

**Q: Can I use normalized values for analysis?**  
A: Yes, both raw and normalized (0-1 scale) values are supported

**Q: How to handle large datasets?**  
A: Consider pre-filtering low-abundance OTUs before import

**Q: Can I save my results?**  
A: Use the download options for plots (PNG/PDF) and tables (CSV)

**Q: Is the AI interpretation secure?**  
A: Yes, API tokens are only used for the current session and are not stored

**Q: What model is used for AI interpretation?**  
A: The app uses Hugging Face's Mixtral-8x7B-Instruct-v0.1 model

---

**Need more help?**  
Open an issue on [GitHub repository](https://github.com/yourusername/microbiome-diversity-shiny)