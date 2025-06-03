# Data Analysis Toolkit

The **Data Analysis Toolkit** is a Shiny-based R application designed to provide an interactive and user-friendly platform for performing statistical, omics, and protein sequence analyses. The application is divided into three main modules: **Statistical Data Analysis**, **Omics Data Analysis**, and **Protein BLAST Toolkit**. 

You can access it remotely on shiny server via link: <u>https://proma.shinyapps.io/toolkit_project/ </u>

(only 24h of active usage per month divided between users)

This README provides an overview of the toolkit's features, installation instructions, usage guide, and details on user capabilities for each module.

## Table of Contents

- Features
- Installation
- Usage
- Modules
  - Statistical Data Analysis
  - Omics Data Analysis
  - Protein BLAST Toolkit
- Contributing
- License

## Features

- **Interactive Dashboards**: Built with `shinydashboard`, the toolkit offers a responsive interface with sidebar navigation for easy access to different analysis modules.
- **Data Import Flexibility**: Supports multiple file formats (CSV, TSV, Excel) and built-in datasets for omics analysis.
- **Visualization Tools**: Includes dynamic plots (boxplots, histograms, PCA, density plots, MA plots, scatterplots) powered by `ggplot2` and `plotly`.
- **Statistical Analysis**: Performs normality tests, variance tests, t-tests, ANOVA, and non-parametric tests with bootstrap and Monte Carlo methods.
- **Omics Analysis**: Provides quality control plots, PCA, and gene expression barplots for transcriptomics/proteomics data.
- **Protein BLAST**: Integrates with UniProt and EBI BLAST APIs for protein sequence search and alignment.
- **Customizable Outputs**: Allows users to download data, results, and plots in various formats (CSV, TXT, PNG).
- **Debugging Support**: Includes debug outputs for troubleshooting data issues in the omics module.

## Installation ( for local usage )

### Prerequisites

- **R** (version 4.0 or higher)
- **RStudio** (recommended for development)
- Required R packages:

  ```R
  install.packages(c(
    "shiny", "shinydashboard", "rsconnect", "httr", "jsonlite", "DT",
    "ggplot2", "colourpicker", "shinyBS", "shinyjs", "shinyWidgets",
    "shinythemes", "plotly", "pheatmap", "reshape2", "tidyverse",
    "readxl", "car", "shiny.i18n", "viridis"
  ))
  ```

### Setup

1. Clone the repository:

   ```bash
   git clone https://github.com/0Proma0/Assignments-for-Toolkit.git
   cd data-analysis-toolkit
   ```
2. Ensure the project directory structure is as follows:

   ```
   data-analysis-toolkit/
   ├── R/
   │   ├── blast_server.R
   │   ├── blast_ui.R
   │   ├── omics_server.R
   │   ├── omics_ui.R
   │   ├── stat_analysis_server.R
   │   ├── stat_analysis_ui.R
   ├── data/
   │   ├── TPMs_table_100genes.csv
   ├── app.R
   ```
3. Place the `TPMs_table_100genes.csv` file in the `data/` directory or create a sample dataset:

   ```R
   dir.create("data", showWarnings = FALSE)
   sample_data <- data.frame(
     row.names = c("Gene1", "Gene2", "Gene3"),
     Sample1 = c(5.2, 3.4, 7.8),
     Sample2 = c(6.1, 2.9, 8.2),
     Sample3 = c(4.8, 3.7, 7.5)
   )
   write.csv(sample_data, "data/TPMs_table_100genes.csv")
   ```
4. Run the application:

   ```R
   setwd("path/to/data-analysis-toolkit")
   shiny::runApp()
   ```

### 

## Usage

1. Launch the app in RStudio or a browser via `shiny::runApp()`.
2. Navigate through the sidebar menu to select a module: **Statistical Analysis**, **Omics Analysis**, or **BLAST & Hits**.
3. Follow the module-specific instructions below to interact with the tools.

## Modules

### Statistical Data Analysis

The **Statistical Data Analysis** module provides tools for importing, transforming, visualizing, and analyzing tabular data, with a focus on statistical testing and data exploration.

#### User Abilities

- **Data Import**:
  - Upload files in CSV, TSV, TXT, or Excel formats.
  - Specify file type, separator, quote character, and header presence.
  - Generate synthetic data with normal or non-normal distributions, customizable by number of points, groups, and variance.
- **Data Transformation**:
  - Pivot data to long format by selecting columns to pivot, specifying names and values columns.
  - Filter columns to include only selected variables.
- **Statistical Analysis**:
  - Perform bootstrap and Monte Carlo analyses with estimators (mean, trimmed mean, median).
  - Run distribution analysis with automatic test selection (Student’s t-test, Welch’s t-test, Wilcoxon rank-sum, ANOVA, Welch’s ANOVA, Kruskal-Wallis) based on normality (Shapiro-Wilk) and variance (Levene’s test) tests.
  - Set significance level (α) for hypothesis testing.
- **Visualizations**:
  - Create boxplots and histograms, with options to color by group or use custom fill/border colors.
  - View bootstrap distribution plots for selected estimators.
- **Data Exploration**:
  - View raw, filtered, and transformed data in interactive DataTables.
  - Download statistical summaries as TXT or CSV files.
- **Feedback**:
  - Submit feedback (up to 200 words) via a Formspree endpoint.
- **Reset**:
  - Reset data to clear transformations and filters.

#### Example Workflow

1. Upload a CSV file or generate synthetic data with 2 groups and 50 points each.
2. Filter columns to focus on numeric variables.
3. Transform data to long format for grouped analysis.
4. Select a column for analysis, set α = 0.05, and run distribution analysis to perform a t-test.
5. Visualize results with a boxplot colored by group.
6. Download the statistical summary as a CSV.

### Omics Data Analysis

The **Omics Data Analysis** module is designed for analyzing transcriptomics or proteomics data, offering interactive plots and tables for quality control and gene expression analysis.

#### User Abilities

- **Data Import**:
  - Load the built-in `TPMs_table_100genes.csv` dataset or upload a custom CSV/TSV file (up to 100 MB).
  - Files must have gene/protein names as row names and numeric expression values in columns.
- **Data Filtering**:
  - Select numeric columns for analysis via a dynamic dropdown.
  - Filter genes by minimum sum of expression using a slider.
- **Visualizations**:
  - **Data Table**: View filtered data in an interactive DataTable with color-coded cells (blue for low, white for medium, red for high expression).
  - **QC Plot (Advanced)**:
    - **Density Plot**: Visualize expression distribution per sample to detect outliers or batch effects.
    - **Histogram**: Show overall expression distribution across all samples.
    - **MA Plot**: Compare two samples by plotting log2 fold change (M) vs. mean expression (A), with a red dashed line at M=0.
  - **PCA Plot**: Perform Principal Component Analysis (PCA) to visualize sample clustering in 2D (PC1 vs. PC2), triggered by a "Run PCA" button.
  - **Barplot**: Display expression values for a selected gene/protein across samples (currently under debugging for dropdown issues).
- **Data Exploration**:
  - View summary statistics of filtered data.
  - Access debug output in the sidebar to diagnose data issues (e.g., row counts, selected columns, gene choices).
- **Export**:
  - Download filtered data as a CSV file.

#### Notes

- Blue and red colors in plots represent low and high expression values, respectively, aiding in pattern identification.
- The barplot feature is currently bugged; users may see an empty gene dropdown or no plot. Debugging is ongoing (see issue #1).

#### Example Workflow

1. Select `TPMs_table_100genes` as the data source.
2. Choose numeric columns and set a minimum expression filter (e.g., 10).
3. View the filtered data table with color-coded expression values.
4. Generate a density plot to check sample distributions.
5. Run PCA to visualize sample clustering.
6. (If fixed) Select a gene to view its expression barplot.
7. Download the filtered dataset.

### Protein BLAST Toolkit

The **Protein BLAST Toolkit** module enables users to search for protein sequences, retrieve FASTA files, run BLASTp searches, and analyze results through tables and plots.

#### User Abilities

- **Protein Search and FASTA Retrieval**:
  - Search UniProt for proteins by name and organism (e.g., "p53" in "Homo sapiens").
  - Select a protein from search results to retrieve its FASTA sequence.
  - Upload a custom FASTA file instead of searching UniProt.
  - View and download the FASTA sequence.
- **BLAST Analysis**:
  - Run BLASTp against the UniProtKB/Swiss-Prot database using the retrieved or uploaded FASTA sequence.
  - Monitor BLAST job status with a progress bar and status messages.
- **Results Table**:
  - View BLAST results in an interactive DataTable with columns like Hit ID, Description, E-value, Identity, and more.
  - Filter results by:
    - Sequence length range.
    - Identity percentage (0–100%).
    - Log10 E-value range.
  - Select which columns to display.
  - Download results as a CSV file.
- **Visualizations**:
  - Create plots based on BLAST results:
    - **Histogram**: Show distribution of a numeric variable (e.g., E-value, Identity).
    - **Scatterplot**: Plot two numeric variables (e.g., E-value vs. Bit Score).
    - **Boxplot**: Display distribution of a numeric variable.
  - Customize plots with:
    - Fill and border colors via color pickers.
    - Border thickness and point/box size sliders.
    - Number of bins for histograms.
  - Download plots as PNG files.
- **Error Handling**:
  - Receive notifications for invalid inputs (e.g., non-FASTA files, no search results).
  - View status messages for BLAST job progress or failures.

#### Example Workflow

1. Search for "p53" in "Homo sapiens" and select a protein from the results.
2. Load the FASTA sequence or upload a custom FASTA file.
3. Run BLASTp and wait for results (up to 90 seconds).
4. Filter the results table by E-value &lt; 1e-10 and Identity &gt; 50% and select Bit Score in filters.
5. Create a scatterplot of E-value vs. Bit Score with custom colors.
6. Download the filtered results and plot.

## Contributing

Contributions are welcome! To contribute:

1. Fork the repository.
2. Create a feature branch (`git checkout -b feature/new-feature`).
3. Commit changes (`git commit -m "Add new feature"`).
4. Push to the branch (`git push origin feature/new-feature`).
5. Open a Pull Request.

Please report bugs or suggest features via GitHub Issues or via Statistical Analysis feedback form.

## License and Citations

This project is licensed under the MIT License. See the LICENSE file for details.
This project relies on the following R packages (versions as of October 18, 2023):

- `shiny (2.0.1)`: Web application framework for R. Author: RStudio Team. [CRAN](https://cran.r-project.org/web/packages/shiny/index.html), [GitHub](https://github.com/rstudio/shiny)
- `shinyBS (0.61.1)`: Bootstrap components for Shiny. Author: Eric Bailey. [CRAN](https://cran.r-project.org/web/packages/shinyBS/index.html), [GitHub](https://github.com/ebailey78/shinyBS)
- `shinyjs (2.1.0)`: JavaScript functions for Shiny. Author: Dean Attali. [CRAN](https://cran.r-project.org/web/packages/shinyjs/index.html), [GitHub](https://github.com/daattali/shinyjs)
- `shinyWidgets (0.8.7)`: Enhanced widgets for Shiny. Authors: Victor Perrier et al. [CRAN](https://cran.r-project.org/web/packages/shinyWidgets/index.html), [GitHub](https://github.com/dreamRs/shinyWidgets)
- `shinythemes (1.2.0)`: Themes for Shiny. Author: Winston Chang. [CRAN](https://cran.r-project.org/web/packages/shinythemes/index.html), [GitHub](https://github.com/rstudio/shinythemes)
- `shinydashboard (0.7.2)`: Dashboard interface for Shiny. Authors: Winston Chang et al. [CRAN](https://cran.r-project.org/web/packages/shinydashboard/index.html), [GitHub](https://github.com/rstudio/shinydashboard)
- `rsconnect (1.3.1)`: Deployment to Shinyapps.io. Author: RStudio Team. [CRAN](https://cran.r-project.org/web/packages/rsconnect/index.html), [GitHub](https://github.com/rstudio/rsconnect)
- `httr (1.4.7)`: HTTP interface for R. Author: Hadley Wickham. [CRAN](https://cran.r-project.org/web/packages/httr/index.html), [GitHub](https://github.com/r-lib/httr)
- `jsonlite (1.8.8)`: JSON parsing in R. Authors: Jeroen Ooms et al. [CRAN](https://cran.r-project.org/web/packages/jsonlite/index.html), [GitHub](https://github.com/jeroenooms/jsonlite)
- `DT (0.33)`: Interactive tables in R. Authors: Yihui Xie et al. [CRAN](https://cran.r-project.org/web/packages/DT/index.html), [GitHub](https://github.com/rstudio/DT)
- `ggplot2 (3.5.1)`: Data visualizations. Authors: Hadley Wickham et al. [CRAN](https://cran.r-project.org/web/packages/ggplot2/index.html), [GitHub](https://github.com/tidyverse/ggplot2)
- `colourpicker (1.3.0)`: Color picker for Shiny. Author: Dean Attali. [CRAN](https://cran.r-project.org/web/packages/colourpicker/index.html), [GitHub](https://github.com/daattali/colourpicker)
- `plotly (4.10.4)`: Interactive plots. Authors: Carson Sievert et al. [CRAN](https://cran.r-project.org/web/packages/plotly/index.html), [GitHub](https://github.com/plotly/plotly.R)
- `pheatmap (1.0.12)`: Heatmaps in R. Author: Raivo Kolde. [CRAN](https://cran.r-project.org/web/packages/pheatmap/index.html)
- `reshape2 (1.4.4)`: Data reshaping. Author: Hadley Wickham. [CRAN](https://cran.r-project.org/web/packages/reshape2/index.html), [GitHub](https://github.com/hadley/reshape)
- `tidyverse (2.0.0)`: Suite of data analysis packages. Authors: Hadley Wickham et al. [CRAN](https://cran.r-project.org/web/packages/tidyverse/index.html), [GitHub](https://github.com/tidyverse/tidyverse)
- `readxl (1.4.3)`: Excel file reading. Authors: Hadley Wickham et al. [CRAN](https://cran.r-project.org/web/packages/readxl/index.html), [GitHub](https://github.com/tidyverse/readxl)
- `car (3.1-2)`: Regression analysis. Authors: John Fox et al. [CRAN](https://cran.r-project.org/web/packages/car/index.html)
- `shiny.i18n (0.3.0)`: Internationalization for Shiny. Author: Appsilon. [CRAN](https://cran.r-project.org/web/packages/shiny.i18n/index.html), [GitHub](https://github.com/Appsilon/shiny.i18n)
- `viridis (0.6.5)`: Color scales for visualizations. Author: Simon Garnier. [CRAN](https://cran.r-project.org/web/packages/viridis/index.html), [GitHub](https://github.com/sjmgarnier/viridis)
- BLAST: Altschul et al. (1990). [Website](https://blast.ncbi.nlm.nih.gov/)
- UniProt: The UniProt Consortium (2021). [Website](https://www.uniprot.org/)
- Hosted on [Shinyapps.io](https://www.shinyapps.io/).
- Feedback collected via [Formspree.io](https://formspree.io/).
---

*Developers:<br> 
Makar Dorohuntsev (Statistics module, modules integration and debugging)<br>
Aleksandra Janik (Omics module)<br>
Weronika Lepiarz (Blast module)<br>*
