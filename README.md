---
editor_options: 
  markdown: 
    wrap: 72
---

# GenoBrew

R shiny application template for BI. Using this template ensures that
the application is compatible with the Breedverse and follows internal
coding best practices. Adapt for your specific application. We highlight
the miniumum expected sections, but you are welcome to add additional
sections for your specific application

--

<!-- badges: start -->

{Add badges for your application, see img.shields.io for more examples}

[![R-CMD-check](https://github.com/Breeding-Insight/BIGapp/workflows/R-CMD-check/badge.svg)](https://github.com/Breeding-Insight/BIGapp/actions)
[![Development
Status](https://img.shields.io/badge/development-active-blue.svg)](https://img.shields.io/badge/development-active-blue.svg)
![GitHub
License](https://img.shields.io/github/license/Breeding-Insight/GenoBrew)

<!-- badges: end -->

::: {align="center"}
# GenoBrew

:::

{Short description of your application}

{EXAMPLE APPLICATION NAME} is a user-friendly web application built with
R and Shiny, designed to simplify the processing of low to mid-density
genotyping data for both diploid and polyploid species. It provides a
powerful and intuitive interface for researchers and breeders to analyze
genomic data without requiring command-line expertise.

## Key Features

{Summarize the application here}

-   **Web-Based Interface:** Access {EXAMPLE APPLICATION NAME} through
    your web browser, eliminating the need for using command-line inputs
    to perform genomic analysis.
-   **Genotype Processing:**
    -   Call genotypes from read counts.
    -   Filter SNPs based on various criteria.
    -   Filter samples to ensure data quality.

## User Interface

<p align="center">

<img src="https://github.com/user-attachments/assets/9a6984df-8116-403c-85c1-ba9600623940" alt="BIGapp Screenshot" width="800"/>
<br> <em>{EXAMPLE APPLICATION NAME}'s intuitive interface makes genomic
data analysis accessible to everyone.</em>

</p>

## Getting Started

### Tutorials

New to {EXAMPLE APPLICATION NAME}? Check out our comprehensive tutorial
to guide you through the process: [BIGapp
Tutorials](https://scribehow.com/page/BIGapp_Tutorials__FdLsY9ZxQsi6kgT9p-U2Zg)

### Online Preview (Optional)

Try out a live demo of {EXAMPLE APPLICATION NAME} here: [BIGapp
Demo](https://big-demo.shinyapps.io/bigapp-main/)

### Local Installation

1.  **Install R:** Download and install the latest version of R from
    [CRAN](https://cran.r-project.org/).

2.  **Open Terminal (macOS/Linux) or R Console (Windows).**

3.  **Installation:** \`\`\`R if (!require("BiocManager", quietly =
    TRUE)) install.packages("BiocManager") install.packages("remotes")

    BiocManager::install("Breeding-Insight/{EXAMPLE APPLICATION NAME}",
    dependencies = TRUE) \`\`\`

4.  **Starting {EXAMPLE APPLICATION NAME}:**
    `R     {EXAMPLE APPLICATION NAME}::run_app()`

5.  **Access in Browser:** The {EXAMPLE APPLICATION NAME} interface will
    open in your default web browser.

## Dependencies

{EXAMPLE APPLICATION NAME} leverages a powerful suite of R packages:

### Core R Packages

-   **R (\>= 4.4.0)**

### Shiny Framework

-   [shiny](https://cran.r-project.org/web/packages/shiny/index.html):
    Web application framework.
-   [shinyWidgets](https://cran.r-project.org/web/packages/shinyWidgets/index.html):
    Custom input widgets.
-   [shinyalert](https://cran.r-project.org/web/packages/shinyalert/index.html):
    Create elegant pop-up messages.
-   [shinyjs](https://cran.r-project.org/web/packages/shinyjs/index.html):
    Enhance Shiny apps with JavaScript actions.
-   [shinydisconnect](https://cran.r-project.org/web/packages/shinydisconnect/index.html):
    Handle user disconnections gracefully.
-   [shinycssloaders](https://cran.r-project.org/web/packages/shinycssloaders/index.html):
    Add CSS loaders for visual feedback.
-   [bs4Dash](https://cran.r-project.org/web/packages/bs4Dash/index.html):
    Bootstrap 4 dashboard components.
-   [DT](https://cran.r-project.org/web/packages/DT/index.html): Display
    data tables with interactive features.
-   [config](https://cran.r-project.org/web/packages/config/index.html):
    Manage environment-specific configurations.

### Genetic Analysis [Example category, add R package categories and package links to section]

-   [BIGr](https://github.com/Breeding-Insight/BIGr): Breeding Insight's
    core genomic analysis functions.

## Funding

{EXAMPLE APPLICATION NAME} development is supported by [Breeding
Insight](https://www.breedinginsight.org/), a USDA-funded initiative
based at University of Florida - IFAS.

## Citation

If you use [EXAMPLE APPLICATION NAME] in your research, please cite:

<paste citation information here>
