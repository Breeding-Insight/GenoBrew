<!-- badges: start -->
[![R-CMD-check](https://github.com/Breeding-Insight/GenoBrew/workflows/R-CMD-check/badge.svg)](https://github.com/Breeding-Insight/GenoBrew/actions)
![GitHub Release](https://img.shields.io/github/v/release/Breeding-Insight/GenoBrew)
[![Development Status](https://img.shields.io/badge/development-active-blue.svg)](https://img.shields.io/badge/development-active-blue.svg)
![GitHub License](https://img.shields.io/github/license/Breeding-Insight/GenoBrew)
[![codecov](https://app.codecov.io/gh/Breeding-Insight/GenoBrew/graph/badge.svg?token=PJUZMRN1NF)](https://app.codecov.io/gh/Breeding-Insight/GenoBrew)

<!-- badges: end -->

# GenoBrew <img src="inst/app/www/GenoBrew_logo.png" align="right" width="250"/>

**GenoBrew** is a user-friendly R Shiny application for measuring marker
panel efficiency according to dataset, help users optmize markers selection, 
perform basic relationship analysis, and visualization of copy number variation (CNV) profiles. It
includes built-in datasets for *alfalfa* genomic data exploration
and analysis, designed to support breeders and researchers without
requiring command-line expertise.

### Key Features

- **Web-Based Interface:** Run analyses directly in the browser — no
  command-line required.
- **Select Markers module:** Compare marker statistics derived from
  whole-genome sequencing (WGS), GBS, or other sequencing technology with a set marker panel.
- **CNV Profiles module:** Explore interactive visualizations of copy
  number variation profiles across samples. Also visualize relationship statistics for families.
- **Built-in datasets:** Includes the Alfalfa F1 population dataset, ready to load for analysis. This dataset is publicly available and validated using the Alfalfa 3k DArTag marker panel.
- **Upload your own data:** Accepts VCF and CNV files in CSV/TSV/GZ
  formats alongside a custom marker panel CSV.

### Modules

#### Select Markers

Loads a VCF file (or built-in WGS dataset) and a marker panel, then
computes and displays:

- WGS marker count, panel marker count, common markers, and markers
  passing filters
- Interactive marker distribution plot
- Filters: minimum MAF, maximum missing data, depth range, heterozygosity range,
  maximum % CNV ≠ 2
- Genomic relationship plots


#### CNV Profiles

Loads a CNV file (or built-in dataset) and visualizes:

- Genome-wide CNV profiles for combined samples
- BAF, zscore and CNV calls plots for single sample
- Pairwise IBD plots

### Getting Started

#### Local Installation

1. **Install R** (≥ 4.1.0) from [CRAN](https://cran.r-project.org/).

2. **Install GenoBrew:**

```r
if (!require("remotes", quietly = TRUE)) install.packages("remotes")
remotes::install_github("Breeding-Insight/GenoBrew", dependencies = TRUE)
```

3. **Launch the app:**

```r
GenoBrew::run_app()
```

4. The GenoBrew interface will open in your default web browser.

### Funding

GenoBrew development is supported by
[Breeding Insight](https://www.breedinginsight.org/), a USDA-funded
initiative hosted at the University of Florida – IFAS.

## License

This project is licensed under the Apache-2.0 license. See
[LICENSE](LICENSE) for details.
