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
includes built-in datasets for *Coffea arabica* genomic data exploration
and analysis, designed to support breeders and researchers without
requiring command-line expertise.

### Key Features

- **Web-Based Interface:** Run analyses directly in the browser — no
  command-line required.
- **Select Markers module:** Compare marker statistics derived from
  whole-genome sequencing (WGS) of 91 *C. arabica* samples + KO34
  against the **40k MolBreeding Marker Panel**. Interactively filter
  marker subsets based on MAF, missing data, heterozygosity, and CNV
  thresholds.
- **CNV Profiles module:** Explore interactive visualizations of copy
  number variation profiles across 91 samples + KO34, with per-sample
  and per-family filtering. Also visualize relationship statistics for families.
- **Built-in datasets:** Three WGS *C. arabica* datasets ready to load
  (Hawaii + KO34, Brazil + KO34, combined WGS + KO34).
- **40k MolBreeding Panel:** Built-in marker panel based on the
  *C. arabica* Red Bourbon reference genome
  ([Scalabrin et al., 2024](https://doi.org/10.1038/s41588-024-01695-w)).
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

1. **Install R** (≥ 3.6.0) from [CRAN](https://cran.r-project.org/).

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
