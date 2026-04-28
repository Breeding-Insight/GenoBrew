# Tests for mod_cnv server functions
#
# To run: devtools::test() or testthat::test_file("tests/testthat/test-mod_cnv.R")

# ---------------------------------------------------------------------------
# Helpers / fixtures
# ---------------------------------------------------------------------------

marker_path   <- "https://github.com/Breeding-Insight/BIGapp-PanelHub/raw/refs/heads/long_seq/alfalfa/GenoBrew_example/alfalfa_f1_hmm_CN_estimation_by_marker.csv.gz"
window_path   <- "https://github.com/Breeding-Insight/BIGapp-PanelHub/raw/refs/heads/long_seq/alfalfa/GenoBrew_example/alfalfa_f1_hmm_CN_estimation_by_window.csv.gz"
params_path <- "https://github.com/Breeding-Insight/BIGapp-PanelHub/raw/refs/heads/long_seq/alfalfa/GenoBrew_example/alfalfa_f1_hmm_CN_estimation_params.rds"
passport_path <- "https://github.com/Breeding-Insight/BIGapp-PanelHub/raw/refs/heads/long_seq/alfalfa/GenoBrew_example/alfalfa_F1_passport.csv"

library(vcfR)
# ---------------------------------------------------------------------------
# UI smoke test
# ---------------------------------------------------------------------------

test_that("mod_cnv_ui renders without error", {
  expect_no_error(mod_cnv_ui("test"))
})

# ---------------------------------------------------------------------------
# Server: data loading
# ---------------------------------------------------------------------------

test_that("loading a built-in dataset populates cnv_items", {
  
  hmm_CN <- read_hmm_CN(by_window_file = window_path,
                        by_marker_file = marker_path, 
                        params_file = params_path)
  
  passport_df <- read.csv(passport_path)
  
  passport_prep <- prepare_passport(passport_df, hmm_CN$by_window)

  sel <- passport_prep$fam_list[[1]]
  relat <- passport_prep$rela[[1]]
  
  p <- compare_cn_track(hmm_CN, samples_to_plot = sel)
  
  plot <- plot_cn_track(hmm_CN, sample_id = sel[1])
  
})

# test_that("loading a user CNV file populates cnv_items", {
#
# })

# ---------------------------------------------------------------------------
# Server: sample / family filtering
# ---------------------------------------------------------------------------

# test_that("filtering by sample restricts data to selected samples", {
#
# })

# test_that("filtering by family includes all samples in selected families", {
#
# })

# test_that("family picker populates sample preview output", {
#
# })

# ---------------------------------------------------------------------------
# Server: CNV plots
# ---------------------------------------------------------------------------

# test_that("CNV profile plot renders after data load", {
#
# })

# test_that("dosage distribution plot renders after data load", {
#
# })

# test_that("heterozygosity histogram renders after data load", {
#
# })

# test_that("marker position plot renders after data load", {
#
# })

# ---------------------------------------------------------------------------
# Server: value boxes / summary
# ---------------------------------------------------------------------------

# test_that("summary info is populated after data load", {
#
# })

# ---------------------------------------------------------------------------
# Server: download handlers
# ---------------------------------------------------------------------------

# test_that("download handler produces a non-empty zip file", {
#
# })
