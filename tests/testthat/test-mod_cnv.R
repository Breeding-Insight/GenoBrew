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
  
  df <- passport_df
  # Quality check: report any mismatches between passport and VCF sample names
  samples <- unique(hmm_CN$by_window$Sample)
  only_pass <- df$ID[!df$ID %in% samples]       # passport IDs absent from VCF
  if(length(only_pass) > 0)
    df <- df[-which(!df$ID %in% samples),]
  only_hmm <- samples[!samples %in% df$ID]     # VCF samples absent from passport
  
  # Reshape passport to long format so multi-family individuals are expanded
  df_long <- df %>%
    mutate(fam = as.character(fam)) %>%
    separate_rows(fam, sep = ",") %>%
    mutate(fam = as.integer(fam))
  
  # Build per-family sample lists and generation labels
  fam_list <- split(df_long$ID,          df_long$fam)
  rela      <- split(df_long$generation, df_long$fam)
  
  idx <- lapply(rela, function(x) which(x == "Parent"))
  
  parents <- vector()
  for(i in 1:length(fam_list)) {
    if(length(idx[[i]]) >0)
      parents[i] <- paste0(fam_list[[i]][idx[[i]]], collapse = " x ")
    else parents[i] <- "No parents"
  }
  
  fam_select <- as.list(as.numeric(names(fam_list)))
  names(fam_select) <- parents
  
  sel <- fam_list[[1]]
  relat <- rela[[1]]
  
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
