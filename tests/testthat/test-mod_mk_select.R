# Tests for mod_mk_select server functions
#
# To run: devtools::test() or testthat::test_file("tests/testthat/test-mod_mk_select.R")

# ---------------------------------------------------------------------------
# Helpers / fixtures
# ---------------------------------------------------------------------------

vcf_path  = "https://github.com/Breeding-Insight/BIGapp-PanelHub/raw/refs/heads/long_seq/alfalfa/GenoBrew_example/alfalfa_F1_marker_panel_dataset_publicly_available.vcf.gz"
panel_path   <- "https://github.com/Breeding-Insight/BIGapp-PanelHub/raw/refs/heads/long_seq/alfalfa/20201030-BI-Alfalfa_SNPs_DArTag-probe-design_snpID_lut.csv"
win_path = "https://github.com/Breeding-Insight/BIGapp-PanelHub/raw/refs/heads/long_seq/alfalfa/GenoBrew_example/alfalfa_f1_hmm_CN_estimation_by_window.csv.gz"


library(vcfR)
library(data.table)
# ---------------------------------------------------------------------------
# UI smoke test
# ---------------------------------------------------------------------------

test_that("mod_mk_select_ui renders without error", {
  expect_no_error(mod_mk_select_ui("test"))
})

# ---------------------------------------------------------------------------
# load_vcf_panel()
# ---------------------------------------------------------------------------

test_that("load_vcf_panel returns a list with the expected elements", {
    panel  <- read.csv(panel_path)
    vcf <- read.vcfR(vcf_path)
    cnv_data <- as.data.frame(
      fread(win_path, showProgress = FALSE)
    )
    dist_ploidy <- 2
    
    vcf_input <- vcf
    result <- load_vcf_panel(panel_df = panel, vcf = vcf)
    
    vcf_common <- result$vcf
    
    marker_stats <- GenoBrew:::get_stats_df(vcf = vcf_input, 
                                                 bed_data = NULL, 
                                                 win_data = cnv_data, 
                                                 dist_ploidy = as.numeric(dist_ploidy), 
                                                 filter_samples = colnames(vcf_input@gt)[-1])
    
    id_all <- paste0(marker_stats$CHR, "_", marker_stats$Position)
    id_common <- paste0(vcf_common@fix[,1], "_", vcf_common@fix[,2])
    
    idx <- which(id_all %in% id_common)
    
    common_marker_stats <- marker_stats[idx,]
    
  expect_type(result, "list")
  expect_named(result, c("vcf", "n_wgs", "n_panel", "n_common", "panel_common"))
  expect_equal(result$n_panel, nrow(panel))
  expect_lte(result$n_common, result$n_wgs)
  expect_lte(result$n_common, result$n_panel)
  expect_equal(result$n_common, nrow(result$vcf@fix))
  expect_equal(result$n_common, nrow(result$panel_common))
  expect_s4_class(result$vcf, "vcfR")
  
  bad_panel <- data.frame(marker = c("chr1c_100", "chr1c_200"))
  expect_error(load_vcf_panel(bad_panel, vcf), regexp = "`panel_df` must contain a column named 'Chr', 'Chromosome', or 'CHROM'.")
  
})

# ---------------------------------------------------------------------------
# get_stats_df()
# ---------------------------------------------------------------------------

test_that("plot_marker_positions returns a ggplot object", {

  cnv_data <- as.data.frame(
    data.table::fread(win_path, showProgress = FALSE)
  )
  
  # bed_data <- data.table::fread(
  #   bed_path,
  #   header = FALSE, sep = "\t", select = 1:3,
  #   col.names  = c("chrom", "start", "end"),
  #   colClasses = c("character", "integer", "integer"),
  #   showProgress = FALSE
  # )
  
  panel  <- read.csv(panel_path)
  
  vcf <- read.vcfR(vcf_path)
  result <- load_vcf_panel(panel_df = panel, vcf = vcf)
  
  marker_stats <- get_stats_df(vcf = vcf,bed_data = NULL, 
                         win_data = cnv_data, dist_ploidy = 4, 
                         filter_samples = NULL )
  
  vcf_common <- result$vcf
  
  idx <- which(marker_stats$CHR %in% vcf_common@fix[,1] & marker_stats$Position %in% vcf_common@fix[,2])
  
  common_marker_stats <- marker_stats[idx,]
  
  p <- plot_marker_positions(marker_stats = common_marker_stats, colour_by = "missing")
  p <- plot_marker_positions(marker_stats = common_marker_stats, colour_by = "depth")
  p <- plot_marker_positions(marker_stats = common_marker_stats, colour_by = "heterozygosity")
  p <- plot_marker_positions(marker_stats = common_marker_stats, colour_by = "MAF")
  p <- plot_marker_positions(marker_stats = common_marker_stats, colour_by = "CNV", ploidy = 4)
  
  expect_s3_class(p, "gg")
  
  p <- plot_marker_positions(marker_stats = common_marker_stats, colour_by = "missing", interactive = TRUE)
  
  filtered <- stats_filter(vcf, stats_df = common_marker_stats)
  
  filtered <- stats_filter(vcf, stats_df = common_marker_stats, filter_samples = colnames(vcf@gt)[-1],
                           filter_maf = 0.05, filter_missing = 25, 
                           filter_het = c(0,100), filter_depth = c(5,200), 
                           filter_repeated = FALSE, filter_cnv = 100)
  
  p <- plot_marker_positions(marker_stats = filtered$stats_df_filt, colour_by = "heterozygosity")
  
})


