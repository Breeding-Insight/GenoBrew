# Tests for mod_mk_select server functions
#
# To run: devtools::test() or testthat::test_file("tests/testthat/test-mod_mk_select.R")

# ---------------------------------------------------------------------------
# Helpers / fixtures
# ---------------------------------------------------------------------------

panel_path <- system.file("Coffee_40k_MolBr_SNP.csv", package = "GenoBrew")
vcf_path   <- system.file("all_samples_merged_panel_intersect.vcf.gz", package = "GenoBrew")
bed_path   <- system.file("softmasked.sorted.tsv.gz", package = "GenoBrew")
win_path   <- system.file("vcf9_hmm_CN_estimation_by_window.csv", package = "GenoBrew")
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
  skip_if_not(file.exists(panel_path), "Example panel CSV not found")
  skip_if_not(file.exists(vcf_path),   "Example VCF not found")
  
  panel  <- read.csv(panel_path)
  result <- load_vcf_panel(panel_df = panel, vcf_path = vcf_path)
  
  expect_type(result, "list")
  expect_named(result, c("vcf", "n_wgs", "n_panel", "n_common", "panel_common"))
})

test_that("load_vcf_panel counts are correct", {
  skip_if_not(file.exists(panel_path), "Example panel CSV not found")
  skip_if_not(file.exists(vcf_path),   "Example VCF not found")
  
  panel  <- read.csv(panel_path)
  result <- load_vcf_panel(panel_df = panel, vcf_path = vcf_path)
  
  expect_equal(result$n_panel, nrow(panel))
  expect_lte(result$n_common, result$n_wgs)
  expect_lte(result$n_common, result$n_panel)
  expect_equal(result$n_common, nrow(result$vcf@fix))
  expect_equal(result$n_common, nrow(result$panel_common))
})

test_that("load_vcf_panel returns a vcfR object", {
  skip_if_not(file.exists(panel_path), "Example panel CSV not found")
  skip_if_not(file.exists(vcf_path),   "Example VCF not found")
  
  panel  <- read.csv(panel_path)
  result <- load_vcf_panel(panel_df = panel, vcf_path = vcf_path)
  
  expect_s4_class(result$vcf, "vcfR")
})

test_that("load_vcf_panel errors on missing SNP.ID column", {
  bad_panel <- data.frame(marker = c("chr1c_100", "chr1c_200"))
  expect_error(load_vcf_panel(bad_panel, vcf_path), "SNP.ID")
})

test_that("load_vcf_panel errors on non-existent VCF path", {
  panel <- data.frame(SNP.ID = "chr1c_100")
  expect_error(load_vcf_panel(panel, "/nonexistent/path.vcf"), "not found")
})

# ---------------------------------------------------------------------------
# get_stats_df()
# ---------------------------------------------------------------------------

test_that("plot_marker_positions returns a ggplot object", {
  skip_if_not(file.exists(vcf_path),   "Example VCF not found")
  
  cnv_data <- as.data.frame(
    data.table::fread(win_path, showProgress = FALSE)
  )
  
  bed_data <- data.table::fread(
    bed_path,
    header = FALSE, sep = "\t", select = 1:3,
    col.names  = c("chrom", "start", "end"),
    colClasses = c("character", "integer", "integer"),
    showProgress = FALSE
  )
  
  panel  <- read.csv(panel_path)
  
  results <- load_vcf_panel(panel_df = panel,vcf_path = vcf_path)
  
  result <- get_stats_df(vcf = results$vcf, bed_data, cnv_data, dist_ploidy = 2, filter_samples = NULL )
  
  p <- plot_marker_positions(marker_stats = result, colour_by = "missing")
  p <- plot_marker_positions(marker_stats = result, colour_by = "depth")
  p <- plot_marker_positions(marker_stats = result, colour_by = "heterozygosity")
  p <- plot_marker_positions(marker_stats = result, colour_by = "MAF")
  p <- plot_marker_positions(marker_stats = result, colour_by = "repeated")
  p <- plot_marker_positions(marker_stats = result, colour_by = "CNV")
  
  p <- plot_marker_positions(marker_stats = result, colour_by = "missing", interactive = TRUE)
  expect_s3_class(p, "gg")
  
  filtered <- stats_filter(vcf, stats_df = result)
  
  filtered <- stats_filter(vcf, stats_df = result, filter_samples = colnames(vcf@gt)[-1],
                           filter_maf = 0.05, filter_missing = 25, 
                           filter_het = c(5,100), filter_depth = c(10,200), 
                           filter_repeated = TRUE, filter_cnv = 100)
  
  p <- plot_marker_positions(marker_stats = filtered$stats_df_filt, colour_by = "heterozygosity")
  
  
})


