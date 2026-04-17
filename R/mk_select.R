#' Load and intersect a WGS VCF with a marker panel
#'
#' Reads a VCF file with \code{vcfR} and intersects its markers with those in a
#' marker-panel data frame. Marker identity is determined by matching the VCF
#' \code{ID} field (expected format \code{<CHROM>_<POS>}) against the
#' \code{SNP.ID} column of the panel data frame.
#'
#' @param panel_df A \code{data.frame} read from the marker-panel CSV
#'   (e.g. \code{Coffee_40k_MolBr_SNP.csv}). Must contain a column named
#'   \code{SNP.ID} with marker identifiers in \code{<CHROM>_<POS>} format.
#' @param vcf_path Character. Path to the VCF (or VCF.gz) file to read.
#' @param verbose Logical. Whether to print progress messages (default
#'   \code{FALSE}).
#'
#' @return A named list with the following elements:
#' \describe{
#'   \item{\code{vcf}}{A \code{vcfR} object containing only the markers that
#'     are present in both the VCF and the panel (\emph{common markers}).}
#'   \item{\code{n_wgs}}{Integer. Total number of markers in the input VCF
#'     (before intersecting with the panel).}
#'   \item{\code{n_panel}}{Integer. Total number of markers in \code{panel_df}.}
#'   \item{\code{n_common}}{Integer. Number of markers found in both the VCF
#'     and the panel.}
#'   \item{\code{panel_common}}{A \code{data.frame}. Rows of \code{panel_df}
#'     whose \code{SNP.ID} is present in the VCF, preserving the original
#'     column order.}
#' }
#'
#' @importFrom vcfR read.vcfR
#' @export
#'
#' @examples
#' \dontrun{
#' panel <- read.csv(system.file("Coffee_40k_MolBr_SNP.csv", package = "GenoBrew"))
#' vcf_path <- system.file("all_samples_merged_panel_intersect.vcf.gz",
#'                          package = "GenoBrew")
#' result <- load_vcf_panel(panel_df = panel, vcf_path = vcf_path)
#'
#' result$n_wgs      # markers in VCF
#' result$n_panel    # markers in panel CSV
#' result$n_common   # markers in common
#' result$vcf        # vcfR object (common markers only)
#' }
load_vcf_panel <- function(panel_df, vcf_path, verbose = FALSE) {
  
  # ----- input checks --------------------------------------------------------
  if (!inherits(panel_df, "data.frame"))
    stop("`panel_df` must be a data.frame.")
  if (!"SNP.ID" %in% colnames(panel_df))
    stop("`panel_df` must contain a column named 'SNP.ID'.")
  if (!file.exists(vcf_path))
    stop("VCF file not found: ", vcf_path)
  
  # ----- read VCF ------------------------------------------------------------
  if (verbose) message("Reading VCF: ", vcf_path)
  vcf <- vcfR::read.vcfR(vcf_path, verbose = verbose)
  
  # ----- counts before intersection ------------------------------------------
  n_wgs   <- nrow(vcf@fix)
  n_panel <- nrow(panel_df)
  
  if (verbose) {
    message("  WGS markers  : ", n_wgs)
    message("  Panel markers: ", n_panel)
  }
  
  # ----- intersect by marker ID ----------------------------------------------
  vcf_ids   <- vcf@fix[, "ID"]
  panel_ids <- panel_df[["SNP.ID"]]
  
  common_ids <- intersect(vcf_ids, panel_ids)
  n_common   <- length(common_ids)
  
  if (verbose) message("  Common markers: ", n_common)
  
  # ----- subset VCF and panel to common markers ------------------------------
  vcf_common   <- vcf[vcf_ids %in% common_ids, ]
  panel_common <- panel_df[panel_ids %in% common_ids, ]
  
  # ----- return ---------------------------------------------------------------
  list(
    vcf          = vcf_common,
    n_wgs        = n_wgs,
    n_panel      = n_panel,
    n_common     = n_common,
    panel_common = panel_common
  )
}


##' Plot marker positions across the genome
##'
##' Creates a horizontal plot showing marker positions per chromosome. Each chromosome is drawn as a grey bar and each marker is a vertical tick. Ticks can optionally be coloured by a per-marker statistic derived from the marker statistics data frame.
##'
##' @param marker_stats A data.frame of per-marker statistics, as produced by get_stats_df(). Must contain columns CHR, Position, and MarkerID. Additional columns (e.g., MAF, MeanDP, Pct_Missing, Pct_Het, In_Repeats, Pct_CNV_diff_ploidy) enable additional colour_by options.
##' @param colour_by Character. One of NULL (flat blue), "missing", "depth", "heterozygosity", "MAF", "repeated", or "CNV". Only available if the corresponding column is present in marker_stats.
##' @param point_size Numeric. Line width of the marker tick (default 0.3).
##' @param bar_colour Character. Fill colour for chromosome bars (default "#d9d9d9").
##' @param marker_colour Character. Flat tick colour when colour_by = NULL (default "#2166ac").
##' @param bar_height Numeric. Height of each chromosome bar (default 0.5).
##' @param gradient_colours Character vector of colours for the gradient scale. Defaults to a blue-orange-red palette (c("#2166ac", "#fc8d59", "#b2182b")).
##' @param ploidy Integer. Reference ploidy for CNV legend (default 2).
##' @param interactive Logical. If TRUE, adds a tooltip column for interactive plotting (default FALSE).
##'
##' @return A ggplot object (or data.frame with tooltip column if interactive = TRUE).
##'
##' @importFrom ggplot2 ggplot
##' @importFrom ggplot2 aes
##' @importFrom ggplot2 geom_tile
##' @importFrom ggplot2 geom_segment
##' @importFrom ggplot2 scale_x_continuous
##' @importFrom ggplot2 scale_y_continuous
##' @importFrom ggplot2 scale_colour_gradient
##' @importFrom ggplot2 labs
##' @importFrom ggplot2 theme_minimal
##' @importFrom ggplot2 theme
##' @importFrom ggplot2 element_text
##' @importFrom ggplot2 element_blank
##' @importFrom ggplot2 expansion
##' @importFrom ggplot2 margin
##' @importFrom scales label_number
##' @export
##'
##' @examples
##' \dontrun{
##' stats <- get_stats_df(vcf, bed_data, win_data, dist_ploidy)
##' plot_marker_positions(stats)
##' plot_marker_positions(stats, colour_by = "missing")
##' plot_marker_positions(stats, colour_by = "MAF")
##' }
plot_marker_positions <- function(marker_stats,
                                  colour_by        = NULL,
                                  point_size       = 0.3,
                                  bar_colour       = "#d9d9d9",
                                  marker_colour    = "#2166ac",
                                  bar_height       = 0.5,
                                  gradient_colours = c("#2166ac", "#fc8d59", "#b2182b"),
                                  ploidy           = 2L,
                                  interactive      = FALSE) {

  # ----- input validation ----------------------------------------------------
  if (!is.data.frame(marker_stats))
    stop("`marker_stats` must be a data.frame.")

  required_cols <- c("CHR", "Position", "MarkerID")
  missing_cols  <- setdiff(required_cols, colnames(marker_stats))
  if (length(missing_cols) > 0)
    stop("marker_stats is missing required columns: ", paste(missing_cols, collapse = ", "))

  ploidy <- as.integer(ploidy)

  # ----- valid colour_by options ---------------------------------------------
  valid_colour_by <- c(
    if ("Pct_Missing"         %in% colnames(marker_stats)) "missing",
    if ("MeanDP"              %in% colnames(marker_stats)) "depth",
    if ("Pct_Het"             %in% colnames(marker_stats)) "heterozygosity",
    if ("MAF"                 %in% colnames(marker_stats)) "MAF",
    if ("In_Repeats"          %in% colnames(marker_stats)) "repeated",
    if ("Pct_CNV_diff_ploidy" %in% colnames(marker_stats)) "CNV"
  )

  if (!is.null(colour_by) && !colour_by %in% valid_colour_by)
    stop("`colour_by` must be NULL or one of: ", paste(valid_colour_by, collapse = ", "))

  # ----- metric column -------------------------------------------------------
  fix <- marker_stats
  fix$POS <- as.numeric(fix$Position)

  if (!is.null(colour_by)) {
    fix$metric <- switch(colour_by,
      missing        = fix$Pct_Missing,
      depth          = fix$MeanDP,
      heterozygosity = fix$Pct_Het,
      MAF            = fix$MAF,
      repeated       = ifelse(fix$In_Repeats, "repeated", "unique"),
      CNV            = fix$Pct_CNV_diff_ploidy
    )
  }

  # ----- tooltip -------------------------------------------------------------
  if (interactive) {
    fix$tooltip <- paste0(
      "Position: ",    fix$Position,
      "<br>MarkerID: ", fix$MarkerID,
      "<br>MAF: ",      if ("MAF"                 %in% colnames(fix)) round(fix$MAF, 4)                  else "N/A",
      "<br>Mean DP: ",  if ("MeanDP"              %in% colnames(fix)) round(fix$MeanDP, 2)               else "N/A",
      "<br>% Missing: ",if ("Pct_Missing"         %in% colnames(fix)) round(fix$Pct_Missing, 2)          else "N/A",
      "<br>% Het: ",    if ("Pct_Het"             %in% colnames(fix)) round(fix$Pct_Het, 2)              else "N/A",
      "<br>% CN diff: ",if ("Pct_CNV_diff_ploidy" %in% colnames(fix)) round(fix$Pct_CNV_diff_ploidy, 2) else "N/A",
      "<br>Repeat reg: ",if ("In_Repeats"         %in% colnames(fix)) ifelse(fix$In_Repeats, "Yes", "No") else "N/A"
    )
  }

  # ----- chromosome order ----------------------------------------------------
  chroms    <- unique(fix$CHR)
  num_part  <- as.integer(gsub("^chr([0-9]+)[ce]$", "\\1", chroms))
  sub_part  <- gsub("^chr[0-9]+([ce])$", "\\1", chroms)
  ord       <- order(num_part, sub_part)
  chrom_levels  <- chroms[ord]
  display_levels <- rev(chrom_levels)

  fix$CHROM <- factor(fix$CHR, levels = chrom_levels)
  fix$y_pos <- as.integer(factor(fix$CHROM, levels = display_levels))

  # ----- chromosome background bars ------------------------------------------
  chrom_max <- tapply(fix$POS, fix$CHROM, max, na.rm = TRUE)
  bar_df <- data.frame(
    CHROM   = names(chrom_max),
    max_pos = as.numeric(chrom_max),
    stringsAsFactors = FALSE
  )
  bar_df$CHROM <- factor(bar_df$CHROM, levels = chrom_levels)
  bar_df$y_pos <- as.integer(factor(bar_df$CHROM, levels = display_levels))

  # ----- legend label --------------------------------------------------------
  legend_label <- switch(colour_by %||% "",
    missing        = "Missing (%)",
    depth          = "Mean Depth",
    heterozygosity = "Heterozygosity",
    MAF            = "MAF",
    repeated       = "Region",
    CNV            = paste0("% Samples\nCN\u2260", ploidy),
    ""
  )

  # ----- plot ----------------------------------------------------------------
  p <- ggplot2::ggplot() +
    ggplot2::geom_tile(
      data = bar_df,
      ggplot2::aes(x = max_pos / 2, y = y_pos,
                   width = max_pos, height = bar_height),
      fill = bar_colour, colour = NA
    )

  seg_aes <- function(colour = FALSE) {
    if (interactive) {
      if (colour)
        ggplot2::aes(x = POS, xend = POS,
                     y    = y_pos - bar_height / 2,
                     yend = y_pos + bar_height / 2,
                     colour = metric, text = tooltip)
      else
        ggplot2::aes(x = POS, xend = POS,
                     y    = y_pos - bar_height / 2,
                     yend = y_pos + bar_height / 2,
                     text = tooltip)
    } else {
      if (colour)
        ggplot2::aes(x = POS, xend = POS,
                     y    = y_pos - bar_height / 2,
                     yend = y_pos + bar_height / 2,
                     colour = metric)
      else
        ggplot2::aes(x = POS, xend = POS,
                     y    = y_pos - bar_height / 2,
                     yend = y_pos + bar_height / 2)
    }
  }

  if (is.null(colour_by)) {
    p <- p + ggplot2::geom_segment(
      data      = fix,
      mapping   = seg_aes(colour = FALSE),
      linewidth = point_size,
      colour    = marker_colour
    )
  } else if (colour_by == "repeated") {
    p <- p +
      ggplot2::geom_segment(data = fix, mapping = seg_aes(colour = TRUE), linewidth = point_size) +
      ggplot2::scale_colour_manual(
        name   = legend_label,
        values = c(repeated = "#d73027", unique = "#2166ac"),
        labels = c(repeated = "Repeated", unique = "Unique")
      )
  } else {
    p <- p +
      ggplot2::geom_segment(data = fix, mapping = seg_aes(colour = TRUE), linewidth = point_size) +
      ggplot2::scale_colour_gradientn(
        name     = legend_label,
        colours  = gradient_colours,
        na.value = "grey60"
      )
  }

  p <- p +
    ggplot2::scale_x_continuous(
      labels = scales::label_number(suffix = " Mb", scale = 1e-6),
      expand = ggplot2::expansion(mult = c(0.01, 0.02))
    ) +
    ggplot2::scale_y_continuous(
      breaks = seq_along(display_levels),
      labels = display_levels,
      expand = ggplot2::expansion(add = 0.6)
    ) +
    ggplot2::labs(
      x     = "Genomic Position",
      y     = NULL,
      title = paste0("Marker Distribution (n = ", nrow(fix), ")")
    ) +
    ggplot2::theme_minimal(base_size = 12) +
    ggplot2::theme(
      panel.grid.major.y = ggplot2::element_blank(),
      panel.grid.minor   = ggplot2::element_blank(),
      axis.text.y        = ggplot2::element_text(size = 10),
      axis.text.x        = ggplot2::element_text(size = 9),
      plot.title         = ggplot2::element_text(face = "bold", size = 13),
      plot.margin        = ggplot2::margin(8, 12, 8, 8)
    )

  if (interactive) {
    return(
      plotly::ggplotly(p, tooltip = "text") |>
        plotly::layout(
          hoverlabel = list(bgcolor = "white", font = list(size = 12))
        )
    )
  }

  return(p)
}


# NULL-coalescing operator (base R >= 4.4 has it, keep internal for compat)
`%||%` <- function(a, b) if (!is.null(a)) a else b

##' Compute per-marker statistics for a VCF
##'
##' Calculates per-marker statistics including MAF, mean depth, missingness, heterozygosity, repeated-region overlap, and CNV difference from ploidy, for a given vcfR object. Optionally annotates markers with repeat and CNV information using provided BED and Qploidy window data.
##'
##' @param vcf A vcfR object containing genotype data.
##' @param bed_data Optional. A data.frame or data.table with columns chrom, start, end, representing repeated regions (BED format). Used to annotate markers as in repeats.
##' @param win_data Optional. A data.frame or data.table with columns Sample, Chr, Start, End, CN_call, as produced by Qploidy HMM window output. Used to annotate markers with percentage of samples with CNV different from ploidy.
##' @param dist_ploidy Integer. The reference ploidy level for CNV comparison (default 2 if NULL).
##'
##' @return A data.frame with per-marker statistics: CHR, Position, MarkerID, MAF, MeanDP, Pct_Missing, Pct_Het, and optionally Pct_CNV_diff_ploidy and In_Repeats.
##'
##' @importFrom vcfR extract.gt
##' @importFrom data.table as.data.table
##' @importFrom data.table setkey
##' @importFrom data.table foverlaps
##' @importFrom data.table setDT
##' @importFrom data.table setnames
##' @importFrom data.table data.table
get_stats_df <- function(vcf, bed_data, win_data, dist_ploidy, filter_samples = NULL){
  
  fix <- as.data.frame(vcf@fix, stringsAsFactors = FALSE)
  fix$POS <- as.numeric(fix$POS)
  n_markers <- nrow(fix)
  
  # --- Sample filter: apply first so all stats reflect only chosen samples ---
  req_samples <- filter_samples
  if (!is.null(req_samples) && length(req_samples) > 0) {
    gt_cols  <- colnames(vcf@gt)[-1]          # skip FORMAT column
    keep_col <- c(TRUE, gt_cols %in% req_samples)
    vcf@gt   <- vcf@gt[, keep_col, drop = FALSE]
  }
  
  # Extract GT matrix once
  gt_mat    <- vcfR::extract.gt(vcf, element = "GT")
  n_samples <- ncol(gt_mat)
  
  # --- Missing ---
  n_missing   <- rowSums(is.na(gt_mat) | gt_mat == "./." | gt_mat == "." |
                           gt_mat == ".|.", na.rm = FALSE)
  pct_missing <- n_missing / n_samples * 100
  
  # --- Depth ---
  dp_mat     <- vcfR::extract.gt(vcf, element = "DP", as.numeric = TRUE)
  mean_depth <- rowMeans(dp_mat, na.rm = TRUE)
  
  # --- Heterozygosity ---
  het_patterns <- c("0/1", "1/0", "0|1", "1|0")
  n_called <- rowSums(!is.na(gt_mat))
  n_het    <- rowSums(matrix(gt_mat %in% het_patterns, nrow = nrow(gt_mat)), na.rm = TRUE)
  het_pct  <- ifelse(n_called > 0, n_het / n_called * 100, NA_real_)
  
  # --- MAF ---
  allele_mat <- do.call(rbind, strsplit(as.vector(gt_mat), "[/|]"))
  allele_mat <- matrix(allele_mat, nrow = nrow(gt_mat))
  allele_mat[allele_mat == "."] <- NA
  allele_mat <- matrix(as.integer(allele_mat), nrow = nrow(gt_mat))
  n_alleles <- rowSums(!is.na(allele_mat))
  n_alt     <- rowSums(allele_mat > 0, na.rm = TRUE)
  af        <- ifelse(n_alleles > 0, n_alt / n_alleles, NA_real_)
  maf_vals  <- pmin(af, 1 - af)
  
  if (!is.null(bed_data)) {
    # Re-key: reactiveValues copies objects and data.table keys are dropped
    bed_dt <- as.data.table(bed_data)
    setkey(bed_dt, chrom, start, end)
    markers_dt <- data.table(
      chrom = fix$CHROM,
      start = as.integer(fix$POS) - 1L,
      end   = as.integer(fix$POS) - 1L
    )
    setkey(markers_dt, chrom, start, end)
    ov <- foverlaps(
      markers_dt, bed_dt,
      by.x = c("chrom", "start", "end"),
      by.y = c("chrom", "start", "end"),
      type = "any", mult = "first", nomatch = NA
    )
    in_repeats <- !is.na(ov[["start"]])  # TRUE = marker is inside a repeat
  }
  
  # --- CNV annotation (per marker, if win_data available) ------------------
  cn_counts <- NULL
  if (!is.null(win_data)) {
    ploidy_val <- as.integer(dist_ploidy %||% 2L)
    req_cols   <- c("Sample", "Chr", "Start", "End", "CN_call")
    win        <- as.data.frame(win_data)[, req_cols[req_cols %in% colnames(win_data)]]
    
    if (!all(req_cols %in% colnames(win))) {
      stop("Qploidy HMM_by_window file doesn't have the correct format.")
    }
    
    # Convert to data.table for fast subsetting
    data.table::setDT(win)
    data.table::setkey(win, Chr, Start, End)
    
    fix_dt <- data.table::as.data.table(fix)
    data.table::setnames(fix_dt, c("Chr", "Pos", "V3")[seq_len(ncol(fix_dt))])
    
    cn_counts <- fix_dt[, {
      sub <- win[Chr == .BY$Chr & Start <= .BY$Pos & End >= .BY$Pos]
      if (nrow(sub) > 0) 100 * mean(sub$CN_call != ploidy_val) else NA_real_
    }, by = .(Chr, Pos)]$V1
  }
  
  # --- Build marker stats table --------------------------------------------
  stats_df <- data.frame(
    CHR         = fix$CHROM,
    Position    = as.integer(fix$POS),
    MarkerID    = fix$ID,
    MAF         = round(maf_vals, 4),
    MeanDP      = round(mean_depth, 2),
    Pct_Missing = round(pct_missing, 2),
    Pct_Het     = round(het_pct, 2),
    stringsAsFactors = FALSE
  )
  if (!is.null(cn_counts))
    stats_df$Pct_CNV_diff_ploidy <- round(cn_counts, 2)
  if (!is.null(in_repeats))
    stats_df$In_Repeats <- in_repeats
  
  return(stats_df)
}


##' Filter markers based on statistics and user-defined thresholds
##'
##' Applies a series of filters to a VCF and its per-marker statistics, including MAF, missingness, heterozygosity, depth, repeat status, and CNV difference from ploidy. Returns the filtered VCF and statistics.
##'
##' @param vcf A vcfR object containing genotype data.
##' @param stats_df A data.frame of per-marker statistics as produced by get_stats_df.
##' @param filter_samples Character vector of sample IDs to retain (not used directly in this function, but assumed to be applied upstream).
##' @param filter_maf Numeric. Minimum minor allele frequency threshold.
##' @param filter_missing Numeric. Maximum allowed percent missing data per marker.
##' @param filter_het Numeric vector of length 2. Allowed range for percent heterozygosity.
##' @param filter_depth Numeric vector of length 2. Allowed range for mean depth.
##' @param filter_repeated Logical. If TRUE, remove markers in repeated regions (requires In_Repeats column in stats_df).
##' @param filter_cnv Numeric. Minimum percent of samples with CNV different from ploidy (requires Pct_CNV_diff_ploidy column in stats_df).
##'
##' @return A list with two elements: filtered vcfR object and filtered statistics data.frame.
stats_filter <- function(vcf, stats_df, filter_samples = NULL, 
                         filter_maf  = NULL, filter_missing = NULL, 
                         filter_het = NULL, filter_depth = NULL,
                         filter_repeated = NULL, filter_cnv = NULL){
  
  keep <- rep(TRUE, n_markers)
  
  # --- Sample filter: apply first so all stats reflect only chosen samples ---
  req_samples <- filter_samples
  if (!is.null(req_samples) && length(req_samples) > 0) {
    gt_cols  <- colnames(vcf@gt)[-1]          # skip FORMAT column
    keep_col <- c(TRUE, gt_cols %in% req_samples)
    vcf@gt   <- vcf@gt[, keep_col, drop = FALSE]
  }
  
  # MAF
  maf_thresh <- filter_maf %||% 0
  if (maf_thresh > 0)
    keep <- keep & !is.na(stats_df$MAF) & stats_df$MAF >= maf_thresh
  
  # Missing
  miss_thresh <- filter_missing %||% 100
  if (miss_thresh < 100)
    keep <- keep & stats_df$Pct_Missing <= miss_thresh
  
  # Heterozygosity range
  het_range <- filter_het
  if (!is.null(het_range))
    keep <- keep & !is.na(stats_df$Pct_Het) & stats_df$Pct_Het >= het_range[1] & stats_df$Pct_Het <= het_range[2]
  
  # Depth range
  dep_range <- filter_depth
  if (!is.null(dep_range))
    keep <- keep & !is.na(stats_df$MeanDP) &
    stats_df$MeanDP >= dep_range[1] & stats_df$MeanDP <= dep_range[2]
  
  # --- Repeated  ---
  rm_repeats <- filter_repeated
  if (!is.null(stats_df$In_Repeats) & !is.null(rm_repeats))
    keep <- keep & !stats_df$In_Repeats
  
  # --- Filter by CN
  cn_thres <- filter_cnv
  if (!is.null(stats_df$Pct_CNV_diff_ploidy) & !is.null(cn_thres))
    keep <- keep & !is.na(stats_df$Pct_CNV_diff_ploidy) & cn_thres >= stats_df$Pct_CNV_diff_ploidy
  
  # Apply marker filter
  vcf_filt <- vcf[keep, ]
  
  mk_counts_filtered <- nrow(vcf_filt@fix)
  
  # --- Build marker stats table --------------------------------------------
  fix_filt <- as.data.frame(vcf_filt@fix, stringsAsFactors = FALSE)
  stats_df_filt <- data.frame(
    CHR         = fix_filt$CHROM,
    Position    = as.integer(fix_filt$POS),
    MarkerID    = fix_filt$ID,
    MAF         = round(stats_df$MAF[keep], 4),
    MeanDP      = round(stats_df$MeanDP[keep], 2),
    Pct_Missing = round(stats_df$Pct_Missing[keep], 2),
    Pct_Het     = round(stats_df$Pct_Het[keep], 2),
    stringsAsFactors = FALSE
  )
  if (!is.null(stats_df$Pct_CNV_diff_ploidy))
    stats_df_filt$Pct_CNV_diff_ploidy <- round(stats_df$Pct_CNV_diff_ploidy[keep], 2)
  if (!is.null(stats_df$In_Repeats))
    stats_df_filt$In_Repeats <- stats_df$In_Repeats[keep]
  
  return(list(vcf_filt = vcf_filt, stats_df_filt = stats_df_filt))
}
