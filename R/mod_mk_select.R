#' Example UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
#' @import shinydisconnect
mod_mk_select_ui <- function(id){
  ns <- NS(id)
  tagList(
    # Add GWAS content here
    fluidRow(
      disconnectMessage(
        text = "An unexpected error occurred, please reload the application and check the input file(s).",
        refresh = "Reload now",
        background = "white",
        colour = "grey",
        overlayColour = "grey",
        overlayOpacity = 0.3,
        refreshColour = "purple"
      ),
      column(width = 12,
             box(title="Inputs", width = 12, collapsible = TRUE, collapsed = FALSE, status = "info", solidHeader = TRUE,
                 # --- Upload VCF (left) | OR divider | Built-in (right) ---
                 div(style = "position:relative;",
                   # OR divider - absolutely centered, spans full height
                   div(style = paste(
                     "position:absolute; left:calc(41.66% + 4px); top:0; bottom:0;",
                     "display:flex; flex-direction:column; align-items:center;",
                     "width:50px; margin-left:-25px; z-index:1; user-select:none;"
                   ),
                     div(style = "flex:1; width:1px; background:#ccc;"),
                     div(style = paste(
                       "background:#6c757d; color:white; border-radius:50%;",
                       "width:32px; height:32px; display:flex; align-items:center;",
                       "justify-content:center; font-size:12px; font-weight:bold; flex-shrink:0;"
                     ), "OR"),
                     div(style = "flex:1; width:1px; background:#ccc;")
                   ),
                   # Row 1: VCF
                   fluidRow(
                     column(width = 5,
                            fileInput(ns("mk_select_file"), "Upload VCF File", accept = c(".csv",".vcf",".gz"), width = "80%")
                     ),
                     column(width = 1),
                     column(width = 6,
                            tags$label("Choose a built-in dataset"),
                            selectInput(
                              ns("builtin_dataset"),
                              label    = NULL,
                              choices  = c("-- Select a dataset --" = "",
                                           "WGS Hawaii + KO34"  = "WGS_hawaii_KO34",
                                           "WGS Brazil + KO34"  = "WGS_Brazil_KO34",
                                           "WGS + KO34"  = "WGS_KO34"),
                              selected = "WGS_KO34", width = "80%"
                            )
                     )
                   ),
                   # Row 2: marker panel
                   fluidRow(
                     column(width = 5,
                            fileInput(ns("panel_file"), "Upload Marker Panel CSV", accept = c(".csv", ".txt", ".tsv", ".gz"), width = "80%")
                     ),
                     column(width = 1),
                     column(width = 6,
                            tags$label("Choose a built-in marker panel"),
                            selectInput(
                              ns("builtin_panel"),
                              label    = NULL,
                              choices  = c("-- Select a panel --" = "",
                                           "40k MolBreeding Panel" = "panel_40k_molbreeding"),
                              selected = "panel_40k_molbreeding", width = "80%"
                            )
                     )
                   ),
                   # Row 3: repeat intervals (optional)
                   fluidRow(
                     column(width = 5,
                            fileInput(ns("repeats_file"),
                                      label = HTML("Repeat Intervals <small style='color:grey;font-weight:normal'>(.bed / .tsv / .gz, no header) <em>optional</em></small>"),
                                      accept = c(".bed", ".gz", ".tsv"),
                                      width  = "80%"),
                            div(style = "margin-top:-20px; margin-bottom:20px;",
                                helpText("Unlocks colour-by repeats regions in the Marker Distribution plot.")
                            )
                     ),
                     column(width = 1),
                     column(width = 6,
                            tags$label(HTML("Choose a built-in repeat intervals file <small style='color:grey;font-weight:normal'><em>optional</em></small>")),
                            selectInput(
                              ns("builtin_repeats"),
                              label    = NULL,
                              choices  = c("-- None --" = "none",
                                           "Softmasked regions (WGS KO34)" = "softmasked_ko34"),
                              selected = "softmasked_ko34", width = "80%"
                            )
                     )
                   ),
                   # Row 4: Qploidy HMM window (optional)
                   fluidRow(
                     column(width = 5,
                            fileInput(ns("qploidy_window_file"),
                                      label = HTML("Qploidy HMM Results by Window <small style='color:grey;font-weight:normal'>(.csv) <em>optional</em></small>"),
                                      accept = c(".csv"),
                                      width  = "80%"),
                            div(style = "margin-top:-20px; margin-bottom:20px;",
                                helpText("Unlocks colour-by number of samples with CN different than defined ploidy in the Marker Distribution plot.")
                            )
                     ),
                     column(width = 1),
                     column(width = 6,
                            tags$label(HTML("Choose a built-in Qploidy HMM file <small style='color:grey;font-weight:normal'><em>optional</em></small>")),
                            selectInput(
                              ns("builtin_qploidy_window"),
                              label    = NULL,
                              choices  = c("-- None --" = "none",
                                           "WGS KO34 Panel - common markers" = "qploidy_window_vcf9",
                                           "WGS KO34 optimized filters" = "qploidy_window_vcf7"),
                              selected = "qploidy_window_vcf9", width = "80%"
                            )
                     )
                   ),
                   # Row 5: buttons
                   fluidRow(
                     column(width = 5,
                            actionButton(ns("mk_select_start"), "Load VCF", icon = icon("upload"),
                                         class = "btn-info")
                     ),
                     column(width = 1),
                     column(width = 6,
                            actionButton(ns("load_dataset"), "Load Dataset", icon = icon("database"),
                                         class = "btn-success")
                     )
                   )
                 ), br(),
                 # --- Progress bar ---
                 fluidRow(
                   column(width = 12,
                          shinyWidgets::progressBar(id = ns("pb_mk_select"), value = 0,
                                                    title = "", display_pct = FALSE,
                                                    status = "info", striped = TRUE)
                   )
                 ), hr(),
                 # --- Bottom row: dataset info spanning full width ---
                 fluidRow(
                   column(width = 12,
                        conditionalPanel(
                          condition = sprintf("input['%s'] == 'WGS_hawaii_KO34'", ns("builtin_dataset")),
                          div(style = "border-left: 4px solid #17a2b8; padding-left: 12px; background:#f8f9fa; border-radius:4px; padding: 12px 12px 12px 16px; margin-bottom:8px;",
                            HTML(paste0(
                              "<h5><b>WGS Hawaii + KO34</b></h5>",
                              "<p>Common markers between the 40K MolBreeding Marker Panel and the short reads Whole-genome sequencing dataset comprising <b>70 <em>Coffea arabica</em> accessions</b> ",
                              "from the <b>USDA Hawaii germplasm collection</b>, together with the ",
                              "publicly available HiFi and HiC <b>KO34</b>.</p>",
                              "<ul>",
                              "  <li><b>Samples:</b> 69 WGS accessions + KO34 HiFi + KO34 HiC (71 total)</li>",
                              "  <li><b>Origin:</b> USDA - Hawaii, USA</li>",
                              "  <li><b>Sequencing:</b> Illumina WGS, ~25&times; coverage + KO34 HiFi + KO34 HiC</li>",
                              "  <li><b>Sequencing data total markers (without filters):</b>  Illumina WGS (~7M); KO34 HiFi (~3M); KO34 HiC (~3M) </li>",
                              "  <li><b>Panel comparison:</b> Only Markers intersecting with the 40k MolBreeding Marker Panel</li>",
                              "</ul>"
                            ))
                          )
                        ),
                        conditionalPanel(
                          condition = sprintf("input['%s'] == 'WGS_Brazil_KO34'", ns("builtin_dataset")),
                          div(style = "border-left: 4px solid #28a745; padding-left: 12px; background:#f8f9fa; border-radius:4px; padding: 12px 12px 12px 16px; margin-bottom:8px;",
                            HTML(paste0(
                              "<h5><b>WGS Brazil + KO34</b></h5>",
                              "<p>Common markers between the 40K MolBreeding Marker Panel and the short reads Whole-genome sequencing dataset comprising <b>23 <em>Coffea arabica</em> accessions</b> ",
                              "from the <b>Brazilian germplasm collection (Embrapa)</b>, together with the ",
                              "publicly available HiFi and HiC <b>KO34</b>.</p>",
                              "<ul>",
                              "  <li><b>Samples:</b> 22 accessions + KO34 HiFi + KO34 HiC (24 total)</li>",
                              "  <li><b>Origin:</b> EMBRAPA - Brazil</li>",
                              "  <li><b>Sequencing:</b> Illumina WGS, ~25&times; coverage + KO34 HiFi + KO34 HiC</li>",
                              "  <li><b>Sequencing data total markers (without filters):</b>  Illumina WGS (~7M); KO34 HiFi (~3M); KO34 HiC (~3M) </li>",
                              "  <li><b>Panel comparison:</b> Only Markers intersecting with the 40k MolBreeding Marker Panel</li>",
                              "</ul>"
                            ))
                          )
                        ),
                        conditionalPanel(
                          condition = sprintf("input['%s'] == 'WGS_KO34'", ns("builtin_dataset")),
                          div(style = "border-left: 4px solid #6f42c1; padding-left: 12px; background:#f8f9fa; border-radius:4px; padding: 12px 12px 12px 16px; margin-bottom:8px;",
                            HTML(paste0(
                              "<h5><b>WGS + KO34</b></h5>",
                              "<p>Common markers between the 40K MolBreeding Marker Panel and the short reads whole-genome sequencing dataset comprising <b>all 91 <em>Coffea arabica</em> accessions</b> ",
                              "(Hawaii + Brazil collections), together with the publicly available HiFi and HiC <b>KO34</b>.</p>",
                              "<ul>",
                              "  <li><b>Samples:</b> 91 accessions + KO34_hifi + KO34_hic (93 total)</li>",
                              "  <li><b>Origin:</b> USDA Hawaii (USA) + EMBRAPA (Brazil)</li>",
                              "  <li><b>Sequencing:</b> Illumina WGS, ~25&times; coverage + KO34 HiFi + KO34 HiC</li>",
                              "  <li><b>Sequencing data total markers (without filters):</b>  Illumina WGS (~7M); KO34 HiFi (~3M); KO34 HiC (~3M) </li>",
                              "  <li><b>Panel comparison:</b> Only Markers intersecting with the 40k MolBreeding Marker Panel</li>",
                              "</ul>"
                            ))
                          )
                        )
                   )  # end column width=12
                 ),  # end fluidRow (dataset info)
                 fluidRow(
                   column(width = 12,
                          div(style="display:inline-block; float:right",dropdownButton(
                          HTML("<b>Input files</b>"),
                          p(downloadButton(ns('download_vcf'),""), "VCF Example File"),
                          p(HTML("<b>Parameters description:</b>"), actionButton(ns("goPar"), icon("arrow-up-right-from-square", verify_fa = FALSE) )), hr(),
                          p(HTML("<b>Results description:</b>"), actionButton(ns("goRes"), icon("arrow-up-right-from-square", verify_fa = FALSE) )), hr(),
                          p(HTML("<b>How to cite:</b>"), actionButton(ns("goCite"), icon("arrow-up-right-from-square", verify_fa = FALSE) )), hr(),
                          actionButton(ns("mk_select_summary"), "Summary"),
                          circle = FALSE,
                          status = "warning",
                          icon = icon("info"), width = "300px",
                          tooltip = tooltipOptions(title = "Click to see info!")
                        ))
                   )  # end column width=12
                 )  # end fluidRow (info button)
             )  # end box
      ),
      # --- Summary value boxes ---
      column(width = 12,
             fluidRow(
               column(width = 3, bs4Dash::valueBoxOutput(ns("vbox_wgs_markers"), width = 12)),
               column(width = 3, bs4Dash::valueBoxOutput(ns("vbox_panel_markers"), width = 12)),
               column(width = 3, bs4Dash::valueBoxOutput(ns("vbox_common_markers"), width = 12)),
               column(width = 3, bs4Dash::valueBoxOutput(ns("vbox_filtered_markers"), width = 12))
             )
      ),
      column(width = 12,
             box(
               title = "Common Markers", status = "info", solidHeader = TRUE,
               icon = icon("circle-nodes"), width = 12, maximizable = TRUE,
               box(
                 title = "Marker Filters", status = "success", solidHeader = FALSE,
                 width = 12, collapsible = TRUE, collapsed = TRUE,
                 fluidRow(
                   column(width = 6,
                          numericInput(ns("filter_maf"),
                                       label = HTML("Min MAF <small style='color:grey;font-weight:normal'>(higher = fewer, more informative markers)</small>"),
                                       value = 0, min = 0, max = 0.5, step = 0.01, width = "70%"),
                          helpText("0 = keep all markers; 0.05 = keep only markers with MAF > 5%")
                   ),
                   column(width = 6,
                          numericInput(ns("filter_missing"),
                                       label = HTML("Max % Missing <small style='color:grey;font-weight:normal'>(lower = less missing data allowed)</small>"),
                                       value = 100, min = 0, max = 100, step = 1, width = "70%"),
                          helpText("100 = allow any amount of missing data; 20 = keep markers with \u226420% missing calls")
                   )
                 ), hr(),
                 fluidRow(
                   column(width = 6,
                          sliderInput(ns("filter_het"),
                                      label = HTML("% Heterozygosity Range <small style='color:grey;font-weight:normal'>(narrow range = stricter filter)</small>"),
                                      min = 0, max = 100, value = c(0, 100), step = 1, post = "%", width = "70%"),
                          helpText("Keep markers whose observed heterozygosity falls within this range")
                   ),
                   column(width = 6,
                          numericInput(ns("filter_cnv"),
                                       label = HTML("Max % CNV \u2260 defined ploidy <small style='color:grey;font-weight:normal'>(lower = fewer CNV-affected genotypes allowed)</small>"),
                                       value = 100, min = 0, max = 100, step = 1, width = "70%"),
                          helpText("100 = allow any CNV; 10 = keep markers where \u226410% of genotypes have CNV \u2260 2")
                   )
                 ), hr(),
                 fluidRow(
                   column(width = 6,
                          uiOutput(ns("ui_filter_depth")),
                          helpText("Only markers whose mean read depth falls within the selected range will be kept.")
                   ),
                   column(width = 6,
                          tags$label(HTML("Avoid Repeated Regions <small style='color:grey;font-weight:normal'>(exclude markers in repetitive genomic regions)</small>")),
                          shinyWidgets::prettyRadioButtons(
                            ns("filter_repeated"),
                            label   = NULL,
                            choices = c("TRUE", "FALSE"),
                            selected = "FALSE",
                            inline  = TRUE,
                            status  = "info"
                          ),
                          helpText("TRUE = exclude markers flagged as repeated regions; FALSE = keep all.")
                   )
                 ), hr(),
                 fluidRow(
                   column(width = 12,
                          tags$label(HTML("Samples to include <small style='color:grey;font-weight:normal'>(all selected by default)</small>")),
                          shinyWidgets::pickerInput(
                            ns("filter_samples"),
                            label   = NULL,
                            choices = NULL,
                            selected = NULL,
                            multiple = TRUE,
                            options = shinyWidgets::pickerOptions(
                              actionsBox       = TRUE,
                              liveSearch       = TRUE,
                              liveSearchPlaceholder = "Search samples...",
                              selectedTextFormat = "count > 3",
                              countSelectedText  = "{0} of {1} samples selected",
                              noneSelectedText   = "No samples selected",
                              selectAllText      = "Select All",
                              deselectAllText    = "Deselect All",
                              size = 10
                            ),
                            width = "100%"
                          )
                   )
                 ),
                 fluidRow(
                   column(width = 12,
                          div(style = "margin-top:8px",
                              actionButton(ns("apply_filters"), "Apply Filters",
                                           icon = icon("filter"), class = "btn-success")
                          )
                   )
                 ), br(),
                 fluidRow(
                   column(width = 12,
                          shinyWidgets::progressBar(id = ns("pb_filters"), value = 0,
                                                    title = "", display_pct = FALSE,
                                                    status = "success", striped = TRUE)
                   )
                 )
               ),
               # --- Marker Distribution ---
               box(
                 title = "Marker Distribution", status = "info", solidHeader = FALSE,
                 icon = icon("chart-bar"),
                 width = 12, collapsible = TRUE, collapsed = TRUE,
                 fluidRow(
                   column(width = 4,
                          uiOutput(ns("ui_colour_by"))
                   ),
                   column(width = 5,
                          numericInput(ns("dist_ploidy"), label = "Ploidy",
                                       value = 2L, min = 1L, max = 12L, step = 1L, width = "50%")
                   ),
                   column(width = 3,
                          tags$label("Interactive plot"),
                          shinyWidgets::prettyRadioButtons(
                            ns("dist_interactive"),
                            label    = NULL,
                            choices  = c("TRUE", "FALSE"),
                            selected = "TRUE",
                            inline   = TRUE,
                            status   = "info"
                          )
                   )
                 ),
                 fluidRow(
                   column(width = 12,
                          shinyWidgets::progressBar(id = ns("pb_plot"), value = 0,
                                                    title = "", display_pct = FALSE,
                                                    status = "info", striped = TRUE)
                   )
                 ),
                 fluidRow(
                   column(width = 12,
                          uiOutput(ns("marker_distribution_plot_ui"))
                   )
                 ),
                 fluidRow(
                   column(width = 12,
                          div(style = "float:left",
                              dropdownButton(
                                tags$h3("Save Marker Distribution"),
                                selectInput(ns("dist_image_type"), "File Type", choices = c("jpeg","tiff","png","svg"), selected = "jpeg"),
                                sliderInput(ns("dist_image_res"), "Resolution", value = 300, min = 50, max = 1000, step = 50),
                                sliderInput(ns("dist_image_width"), "Width", value = 10, min = 1, max = 20, step = 0.5),
                                sliderInput(ns("dist_image_height"), "Height", value = 6, min = 1, max = 20, step = 0.5),
                                downloadButton(ns("download_dist_plot"), "Save Image"),
                                circle = FALSE, status = "danger",
                                icon = icon("floppy-disk"), width = "300px", label = "Save",
                                tooltip = tooltipOptions(title = "Save distribution plot")
                              )
                          )
                   )
                 )
               ),
               # --- Selected Markers ---
               box(
                 title = "Selected Markers", status = "info", solidHeader = FALSE,
                 icon = icon("table"),
                 width = 12, collapsible = TRUE, collapsed = TRUE,
                 fluidRow(
                   column(width = 12,
                          div(style = "margin-bottom:8px; display:flex; gap:8px;",
                              downloadButton(ns("download_selected_markers"), "Save Table (.csv)",
                                             icon = icon("floppy-disk")),
                              downloadButton(ns("download_filtered_vcf"), "Save Filtered VCF (.vcf.gz)",
                                             icon = icon("file-arrow-down"))
                          )
                   )
                 ),
                 fluidRow(
                   column(width = 12,
                          DT::DTOutput(ns("selected_markers_table"))
                   )
                 )
               )
             )
      )
    )
  )
}

#' mk_select Server Functions
#'
#' @importFrom graphics axis hist points
#' @import ggplot2
#' @importFrom scales comma_format
#'
#' @noRd
mod_mk_select_server <- function(input, output, session, parent_session){
  
  ns <- session$ns

  # --- Summary value boxes (initialised as placeholders) ---
  mk_counts <- reactiveValues(
    wgs = 0, panel = 0, common = 0, filtered = 0
  )

  # --- Reactive storage for analysis outputs ---
  mk_select_items <- reactiveValues(
    mk_select_df = NULL,
    dosage_df    = NULL,
    het_df       = NULL,
    maf_df       = NULL,
    pos_df       = NULL,
    markerPlot   = NULL,
    snp_stats    = NULL,
    vcf_result   = NULL,   # list returned by load_vcf_panel
    vcf_filtered = NULL,   # vcfR after applying marker/sample filters
    marker_stats = NULL    # data.frame for Selected Markers table
  )

  # --- Optional annotation file storage ---
  opt_files <- reactiveValues(
    bed_data = NULL,   # data.table: chrom/start/end
    cnv_data = NULL    # data.frame: Qploidy HMM by-window
  )

  output$vbox_wgs_markers <- bs4Dash::renderValueBox({
    bs4Dash::valueBox(
      value = mk_counts$wgs, subtitle = "WGS Markers",
      icon = icon("dna"), color = "info"
    )
  })
  output$vbox_panel_markers <- bs4Dash::renderValueBox({
    bs4Dash::valueBox(
      value = mk_counts$panel, subtitle = "Panel Markers",
      icon = icon("list"), color = "success"
    )
  })
  output$vbox_common_markers <- bs4Dash::renderValueBox({
    bs4Dash::valueBox(
      value = mk_counts$common, subtitle = "Common Markers",
      icon = icon("circle-nodes"), color = "purple"
    )
  })
  output$vbox_filtered_markers <- bs4Dash::renderValueBox({
    bs4Dash::valueBox(
      value = mk_counts$filtered, subtitle = "Markers After Filters",
      icon = icon("filter"), color = "warning"
    )
  })

  # --- Built-in path resolver --------------------------------------------------
  builtin_vcf_paths <- list(
    WGS_hawaii_KO34 = "coffee/all_samples_merged_panel_intersect.vcf.gz",  # TODO: replace
    WGS_Brazil_KO34 = "coffee/all_samples_merged_panel_intersect.vcf.gz",  # TODO: replace
    WGS_KO34        = "coffee/all_samples_merged_panel_intersect.vcf.gz"
  )
  builtin_panel_paths <- list(
    panel_40k_molbreeding = "coffee/Coffee_40k_MolBr_SNP.csv"
  )
  builtin_repeats_paths <- list(
    none = NULL,
    softmasked_ko34 = "coffee/repeats_intervals.tsv.gz"
  )
  builtin_qploidy_window_paths <- list(
    none = NULL,
    qploidy_window_vcf9 = "coffee/vcf9_hmm_CN_estimation_by_window.csv",
    qploidy_window_vcf7 = "coffee/vcf7_hmm_CN_estimation_by_window.csv"
  )

  .resolve_builtin <- function(vcf_key, panel_key, rep_key = NULL, cnv_key = NULL) {
    vcf_file   <- builtin_vcf_paths[[vcf_key]]
    panel_file <- builtin_panel_paths[[panel_key]]
    list(
      vcf_path  = system.file(vcf_file,   package = "GenoBrew"),
      panel_path = system.file(panel_file, package = "GenoBrew"),
      repeats_bed = system.file(builtin_repeats_paths[[rep_key]], package = "GenoBrew"),
      cn_window = system.file(builtin_qploidy_window_paths[[cnv_key]], package = "GenoBrew")
    )
  }

  # --- Optional: load repeat-intervals BED (uploaded file) --------------------
  observeEvent(input$mk_select_start, {
    req(input$repeats_file && input$repeats_file$datapath != "")
    shinyWidgets::updateProgressBar(session, "pb_mk_select", value = 10, title = "Loading repeat-intervals BED...")
    opt_files$bed_data <- data.table::fread(
      input$repeats_file$datapath,
      header = TRUE, sep = "\t", select = 1:3,
      col.names  = c("chrom", "start", "end"),
      colClasses = c("character", "integer", "integer"),
      showProgress = FALSE
    ) 
  })

  # --- Optional: load repeat-intervals BED (built-in) -------------------------
  observeEvent(input$load_dataset, {
    req(input$builtin_repeats != "")
    key <- input$builtin_repeats
    if (!nzchar(key)) { opt_files$bed_data <- NULL; return() }
    shinyWidgets::updateProgressBar(session, "pb_mk_select", value = 10, title = "Loading repeat-intervals BED...")

    path <- system.file(builtin_repeats_paths[[key]], package = "GenoBrew")
    req(file.exists(path))
    opt_files$bed_data <- data.table::fread(
      path,
      header = TRUE, sep = "\t", select = 1:3,
      col.names  = c("chrom", "start", "end"),
      colClasses = c("character", "integer", "integer"),
      showProgress = FALSE
    )
  })

  # --- Optional: load Qploidy HMM window CSV (uploaded file) ------------------
  observeEvent(input$mk_select_start, {
    req(input$qploidy_window_file && input$qploidy_window_file$datapath != "")
    shinyWidgets::updateProgressBar(session, "pb_mk_select", value = 20, title = "Loading Qploidy HMM window CSV...")
    opt_files$cnv_data <- as.data.frame(
      data.table::fread(input$qploidy_window_file$datapath, showProgress = FALSE)
    )
  })

  # --- Optional: load Qploidy HMM window CSV (built-in) -----------------------
  observeEvent(input$load_dataset, {
    req(input$builtin_qploidy_window != "")
    key <- input$builtin_qploidy_window
    if (!nzchar(key)) { opt_files$cnv_data <- NULL; return() }
    shinyWidgets::updateProgressBar(session, "pb_mk_select", value = 20, title = "Loading Qploidy HMM window CSV...")
    path <- system.file(builtin_qploidy_window_paths[[key]], package = "GenoBrew")
    req(file.exists(path))
    opt_files$cnv_data <- as.data.frame(
      data.table::fread(path, showProgress = FALSE)
    )
  })

  # --- Load built-in dataset --------------------------------------------------
  observeEvent(input$load_dataset, {
    req(input$builtin_dataset != "", input$builtin_panel != "")
    paths <- .resolve_builtin(input$builtin_dataset, input$builtin_panel, input$builtin_repeats, input$builtin_qploidy_window)
    req(file.exists(paths$vcf_path), file.exists(paths$panel_path))

    shinyWidgets::updateProgressBar(session, "pb_mk_select", value = 25, title = "Reading panel...")
    panel_df <- read.csv(paths$panel_path)

    shinyWidgets::updateProgressBar(session, "pb_mk_select", value = 50, title = "Loading VCF...")
    result <- load_vcf_panel(panel_df = panel_df, vcf_path = paths$vcf_path)

    shinyWidgets::updateProgressBar(session, "pb_mk_select", value = 80, title = "Calculating marker stats...")

    mk_select_items$marker_stats <- get_stats_df(vcf = result$vcf, bed_data = opt_files$bed_data, 
                                                  win_data = opt_files$cnv_data, dist_ploidy = as.numeric(input$dist_ploidy), 
                                                  filter_samples = NULL)

    mk_select_items$vcf_result <- result
    mk_counts$wgs      <- result$n_wgs
    mk_counts$panel    <- result$n_panel
    mk_counts$common   <- result$n_common
    mk_counts$filtered <- result$n_common   # no filter applied yet
    mk_select_items$vcf_filtered <- NULL    # reset filters on new load
    mk_select_items$marker_stats_filtered <- NULL

    # populate sample picker
    samples <- colnames(result$vcf@gt)[-1]  # first column is FORMAT
    shinyWidgets::updatePickerInput(session, "filter_samples",
                                    choices = samples, selected = samples)

    shinyWidgets::updateProgressBar(session, "pb_mk_select", value = 100, title = "Done")
  })

  # --- Load uploaded VCF + panel -----------------------------------------------
  observeEvent(input$mk_select_start, {
    req(input$mk_select_file, input$panel_file)

    shinyWidgets::updateProgressBar(session, "pb_mk_select", value = 20, title = "Reading panel...")
    panel_df <- read.csv(input$panel_file$datapath)

    shinyWidgets::updateProgressBar(session, "pb_mk_select", value = 50, title = "Loading VCF...")
    result <- load_vcf_panel(panel_df = panel_df, vcf_path = input$mk_select_file$datapath)

    mk_select_items$marker_stats <- get_stats_df(result$vcf, opt_files$bed_data, opt_files$cnv_data, 2, colnames(result$vcf@gt)[-1])

    mk_select_items$vcf_result <- result
    mk_counts$wgs      <- result$n_wgs
    mk_counts$panel    <- result$n_panel
    mk_counts$common   <- result$n_common
    mk_counts$filtered <- result$n_common
    mk_select_items$vcf_filtered <- NULL    # reset filters on new load
    mk_select_items$marker_stats_filtered <- NULL

    samples <- colnames(result$vcf@gt)[-1]
    shinyWidgets::updatePickerInput(session, "filter_samples",
                                    choices = samples, selected = samples)

    shinyWidgets::updateProgressBar(session, "pb_mk_select", value = 100, title = "Done")
  })


  # --- Dynamic colour_by selector (unlocks options when files are loaded) ------
  output$ui_colour_by <- renderUI({
    choices <- c("None" = "", "Missing" = "missing", "Depth" = "depth",
                 "Heterozygosity" = "heterozygosity", "MAF" = "MAF")
    if (!is.null(opt_files$bed_data)) choices <- c(choices, "Repeated regions" = "repeated")
    if (!is.null(opt_files$cnv_data)) choices <- c(choices, "CNV (samples ≠ ploidy)" = "CNV")
    selectInput(ns("dist_colour_by"), label = "Colour by",
                choices = choices, selected = "", width = "100%")
  })

  # --- Marker Distribution plot UI (static or interactive) -------------------
  output$marker_distribution_plot_ui <- renderUI({
    if (identical(input$dist_interactive, "TRUE")) {
      plotly::plotlyOutput(ns("marker_distribution_plotly"), height = "600px")
    } else {
      plotOutput(ns("marker_distribution_plot"), height = "600px")
    }
  })

  # --- Marker Distribution plot (static ggplot) --------------------------------
  output$marker_distribution_plot <- renderPlot({

    req(!is.null(active_vcf()), identical(input$dist_interactive, "FALSE"))

    shinyWidgets::updateProgressBar(session, "pb_mk_select", value = 20, title = "Preparing plot data...")
    shinyWidgets::updateProgressBar(session, "pb_plot",      value = 20, title = "Preparing plot data...")

    colour_by <- if (!is.null(input$dist_colour_by) && nzchar(input$dist_colour_by))
      input$dist_colour_by else NULL

    shinyWidgets::updateProgressBar(session, "pb_mk_select", value = 60, title = "Rendering plot...")
    shinyWidgets::updateProgressBar(session, "pb_plot",      value = 60, title = "Rendering plot...")
   
    p <- plot_marker_positions(
      marker_stats          = if(!is.null(mk_select_items$marker_stats_filtered)) mk_select_items$marker_stats_filtered else mk_select_items$marker_stats,
      colour_by             = colour_by,
      ploidy                = input$dist_ploidy 
    )

    shinyWidgets::updateProgressBar(session, "pb_mk_select", value = 100, title = "Done")
    shinyWidgets::updateProgressBar(session, "pb_plot",      value = 100, title = "Done")
    
    p
  }, res = 120)

  # --- Marker Distribution plot (interactive plotly) ---------------------------
  output$marker_distribution_plotly <- plotly::renderPlotly({

    req(!is.null(active_vcf()), identical(input$dist_interactive, "TRUE"))

    shinyWidgets::updateProgressBar(session, "pb_mk_select", value = 20, title = "Preparing plot data...")
    shinyWidgets::updateProgressBar(session, "pb_plot",      value = 20, title = "Preparing plot data...")

    colour_by <- if (!is.null(input$dist_colour_by) && nzchar(input$dist_colour_by))
      input$dist_colour_by else NULL

    shinyWidgets::updateProgressBar(session, "pb_mk_select", value = 60, title = "Rendering plot...")
    shinyWidgets::updateProgressBar(session, "pb_plot",      value = 60, title = "Rendering plot...")

    p <- plot_marker_positions(
      colour_by             = colour_by,
      marker_stats          = if(!is.null(mk_select_items$marker_stats_filtered)) mk_select_items$marker_stats_filtered else mk_select_items$marker_stats,
      ploidy                = input$dist_ploidy %||% 2L,
      interactive = as.logical(input$dist_interactive)
    )

    shinyWidgets::updateProgressBar(session, "pb_mk_select", value = 100, title = "Done")
    shinyWidgets::updateProgressBar(session, "pb_plot",      value = 100, title = "Done")

    p
  })
  
  
  # --- Apply Filters -----------------------------------------------------------
  observeEvent(input$apply_filters, {
    req(!is.null(mk_select_items$marker_stats), !is.null(mk_select_items$vcf_result))

  shinyWidgets::updateProgressBar(session, "pb_mk_select", value = 25, title = "Applying filters...")
  shinyWidgets::updateProgressBar(session, "pb_filters",   value = 25, title = "Applying filters...")


  print(input$filter_samples)

  filtered <- stats_filter(vcf = mk_select_items$vcf_result$vcf, 
                          stats_df = mk_select_items$marker_stats, 
                           filter_samples = input$filter_samples, 
                           filter_maf = as.numeric(input$filter_maf), 
                           filter_missing = as.numeric(input$filter_missing), 
                           filter_het = as.numeric(input$filter_het), 
                           filter_depth = as.numeric(input$filter_depth), 
                           filter_repeated = as.logical(input$filter_repeated), 
                           filter_cnv = as.numeric(input$filter_cnv))

    mk_select_items$marker_stats_filtered <- filtered$stats_df_filt
    mk_select_items$vcf_filtered <- filtered$vcf_filt
    mk_counts$filtered <- nrow(filtered$stats_df_filt)

    shinyWidgets::updateProgressBar(session, "pb_mk_select", value = 100, title = "Done")
    shinyWidgets::updateProgressBar(session, "pb_filters",   value = 100, title = "Done")
  })

  # --- Helper: active vcf (filtered if available, else full) ---
  active_vcf <- reactive({
    if (!is.null(mk_select_items$vcf_filtered))
      mk_select_items$vcf_filtered
    else if (!is.null(mk_select_items$vcf_result))
      mk_select_items$vcf_result$vcf
    else
      NULL
  })

  # --- Selected Markers table -------------------------------------------------
  output$selected_markers_table <- DT::renderDT({
    df <- if(!is.null(mk_select_items$marker_stats_filtered)) mk_select_items$marker_stats_filtered else mk_select_items$marker_stats
    if (is.null(df)) {
      # Show a placeholder when no filters have been applied yet
      return(DT::datatable(
        data.frame(Info = "Load data to populate this table."),
        options = list(dom = "t"), rownames = FALSE
      ))
    }
    DT::datatable(
      df,
      filter    = "top",
      rownames  = FALSE,
      extensions = "Buttons",
      options   = list(
        pageLength  = 25,
        scrollX     = TRUE,
        dom         = "Bfrtip",
        buttons     = list("colvis"),
        columnDefs  = list(list(className = "dt-right",
                                targets   = which(sapply(df, is.numeric)) - 1L))
      )
    )
  })

  # --- Download: marker stats CSV ---------------------------------------------
  output$download_selected_markers <- downloadHandler(
    filename = function() paste0("GenoBrew_selected_markers_", Sys.Date(), ".csv"),
    content  = function(file) {
      df <- if(!is.null(mk_select_items$marker_stats_filtered)) mk_select_items$marker_stats_filtered else mk_select_items$marker_stats
      if (is.null(df)) df <- data.frame()
      write.csv(df, file, row.names = FALSE)
    }
  )

  # --- Download: filtered VCF -------------------------------------------------
  output$download_filtered_vcf <- downloadHandler(
    filename = function() paste0("GenoBrew_filtered_markers_", Sys.Date(), ".vcf.gz"),
    content  = function(file) {
      vcf_out <- mk_select_items$vcf_filtered
      if (is.null(vcf_out)) vcf_out <- mk_select_items$vcf_result$vcf
      req(!is.null(vcf_out))
      # Write to a temp path ending in .vcf.gz so vcfR uses gzfile internally
      tmp <- paste0(tempfile(), ".vcf.gz")
      vcfR::write.vcf(vcf_out, file = tmp)
      file.copy(tmp, file, overwrite = TRUE)
      unlink(tmp)
    }
  )

  output$ui_filter_depth <- renderUI({
    depth_col <- NULL
    if (!is.null(mk_select_items$marker_stats)) {
      depth_col <- mk_select_items$marker_stats$MeanDP
    }
    if (!is.null(depth_col) && length(depth_col) > 0) {
      d_min <- floor(min(depth_col, na.rm = TRUE))
      d_max <- ceiling(max(depth_col, na.rm = TRUE))
    } else {
      d_min <- 0
      d_max <- 500
    }
    sliderInput(ns("filter_depth"),
                label = HTML("Mean Depth Range <small style='color:grey;font-weight:normal'>(keep markers with mean depth within this range)</small>"),
                min = d_min, max = d_max, value = c(d_min, d_max), step = 1, width = "70%")
  })

  # --- Update sample picker when dataset or VCF changes ---

  # Help links
  observeEvent(input$goPar, {
    # change to help tab
    bs4Dash::updatebs4TabItems(session = parent_session, inputId = "MainMenu",
                      selected = "help")
    
    # select specific tab
    updateTabsetPanel(session = parent_session, inputId = "Genomic_mk_select_tabset",
                      selected = "Genomic_mk_select_par")
    # expand specific box
    updateBox(id = "Genomic_mk_select_box", action = "toggle", session = parent_session)
  })
  
  observeEvent(input$goRes, {
    # change to help tab
    bs4Dash::updatebs4TabItems(session = parent_session, inputId = "MainMenu",
                      selected = "help")
    
    # select specific tab
    updateTabsetPanel(session = parent_session, inputId = "Genomic_mk_select_tabset",
                      selected = "Genomic_mk_select_results")
    # expand specific box
    updateBox(id = "Genomic_mk_select_box", action = "toggle", session = parent_session)
  })
  
  observeEvent(input$goCite, {
    # change to help tab
    bs4Dash::updatebs4TabItems(session = parent_session, inputId = "MainMenu",
                      selected = "help")
    
    # select specific tab
    updateTabsetPanel(session = parent_session, inputId = "Genomic_mk_select_tabset",
                      selected = "Genomic_mk_select_cite")
    # expand specific box
    updateBox(id = "Genomic_mk_select_box", action = "toggle", session = parent_session)
  })
  
  output$download_vcf <- downloadHandler(
    filename = function() {
      paste0("BIGapp_VCF_Example_file.vcf.gz")
    },
    content = function(file) {
      ex <- system.file("iris_DArT_VCF.vcf.gz", package = "BIGapp")
      file.copy(ex, file)
    })

# --- Download: Marker Distribution Plot (static or interactive) ---
  output$download_dist_plot <- downloadHandler(
    filename = function() {
      ext <- input$dist_image_type
      if (identical(input$dist_interactive, "TRUE") && ext == "html") {
        paste0("GenoBrew_marker_distribution_", Sys.Date(), ".html")
      } else {
        paste0("GenoBrew_marker_distribution_", Sys.Date(), ".", ext)
      }
    },
    content = function(file) {
      colour_by <- if (!is.null(input$dist_colour_by) && nzchar(input$dist_colour_by)) input$dist_colour_by else NULL
      marker_stats <- if(!is.null(mk_select_items$marker_stats_filtered)) mk_select_items$marker_stats_filtered else mk_select_items$marker_stats
      ploidy_val <- input$dist_ploidy %||% 2L
      interactive <- identical(input$dist_interactive, "TRUE")
      ext <- input$dist_image_type
      width <- input$dist_image_width %||% 10
      height <- input$dist_image_height %||% 6
      res <- input$dist_image_res %||% 300
      if (interactive && ext == "html") {
        # Save interactive plot as HTML
        p <- plot_marker_positions(
          marker_stats = marker_stats,
          colour_by    = colour_by,
          ploidy       = ploidy_val,
          interactive  = TRUE
        )
        pltly <- plotly::ggplotly(p)
        htmlwidgets::saveWidget(pltly, file)
      } else {
        # Save static plot as image
        p <- plot_marker_positions(
          marker_stats = marker_stats,
          colour_by    = colour_by,
          ploidy       = ploidy_val,
          interactive  = FALSE
        )
        if (ext == "svg") {
          svglite::svglite(file, width = width, height = height)
          print(p)
          dev.off()
        } else {
          ggsave(
            filename = file,
            plot = p,
            device = ext,
            width = width,
            height = height,
            units = "in",
            dpi = res
          )
        }
      }
    }
  )
  
  ##Summary Info
  mk_select_summary_info <- function() {
    # Handle possible NULL values for inputs
    vcf_file <- if (!is.null(input$mk_select_file$name)) input$mk_select_file$name else "No file selected"
    panel_file <- if (!is.null(input$mk_select_panel$name)) input$mk_select_panel$name else "Not selected"
    selected_ploidy <- if (!is.null(input$dist_ploidy)) input$dist_ploidy else "Not selected"
    filter_maf <- if (!is.null(input$filter_maf)) input$filter_maf else "Not selected"
    filter_missing <- if (!is.null(input$filter_missing)) input$filter_missing else "Not selected"
    filter_het <- if (!is.null(input$filter_het)) paste0(input$filter_het[1], "% - ", input$filter_het[2], "%") else "Not selected"
    filter_cnv <- if (!is.null(input$filter_cnv)) input$filter_cnv else "Not selected"
    filter_repeated <- if (!is.null(input$filter_repeated)) input$filter_repeated else "Not selected"
    filter_samples <- if (!is.null(input$filter_samples)) paste(input$filter_samples, collapse = ", ") else "Not selected"
    
    # Print the summary information
    cat(
      "GenoBrew Summary Metrics Summary\n",
      "\n",
      paste0("Date: ", Sys.Date()), "\n",
      paste(R.Version()$version.string), "\n",
      "\n",
      "### Input Files ###\n",
      "\n",
      paste("VCF File:", vcf_file), "\n",
      paste("Panel File:", panel_file), "\n",
      "\n",
      "### User Selected Parameters ###\n",
      "\n",
      paste("Selected Ploidy:", selected_ploidy), "\n",
      paste("Filter MAF:", filter_maf), "\n",
      paste("Filter Missing:", filter_missing), "\n",
      paste("Filter Heterozygosity:", filter_het), "\n",
      paste("Filter CNV:", filter_cnv), "\n",
      paste("Filter Repeated:", filter_repeated), "\n",
      paste("Filter Samples:", filter_samples), "\n",
      "\n",
      "### R Packages Used ###\n",
      "\n",
      paste("GenoBrew:", packageVersion("GenoBrew")), "\n",
      paste("ggplot2:", packageVersion("ggplot2")), "\n",
      paste("vcfR:", packageVersion("vcfR")), "\n",
      sep = ""
    )
  }
  
  # Popup for analysis summary
  observeEvent(input$mk_select_summary, {
    showModal(modalDialog(
      title = "Summary Information",
      size = "l",
      easyClose = TRUE,
      footer = tagList(
        modalButton("Close"),
        downloadButton("download_mk_select_info", "Download")
      ),
      pre(
        paste(capture.output(mk_select_summary_info()), collapse = "\n")
      )
    ))
  })
  
  
  # Download Summary Info
  output$download_mk_select_info <- downloadHandler(
    filename = function() {
      paste("mk_select_summary_", Sys.Date(), ".txt", sep = "")
    },
    content = function(file) {
      # Write the summary info to a file
      writeLines(paste(capture.output(mk_select_summary_info()), collapse = "\n"), file)
    }
  )
}

## To be copied in the UI
# mod_mk_select_ui("mk_select_1")

## To be copied in the server
# mod_mk_select_server("mk_select_1")
