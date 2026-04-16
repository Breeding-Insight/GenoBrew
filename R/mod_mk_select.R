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
#' @importFrom plotly plotlyOutput renderPlotly
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
                   # Row 1: main data
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
                              selected = "", width = "80%"
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
                              selected = "", width = "80%"
                            )
                     )
                   ),
                   # Row 3: buttons
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
                 ),
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
                              "<p>Whole-genome sequencing dataset comprising <b>70 <em>Coffea arabica</em> accessions</b> ",
                              "from the <b>University of Hawaii germplasm collection</b>, together with the ",
                              "reference genotype <b>KO34</b>.</p>",
                              "<ul>",
                              "  <li><b>Samples:</b> 69 WGS accessions + KO34 (70 total)</li>",
                              "  <li><b>Origin:</b> USDA - Hawaii, USA</li>",
                              "  <li><b>Sequencing:</b> Illumina WGS, ~25&times; coverage + KO34 HiFi</li>",
                              "  <li><b>Panel comparison:</b> Markers intersected with the 40k MolBreeding Marker Panel</li>",
                              "</ul>"
                            ))
                          )
                        ),
                        conditionalPanel(
                          condition = sprintf("input['%s'] == 'WGS_Brazil_KO34'", ns("builtin_dataset")),
                          div(style = "border-left: 4px solid #28a745; padding-left: 12px; background:#f8f9fa; border-radius:4px; padding: 12px 12px 12px 16px; margin-bottom:8px;",
                            HTML(paste0(
                              "<h5><b>WGS Brazil + KO34</b></h5>",
                              "<p>Whole-genome sequencing dataset comprising <b>23 <em>Coffea arabica</em> accessions</b> ",
                              "from the <b>Brazilian germplasm collection (Embrapa)</b>, together with the ",
                              "reference genotype <b>KO34</b>.</p>",
                              "<ul>",
                              "  <li><b>Samples:</b> 22 accessions + KO34 (23 total)</li>",
                              "  <li><b>Origin:</b> EMBRAPA - Brazil</li>",
                              "  <li><b>Sequencing:</b> Illumina WGS, ~25&times; coverage + KO34 HiFi</li>",
                              "  <li><b>Panel comparison:</b> Markers intersected with the 40k MolBreeding Marker Panel</li>",
                              "</ul>"
                            ))
                          )
                        ),
                        conditionalPanel(
                          condition = sprintf("input['%s'] == 'WGS_KO34'", ns("builtin_dataset")),
                          div(style = "border-left: 4px solid #6f42c1; padding-left: 12px; background:#f8f9fa; border-radius:4px; padding: 12px 12px 12px 16px; margin-bottom:8px;",
                            HTML(paste0(
                              "<h5><b>WGS + KO34</b></h5>",
                              "<p>Combined whole-genome sequencing dataset comprising <b>all 91 <em>Coffea arabica</em> accessions</b> ",
                              "(Hawaii + Brazil collections), together with the reference genotype <b>KO34</b>.</p>",
                              "<ul>",
                              "  <li><b>Samples:</b> 91 accessions + KO34 (92 total)</li>",
                              "  <li><b>Origin:</b> USDA Hawaii (USA) + EMBRAPA (Brazil)</li>",
                              "  <li><b>Sequencing:</b> Illumina WGS, ~25&times; coverage + KO34 HiFi</li>",
                              "  <li><b>Panel comparison:</b> Markers intersected with the 40k MolBreeding Marker Panel</li>",
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
                                       label = HTML("Max % CNV \u2260 2 <small style='color:grey;font-weight:normal'>(lower = fewer CNV-affected genotypes allowed)</small>"),
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
                 )
               ),
               # --- Venn Diagrams ---
               box(
                 title = "Venn Diagrams", status = "info", solidHeader = FALSE,
                 icon = icon("circle-nodes"),
                 width = 12, collapsible = TRUE, collapsed = FALSE,
                 fluidRow(
                   column(width = 6,
                          h5("Matching Positions"),
                          plotOutput(ns("venn_positions"), height = "350px")
                   ),
                   column(width = 6,
                          h5("Matching REF & ALT Alleles"),
                          plotOutput(ns("venn_ref_alt"), height = "350px")
                   )
                 ),
                 fluidRow(
                   column(width = 12,
                          div(style = "float:left",
                              dropdownButton(
                                tags$h3("Save Venn Diagrams"),
                                selectInput(ns("venn_image_type"), "File Type", choices = c("jpeg","tiff","png","svg"), selected = "jpeg"),
                                sliderInput(ns("venn_image_res"), "Resolution", value = 300, min = 50, max = 1000, step = 50),
                                sliderInput(ns("venn_image_width"), "Width", value = 10, min = 1, max = 20, step = 0.5),
                                sliderInput(ns("venn_image_height"), "Height", value = 5, min = 1, max = 20, step = 0.5),
                                downloadButton(ns("download_venn"), "Save Image"),
                                circle = FALSE, status = "danger",
                                icon = icon("floppy-disk"), width = "300px", label = "Save",
                                tooltip = tooltipOptions(title = "Save Venn diagrams")
                              )
                          )
                   )
                 )
               ),
               # --- Marker Distribution ---
               box(
                 title = "Marker Distribution", status = "info", solidHeader = FALSE,
                 icon = icon("chart-bar"),
                 width = 12, collapsible = TRUE, collapsed = TRUE,
                 fluidRow(
                   column(width = 12,
                          plotly::plotlyOutput(ns("marker_distribution_plot"), height = "400px")
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
                          DT::DTOutput(ns("selected_markers_table"))
                   )
                 ),
                 fluidRow(
                   column(width = 12,
                          div(style = "float:left; margin-top:8px",
                              downloadButton(ns("download_selected_markers"), "Save Table",
                                             icon = icon("floppy-disk"))
                          )
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
    snp_stats    = NULL
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
  
  
  # --- Depth slider: update range from data ---
  output$ui_filter_depth <- renderUI({
    depth_col <- NULL
    if (!is.null(mk_select_items$snp_stats)) {
      candidates <- grep("depth|DP|mean_dp", colnames(mk_select_items$snp_stats),
                         ignore.case = TRUE, value = TRUE)
      if (length(candidates) > 0) depth_col <- mk_select_items$snp_stats[[candidates[1]]]
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

  # Sample lists for built-in datasets (replace with real sample names when available)
  builtin_samples <- list(
    WGS_hawaii_KO34 = c("KO34"),  # TODO: populate with actual Hawaii sample IDs
    WGS_Brazil_KO34 = c("KO34"),  # TODO: populate with actual Brazil sample IDs
    WGS_KO34        = c("KO34")   # TODO: populate with combined sample IDs
  )

  observeEvent(input$builtin_dataset, {
    req(input$builtin_dataset != "")
    samples <- builtin_samples[[input$builtin_dataset]]
    shinyWidgets::updatePickerInput(
      session, "filter_samples",
      choices  = samples,
      selected = samples
    )
  })

  observeEvent(mk_select_items$het_df, {
    req(!is.null(mk_select_items$het_df))
    # sample names may be in rownames or the first column
    samples <- if (!is.null(rownames(mk_select_items$het_df)) &&
                   !all(rownames(mk_select_items$het_df) == as.character(seq_len(nrow(mk_select_items$het_df))))) {
      rownames(mk_select_items$het_df)
    } else {
      as.character(mk_select_items$het_df[[1]])
    }
    shinyWidgets::updatePickerInput(
      session, "filter_samples",
      choices  = samples,
      selected = samples
    )
  })


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
  
  ##Summary Info
  mk_select_summary_info <- function() {
    # Handle possible NULL values for inputs
    dosage_file_name <- if (!is.null(input$mk_select_file$name)) input$mk_select_file$name else "No file selected"
    selected_ploidy <- if (!is.null(input$mk_select_ploidy)) as.character(input$mk_select_ploidy) else "Not selected"
    
    # Print the summary information
    cat(
      "BIGapp Summary Metrics Summary\n",
      "\n",
      paste0("Date: ", Sys.Date()), "\n",
      paste(R.Version()$version.string), "\n",
      "\n",
      "### Input Files ###\n",
      "\n",
      paste("Input Genotype File:", dosage_file_name), "\n",
      "\n",
      "### User Selected Parameters ###\n",
      "\n",
      paste("Selected Ploidy:", selected_ploidy), "\n",
      "\n",
      "### R Packages Used ###\n",
      "\n",
      paste("BIGapp:", packageVersion("BIGapp")), "\n",
      paste("BIGr:", packageVersion("BIGr")), "\n",
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
