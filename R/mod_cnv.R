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
mod_cnv_ui <- function(id){
  ns <- NS(id)
  tagList(
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
             box(title = "Inputs", width = 12, collapsible = TRUE, collapsed = FALSE, status = "info", solidHeader = TRUE,
                 # --- Upload (left) | OR divider | Built-in (right) ---
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
                            fileInput(ns("cnv_file"), "Upload CNV File", accept = c(".csv", ".txt", ".tsv", ".gz"), width = "80%")
                     ),
                     column(width = 1),
                     column(width = 6,
                            tags$label("Choose a built-in dataset"),
                            selectInput(
                              ns("cnv_builtin_dataset"),
                              label   = NULL,
                              choices = c("-- Select a dataset --" = "",
                                          "WGS Hawaii + KO34" = "WGS_hawaii_KO34",
                                          "WGS Brazil + KO34" = "WGS_Brazil_KO34",
                                          "WGS + KO34"        = "WGS_KO34"),
                              selected = "", width = "80%"
                            )
                     )
                   ),
                   # Row 2: marker panel
                   fluidRow(
                     column(width = 5,
                            fileInput(ns("cnv_panel_file"), "Upload Marker Panel CSV", accept = c(".csv", ".txt", ".tsv", ".gz"), width = "80%")
                     ),
                     column(width = 1),
                     column(width = 6,
                            tags$label("Choose a built-in marker panel"),
                            selectInput(
                              ns("cnv_builtin_panel"),
                              label   = NULL,
                              choices = c("-- Select a panel --" = "",
                                          "40k MolBreeding Panel" = "panel_40k_molbreeding"),
                              selected = "", width = "80%"
                            )
                     )
                   ),
                   # Row 3: buttons
                   fluidRow(
                     column(width = 5,
                            actionButton(ns("cnv_load_file"), "Load File", icon = icon("upload"),
                                         class = "btn-info")
                     ),
                     column(width = 1),
                     column(width = 6,
                            actionButton(ns("cnv_load_dataset"), "Load Dataset", icon = icon("database"),
                                         class = "btn-success")
                     )
                   )
                 ),
                 # --- Progress bar ---
                 fluidRow(
                   column(width = 12,
                          shinyWidgets::progressBar(id = ns("pb_cnv"), value = 0,
                                                    title = "", display_pct = FALSE,
                                                    status = "info", striped = TRUE)
                   )
                 ), hr(),
                 # --- Dataset info panels ---
                 fluidRow(
                   column(width = 12,
                          conditionalPanel(
                            condition = sprintf("input['%s'] == 'WGS_hawaii_KO34'", ns("cnv_builtin_dataset")),
                            div(style = "border-left: 4px solid #17a2b8; background:#f8f9fa; border-radius:4px; padding: 12px 12px 12px 16px; margin-bottom:8px;",
                                HTML(paste0(
                                  "<h5><b>WGS Hawaii + KO34</b></h5>",
                                  "<p>CNV profiles derived from whole-genome sequencing of <b>69 <em>Coffea arabica</em> accessions</b> ",
                                  "from the <b>University of Hawaii germplasm collection</b>, together with the reference genotype <b>KO34</b>.</p>",
                                  "<ul>",
                                  "  <li><b>Samples:</b> 69 WGS accessions + KO34 (70 total)</li>",
                                  "  <li><b>Origin:</b> USDA - Hawaii, USA</li>",
                                  "  <li><b>Sequencing:</b> Illumina WGS, ~25&times; coverage + KO34 HiFi</li>",
                                  "  <li><b>Reference:</b> <em>C. arabica</em> Red Bourbon (Scalabrin et al., 2024)</li>",
                                  "</ul>"
                                ))
                            )
                          ),
                          conditionalPanel(
                            condition = sprintf("input['%s'] == 'WGS_Brazil_KO34'", ns("cnv_builtin_dataset")),
                            div(style = "border-left: 4px solid #28a745; background:#f8f9fa; border-radius:4px; padding: 12px 12px 12px 16px; margin-bottom:8px;",
                                HTML(paste0(
                                  "<h5><b>WGS Brazil + KO34</b></h5>",
                                  "<p>CNV profiles derived from whole-genome sequencing of <b>22 <em>Coffea arabica</em> accessions</b> ",
                                  "from the <b>Brazilian germplasm collection (Embrapa)</b>, together with the reference genotype <b>KO34</b>.</p>",
                                  "<ul>",
                                  "  <li><b>Samples:</b> 22 accessions + KO34 (23 total)</li>",
                                  "  <li><b>Origin:</b> EMBRAPA - Brazil</li>",
                                  "  <li><b>Sequencing:</b> Illumina WGS, ~25&times; coverage + KO34 HiFi</li>",
                                  "  <li><b>Reference:</b> <em>C. arabica</em> Red Bourbon (Scalabrin et al., 2024)</li>",
                                  "</ul>"
                                ))
                            )
                          ),
                          conditionalPanel(
                            condition = sprintf("input['%s'] == 'WGS_KO34'", ns("cnv_builtin_dataset")),
                            div(style = "border-left: 4px solid #6f42c1; background:#f8f9fa; border-radius:4px; padding: 12px 12px 12px 16px; margin-bottom:8px;",
                                HTML(paste0(
                                  "<h5><b>WGS + KO34</b></h5>",
                                  "<p>CNV profiles from the combined dataset of <b>all 91 <em>Coffea arabica</em> accessions</b> ",
                                  "(Hawaii + Brazil), together with the reference genotype <b>KO34</b>.</p>",
                                  "<ul>",
                                  "  <li><b>Samples:</b> 91 accessions + KO34 (92 total)</li>",
                                  "  <li><b>Origin:</b> USDA Hawaii (USA) + EMBRAPA (Brazil)</li>",
                                  "  <li><b>Sequencing:</b> Illumina WGS, ~25&times; coverage + KO34 HiFi</li>",
                                  "  <li><b>Reference:</b> <em>C. arabica</em> Red Bourbon (Scalabrin et al., 2024)</li>",
                                  "</ul>"
                                ))
                            )
                          )
                   )
                 ),
                 fluidRow(
                   column(width = 12,
                          div(style = "display:inline-block; float:right",
                              dropdownButton(
                                HTML("<b>Input files</b>"),
                                p(downloadButton(ns("download_cnv_example"), ""), "CNV Example File"),
                                p(HTML("<b>Parameters description:</b>"), actionButton(ns("goPar"), icon("arrow-up-right-from-square", verify_fa = FALSE))), hr(),
                                p(HTML("<b>Results description:</b>"),     actionButton(ns("goRes"), icon("arrow-up-right-from-square", verify_fa = FALSE))), hr(),
                                p(HTML("<b>How to cite:</b>"),             actionButton(ns("goCite"), icon("arrow-up-right-from-square", verify_fa = FALSE))), hr(),
                                actionButton(ns("cnv_summary"), "Summary"),
                                circle = FALSE,
                                status = "warning",
                                icon = icon("info"), width = "300px",
                                tooltip = tooltipOptions(title = "Click to see info!")
                              )
                          )
                   )
                 )
             )  # end box
      ),
      # --- Select Samples box ---
      column(width = 12,
             box(
               title = "Select Samples", status = "info", solidHeader = TRUE,
               icon = icon("users"), width = 12, collapsible = TRUE, collapsed = FALSE,
               tabsetPanel(
                 id = ns("sample_select_tabs"),
                 type = "tabs",
                 tabPanel(
                   title = tagList(icon("sitemap"), " By Family"),
                   br(),
                   fluidRow(
                     column(width = 6,
                            shinyWidgets::pickerInput(
                              ns("cnv_filter_families"),
                              label   = tags$label("Select families", style = "font-weight:bold"),
                              choices  = NULL,
                              selected = NULL,
                              multiple = TRUE,
                              options  = shinyWidgets::pickerOptions(
                                actionsBox            = TRUE,
                                liveSearch            = TRUE,
                                liveSearchPlaceholder = "Search families...",
                                selectedTextFormat    = "count > 3",
                                countSelectedText     = "{0} of {1} families selected",
                                noneSelectedText      = "No families selected",
                                selectAllText         = "Select All",
                                deselectAllText       = "Deselect All",
                                size = 12
                              ),
                              width = "100%"
                            ),
                            helpText("Selecting a family will include all samples belonging to it.")
                     ),
                     column(width = 6,
                            tags$label("Samples in selected families", style = "font-weight:bold"),
                            verbatimTextOutput(ns("cnv_family_sample_preview"))
                     )
                   )
                 ),
                                  tabPanel(
                   title = tagList(icon("user"), " By Sample"),
                   br(),
                   fluidRow(
                     column(width = 12,
                            shinyWidgets::pickerInput(
                              ns("cnv_filter_samples"),
                              label   = NULL,
                              choices = NULL,
                              selected = NULL,
                              multiple = TRUE,
                              options  = shinyWidgets::pickerOptions(
                                actionsBox            = TRUE,
                                liveSearch            = TRUE,
                                liveSearchPlaceholder = "Search samples...",
                                selectedTextFormat    = "count > 3",
                                countSelectedText     = "{0} of {1} samples selected",
                                noneSelectedText      = "No samples selected",
                                selectAllText         = "Select All",
                                deselectAllText       = "Deselect All",
                                size = 12
                              ),
                              width = "100%"
                            ),
                            helpText("All samples are selected by default. Deselect to exclude from plots.")
                     )
                   )
                 )
               )
             )
      )
    )
  )
}

#' cnv Server Functions
#'
#' @importFrom graphics axis hist points
#' @import ggplot2
#' @importFrom scales comma_format
#'
#' @noRd
mod_cnv_server <- function(input, output, session, parent_session){
  
  ns <- session$ns

  # --- Reactive storage for dataset sample/family metadata ---
  cnv_meta <- reactiveValues(
    samples  = character(0),
    families = character(0),
    sample_family_map = data.frame()   # data.frame with columns: Sample, Family
  )

  # Built-in dataset metadata (TODO: populate with real sample/family lists)
  builtin_meta <- list(
    WGS_hawaii_KO34 = list(
      samples  = c("KO34"),            # TODO: replace with actual Hawaii sample IDs
      families = c("Unknown")          # TODO: replace with actual family names
    ),
    WGS_Brazil_KO34 = list(
      samples  = c("KO34"),            # TODO: replace with actual Brazil sample IDs
      families = c("Unknown")
    ),
    WGS_KO34 = list(
      samples  = c("KO34"),            # TODO: replace with combined sample IDs
      families = c("Unknown")
    )
  )

  # Helper to update both pickers at once
  update_sample_pickers <- function(samples, families) {
    shinyWidgets::updatePickerInput(session, "cnv_filter_samples",
                                    choices = samples, selected = samples)
    shinyWidgets::updatePickerInput(session, "cnv_filter_families",
                                    choices = families, selected = families)
  }

  # Update pickers when built-in dataset is chosen
  observeEvent(input$cnv_builtin_dataset, {
    req(input$cnv_builtin_dataset != "")
    meta <- builtin_meta[[input$cnv_builtin_dataset]]
    cnv_meta$samples  <- meta$samples
    cnv_meta$families <- meta$families
    update_sample_pickers(meta$samples, meta$families)
  })

  # Update pickers when a file is loaded (triggered after cnv_items$het_df is populated)
  # NOTE: observer defined after cnv_items below

  # Preview samples belonging to selected families
  output$cnv_family_sample_preview <- renderText({
    req(length(input$cnv_filter_families) > 0)
    if (nrow(cnv_meta$sample_family_map) == 0) {
      return("Family-sample mapping not yet available for this dataset.")
    }
    sel <- cnv_meta$sample_family_map[
      cnv_meta$sample_family_map$Family %in% input$cnv_filter_families, "Sample"
    ]
    if (length(sel) == 0) return("No samples found for selected families.")
    paste(sel, collapse = "\n")
  })

  # Help links
  observeEvent(input$goPar, {
    # change to help tab
    bs4Dash::updatebs4TabItems(session = parent_session, inputId = "MainMenu",
                      selected = "help")
    
    # select specific tab
    updateTabsetPanel(session = parent_session, inputId = "Genomic_cnv_tabset",
                      selected = "Genomic_cnv_par")
    # expand specific box
    updateBox(id = "Genomic_cnv_box", action = "toggle", session = parent_session)
  })
  
  observeEvent(input$goRes, {
    # change to help tab
    bs4Dash::updatebs4TabItems(session = parent_session, inputId = "MainMenu",
                      selected = "help")
    
    # select specific tab
    updateTabsetPanel(session = parent_session, inputId = "Genomic_cnv_tabset",
                      selected = "Genomic_cnv_results")
    # expand specific box
    updateBox(id = "Genomic_cnv_box", action = "toggle", session = parent_session)
  })
  
  observeEvent(input$goCite, {
    # change to help tab
    bs4Dash::updatebs4TabItems(session = parent_session, inputId = "MainMenu",
                      selected = "help")
    
    # select specific tab
    updateTabsetPanel(session = parent_session, inputId = "Genomic_cnv_tabset",
                      selected = "Genomic_cnv_cite")
    # expand specific box
    updateBox(id = "Genomic_cnv_box", action = "toggle", session = parent_session)
  })
  
  ##UI text
  # NOTE: dosage_text references cnv_items; defined after cnv_items below

  #Genomic cnv analysis
  
  #Genomic cnv output files
  cnv_items <- reactiveValues(
    cnv_df = NULL,
    dosage_df = NULL,
    het_df = NULL,
    maf_df = NULL,
    pos_df = NULL,
    markerPlot = NULL,
    snp_stats = NULL
  )

  # Update pickers when a file is loaded (triggered after cnv_items$het_df is populated)
  observeEvent(cnv_items$het_df, {
    req(!is.null(cnv_items$het_df))
    samples <- if (!is.null(rownames(cnv_items$het_df)) &&
                   !all(rownames(cnv_items$het_df) == as.character(seq_len(nrow(cnv_items$het_df))))) {
      rownames(cnv_items$het_df)
    } else {
      as.character(cnv_items$het_df[[1]])
    }
    cnv_meta$samples <- samples
    shinyWidgets::updatePickerInput(session, "cnv_filter_samples",
                                    choices = samples, selected = samples)
    shinyWidgets::updatePickerInput(session, "cnv_filter_families",
                                    choices = character(0), selected = character(0))
  })

  ##UI text
  output$dosage_text <- renderUI({
    if (is.null(input$cnv_plot_tabs)) return(NULL)
    if (input$cnv_plot_tabs == "Dosage Plot" && !is.null(cnv_items$dosage_df)) {
      div(style = "color: grey; text-align: left; margin-top: 3px;",
          "Note: 0 = homozygous reference")
    } else {
      NULL
    }
  })
  
  #Reactive boxes
  output$mean_het_box <- renderValueBox({
    valueBox(
      value = 0,
      subtitle = "Mean Heterozygosity",
      icon = icon("dna"),
      color = "info"
    )
  })
  
  output$mean_maf_box <- renderValueBox({
    valueBox(
      value = 0,
      subtitle = "Mean MAF",
      icon = icon("dna"),
      color = "info"
    )
  })
  
  observeEvent(input$cnv_start, {
    toggleClass(id = "cnv_ploidy", class = "borderred", condition = (is.na(input$cnv_ploidy) | is.null(input$cnv_ploidy)))
    #toggleClass(id = "zero_value", class = "borderred", condition = (is.na(input$zero_value) | is.null(input$zero_value)))
    
    if (is.null(input$cnv_file$datapath)) {
      shinyalert(
        title = "Missing input!",
        text = "Upload VCF File",
        size = "s",
        closeOnEsc = TRUE,
        closeOnClickOutside = FALSE,
        html = TRUE,
        type = "error",
        showConfirmButton = TRUE,
        confirmButtonText = "OK",
        confirmButtonCol = "#004192",
        showCancelButton = FALSE,
        animation = TRUE
      )
    }
    req(input$cnv_file, input$cnv_ploidy)
    
    #Input variables (need to add support for VCF file)
    ploidy <- as.numeric(input$cnv_ploidy)
    geno <- input$cnv_file$datapath
    
    #Status
    updateProgressBar(session = session, id = "pb_cnv", value = 20, title = "Importing VCF")
    
    #Import genotype information if in VCF format
    #### VCF sanity check
    checks <- vcf_sanity_check(geno)
    
    error_if_false <- c(
      "VCF_header", "VCF_columns", "unique_FORMAT", "GT",
      "samples", "chrom_info", "pos_info", "VCF_compressed"
    )
    
    error_if_true <- c(
      "multiallelics", "phased_GT",  "mixed_ploidies",
      "duplicated_samples", "duplicated_markers"
    )
    
    warning_if_false <- c("ref_alt","max_markers")
    
    checks_result <- vcf_sanity_messages(checks, 
                                         error_if_false, 
                                         error_if_true, 
                                         warning_if_false = NULL, 
                                         warning_if_true = NULL,
                                         input_ploidy = ploidy)
    
    if(checks_result) return() # Stop the analysis if checks fail
    #########
    
    vcf <- read.vcfR(geno, verbose = FALSE)
    
    #Save position information
    cnv_items$pos_df <- data.frame(vcf@fix[, 1:2])
    
    #Get items in FORMAT column
    info <- vcf@gt[1,"FORMAT"] #Getting the first row FORMAT
    
    # Apply the function to the first INFO string
    info_ids <- extract_info_ids(info[1])
    
    #Status
    updateProgressBar(session = session, id = "pb_cnv", value = 40, title = "Converting to Numeric")
    
    #Get the genotype values and convert to numeric format
    #Extract GT and convert to numeric calls
    geno_mat <- extract.gt(vcf, element = "GT")
    geno_mat <- apply(geno_mat, 2, convert_to_dosage)
    rm(vcf) #Remove VCF
    
    #print(class(geno_mat))
    #Convert genotypes to alternate counts if they are the reference allele counts
    #Importantly, the dosage plot is based on the input format NOT the converted genotypes
    is_reference <- FALSE #(input$zero_value == "Reference Allele Counts")
    
    #print("Genotype file successfully imported")
    ######Get MAF plot (Need to remember that the VCF genotypes are likely set as 0 = homozygous reference, where the dosage report is 0 = homozygous alternate)
    
    #print("Starting percentage calc")
    #Status
    updateProgressBar(session = session, id = "pb_cnv", value = 70, title = "Calculating...")
    # Calculate percentages for both genotype matrices
    percentages1 <- calculate_percentages(geno_mat, ploidy)
    # Combine the data matrices into a single data frame
    percentages1_df <- as.data.frame(t(percentages1))
    percentages1_df$Data <- "Dosages"
    # Assuming my_data is your dataframe
    #print("Percentage Complete: melting dataframe")
    melted_data <- percentages1_df %>%
      pivot_longer(cols = -(Data),names_to = "Dosage", values_to = "Percentage")
    
    cnv_items$dosage_df <- melted_data
    
    print("Dosage calculations worked")
    
    #Convert the genotype calls prior to het,af, and maf calculation
    geno_mat <- data.frame(convert_genotype_counts(df = geno_mat, ploidy = ploidy, is_reference),
                           check.names = FALSE)
    
    # Calculating heterozygosity
    cnv_items$het_df <- calculate_heterozygosity(geno_mat, ploidy = ploidy)
    
    #print("Heterozygosity success")
    cnv_items$maf_df <- calculateMAF(geno_mat, ploidy = ploidy)
    cnv_items$maf_df <- cnv_items$maf_df[, c(1,3)]
    
    #Calculate PIC
    calc_allele_frequencies <- function(d_diplo_t, ploidy) {
      allele_frequencies <- apply(d_diplo_t, 1, function(x) {
        count_sum <- sum(!is.na(x))
        allele_sum <- sum(x, na.rm = TRUE)
        if (count_sum != 0) {allele_sum / (ploidy * count_sum)} else {NA}
      })
      
      all_allele_frequencies <- data.frame(SNP = rownames(d_diplo_t), p1= allele_frequencies, p2= 1-allele_frequencies)
      return(all_allele_frequencies)
    }
    Fre <-calc_allele_frequencies(geno_mat,as.numeric(ploidy))
    calc_pic <- function(x) {
      freq_squared <- x^2
      outer_matrix <- outer(freq_squared, freq_squared)
      upper_tri_sum <- sum(outer_matrix[upper.tri(outer_matrix)])
      pic <- 1 - sum(freq_squared) - 2*upper_tri_sum
      return(pic)
    }
    
    print(Fre[1:5,])
    
    PIC_results <- apply(Fre[, c("p1", "p2")], 1, calc_pic)
    PIC_df <- data.frame(SNP_ID = Fre$SNP, PIC = PIC_results)
    rownames(PIC_df) <- NULL
    
    print(PIC_df[1:5,])
    print(cnv_items$maf_df[1:5,])
    
    cnv_items$snp_stats <- (merge(cnv_items$maf_df, PIC_df, by = "SNP_ID", all = TRUE))[,c("SNP_ID","MAF","PIC")]
    colnames(cnv_items$snp_stats)[1] <- "SNP"
    
    #Updating value boxes
    output$mean_het_box <- renderValueBox({
      valueBox(
        value = round(mean(cnv_items$het_df$Ho),3),
        subtitle = "Mean Heterozygosity",
        icon = icon("dna"),
        color = "info"
      )
    })
    output$mean_maf_box <- renderValueBox({
      valueBox(
        value = round(mean(cnv_items$maf_df$MAF),3),
        subtitle = "Mean MAF",
        icon = icon("dna"),
        color = "info"
      )
    })
    
    #Status
    updateProgressBar(session = session, id = "pb_cnv", value = 100, title = "Complete!")
  })
  
  box_plot <- reactive({
    validate(
      need(!is.null(cnv_items$dosage_df), "Input VCF, define parameters and click `run analysis` to access results in this session.")
    )
    
    #Plotting
    box <- ggplot(cnv_items$dosage_df, aes(x=Dosage, y=Percentage, fill=Data)) +
      #geom_point(aes(color = Data), position = position_dodge(width = 0.8), width = 0.2, alpha = 0.5) +  # Add jittered points
      geom_boxplot(position = position_dodge(width = 0.8), alpha = 0.9) +
      labs(x = "\nDosage", y = "Percentage\n", title = "Genotype Distribution by Sample") +
      theme_bw() +
      theme(
        axis.text = element_text(size = 14),
        axis.title = element_text(size = 14)
      )
    
    box
  })
  
  output$dosage_plot <- renderPlot({
    box_plot()
  })
  
  output$het_plot <- renderPlot({
    validate(
      need(!is.null(cnv_items$het_df) & !is.null(input$hist_bins), "Input VCF, define parameters and click `run analysis` to access results in this session.")
    )
    hist(cnv_items$het_df$Ho, breaks = as.numeric(input$hist_bins), col = "tan3", border = "black", xlim= c(0,1),
         xlab = "Observed Heterozygosity",
         ylab = "Number of Samples",
         main = "Sample Observed Heterozygosity")
    axis(1, at = seq(0, 1, by = 0.1), labels = TRUE)
  })
  
  #Marker plot
  marker_plot <- reactive({
    validate(
      need(!is.null(cnv_items$pos_df), "Input VCF, define parameters and click `run analysis` to access results in this session.")
    )
    #Order the Chr column
    cnv_items$pos_df$POS <- as.numeric(cnv_items$pos_df$POS)
    # Sort the dataframe and pad with a 0 if only a single digit is provided
    cnv_items$pos_df$CHROM <- ifelse(
      nchar(cnv_items$pos_df$CHROM) == 1,
      paste0("0", cnv_items$pos_df$CHROM),
      cnv_items$pos_df$CHROM
    )
    cnv_items$pos_df <- cnv_items$pos_df[order(cnv_items$pos_df$CHROM), ]
    
    #Plot
    
    # Create custom breaks for the x-axis labels (every 13Mb)
    x_breaks <- seq(0, max(cnv_items$pos_df$POS), by = (max(cnv_items$pos_df$POS)/5))
    x_breaks <- c(x_breaks, max(cnv_items$pos_df$POS))  # Add 114Mb as a custom break
    
    # Create custom labels for the x-axis using the 'Mb' suffix
    x_labels <- comma_format()(x_breaks / 1000000)
    x_labels <- paste0(x_labels, "Mb")
    
    suppressWarnings({
      markerPlot <- ggplot(cnv_items$pos_df, aes(x = as.numeric(POS), y = CHROM, group = as.factor(CHROM))) +
        geom_point(aes(color = as.factor(CHROM)), shape = 108, size = 5, show.legend = FALSE) +
        xlab("Position") +
        #ylab("Markers\n") +
        theme(axis.text = element_text(size = 11, color = "black"),
              axis.text.x.top = element_text(size = 11, color = "black"),
              axis.title = element_blank(),
              panel.grid = element_blank(),
              axis.ticks.length.x = unit(-0.15, "cm"),
              axis.ticks.margin = unit(0.1, "cm"),
              axis.ticks.y = element_blank(),
              axis.line.x.top = element_line(color="black"),
              panel.background = element_rect(fill="white"),
              plot.margin = margin(10, 25, 10, 10)
        ) +
        scale_x_continuous(
          breaks = x_breaks,     # Set custom breaks for x-axis labels
          labels = x_labels,     # Set custom labels with "Mb" suffixes
          position = "top",       # Move x-axis labels and ticks to the top
          expand = c(0,0),
          limits = c(0,max(cnv_items$pos_df$POS))
        )
    })
    #Display plot
    markerPlot
  })
  
  output$marker_plot <- renderPlot({
    marker_plot()
  })
  
  output$maf_plot <- renderPlot({
    validate(
      need(!is.null(cnv_items$maf_df) & !is.null(input$hist_bins), "Input VCF, define parameters and click `run analysis` to access results in this session.")
    )
    
    hist(cnv_items$maf_df$MAF, breaks = as.numeric(input$hist_bins), col = "grey", border = "black", xlab = "Minor Allele Frequency (MAF)",
         ylab = "Frequency", main = "Minor Allele Frequency Distribution")
  })
  
  sample_table <- reactive({
    validate(
      need(!is.null(cnv_items$het_df), "Input VCF, define parameters and click `run analysis` to access results in this session.")
    )
    tb <- cnv_items$het_df
    tb$Ho <- round(tb$Ho,4)
    tb
  })
  
  output$sample_table <- DT::renderDT({sample_table()}, options = list(scrollX = TRUE,autoWidth = FALSE, pageLength = 5))
  
  snp_table <- reactive({
    validate(
      need(!is.null(cnv_items$snp_stats), "Input VCF, define parameters and click `run analysis` to access results in this session.")
    )
    tb <- cnv_items$snp_stats
    tb$PIC <- round(tb$PIC,4)
    tb$MAF <- round(tb$MAF,4)
    tb
  })
  
  output$snp_table <- DT::renderDT({snp_table()}, options = list(scrollX = TRUE,autoWidth = FALSE, pageLength = 5))
  
  #Download Figures for cnv Tab (Need to convert figures to ggplot)
  output$download_div_figure <- downloadHandler(
    
    filename = function() {
      if (input$div_image_type == "jpeg") {
        paste("genomic-cnv-", Sys.Date(), ".jpg", sep="")
      } else if (input$div_image_type == "png") {
        paste("genomic-cnv-", Sys.Date(), ".png", sep="")
      } else if (input$div_image_type == "svg") {
        paste("genomic-cnv-", Sys.Date(), ".svg", sep="")
      } else {
        paste("genomic-cnv-", Sys.Date(), ".tiff", sep="")
      }
    },
    content = function(file) {
      req(input$div_figure)
      
      if (input$div_image_type == "jpeg") {
        jpeg(file, width = as.numeric(input$div_image_width), height = as.numeric(input$div_image_height), res= as.numeric(input$div_image_res), units = "in")
      } else if (input$div_image_type == "png") {
        png(file, width = as.numeric(input$div_image_width), height = as.numeric(input$div_image_height), res= as.numeric(input$div_image_res), units = "in")
      } else if (input$div_image_type == "svg") {
        svg(file, width = as.numeric(input$div_image_width), height = as.numeric(input$div_image_height))
      } else {
        tiff(file, width = as.numeric(input$div_image_width), height = as.numeric(input$div_image_height), res= as.numeric(input$div_image_res), units = "in")
      }
      
      # Conditional plotting based on input selection
      if (input$div_figure == "Dosage Plot") {
        print(box_plot())
      } else if (input$div_figure == "MAF Histogram") {
        hist(cnv_items$maf_df$MAF, breaks = as.numeric(input$hist_bins), col = "grey", border = "black", xlab = "Minor Allele Frequency (MAF)",
             ylab = "Frequency", main = "Minor Allele Frequency Distribution")
      } else if (input$div_figure == "OHet Histogram") {
        hist(cnv_items$het_df$Ho, breaks = as.numeric(input$hist_bins), col = "tan3", border = "black", xlim= c(0,1),
             xlab = "Observed Heterozygosity",
             ylab = "Number of Samples",
             main = "Sample Observed Heterozygosity")
        axis(1, at = seq(0, 1, by = 0.1), labels = TRUE)
      } else if (input$div_figure == "Marker Plot") {
        print(marker_plot())
      }
      
      dev.off()
    }
    
  )
  
  #Download files for Genotype cnv
  output$download_div_file <- downloadHandler(
    filename = function() {
      paste0("genomic-cnv-results-", Sys.Date(), ".zip")
    },
    content = function(file) {
      # Temporary files list
      temp_dir <- tempdir()
      temp_files <- c()
      
      if (!is.null(cnv_items$het_df)) {
        # Create a temporary file for assignments
        het_file <- file.path(temp_dir, paste0("Sample-statistics-", Sys.Date(), ".csv"))
        write.csv(cnv_items$het_df, het_file, row.names = FALSE)
        temp_files <- c(temp_files, het_file)
      }
      
      if (!is.null(cnv_items$snp_stats)) {
        # Create a temporary file for BIC data frame
        maf_file <- file.path(temp_dir, paste0("SNP-statistics-", Sys.Date(), ".csv"))
        write.csv(cnv_items$snp_stats, maf_file, row.names = FALSE)
        temp_files <- c(temp_files, maf_file)
      }
      
      # Zip files only if there's something to zip
      if (length(temp_files) > 0) {
        zip(file, files = temp_files, extras = "-j") # Using -j to junk paths
      }
      
      # Optionally clean up
      file.remove(temp_files)
    }
  )
  
  output$download_cnv_example <- downloadHandler(
    filename = function() {
      paste0("BIGapp_VCF_Example_file.vcf.gz")
    },
    content = function(file) {
      ex <- system.file("iris_DArT_VCF.vcf.gz", package = "BIGapp")
      file.copy(ex, file)
    })
  
  ##Summary Info
  cnv_summary_info <- function() {
    # Handle possible NULL values for inputs
    dosage_file_name <- if (!is.null(input$cnv_file$name)) input$cnv_file$name else "No file selected"
    selected_ploidy <- if (!is.null(input$cnv_ploidy)) as.character(input$cnv_ploidy) else "Not selected"
    
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
  observeEvent(input$cnv_summary, {
    showModal(modalDialog(
      title = "Summary Information",
      size = "l",
      easyClose = TRUE,
      footer = tagList(
        modalButton("Close"),
        downloadButton("download_cnv_info", "Download")
      ),
      pre(
        paste(capture.output(cnv_summary_info()), collapse = "\n")
      )
    ))
  })
  
  
  # Download Summary Info
  output$download_cnv_info <- downloadHandler(
    filename = function() {
      paste("cnv_summary_", Sys.Date(), ".txt", sep = "")
    },
    content = function(file) {
      # Write the summary info to a file
      writeLines(paste(capture.output(cnv_summary_info()), collapse = "\n"), file)
    }
  )
}

## To be copied in the UI
# mod_cnv_ui("cnv_1")

## To be copied in the server
# mod_cnv_server("cnv_1")
