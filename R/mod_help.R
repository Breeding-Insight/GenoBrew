#' help UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList includeMarkdown
mod_help_ui <- function(id){
  ns <- NS(id)
  tagList(
    fluidPage(
      column(width=12),
      column(width=12,
             box(title="Select Markers", id = "select_markers_box",width = 12, collapsible = TRUE, collapsed = TRUE, status = "info", solidHeader = TRUE,
                 "Here you will find detailed description of the Select Markers module inputs and outputs. Please access the tutorial for a step-by-step guide:",
                 tags$a(href="https://cris-taniguti.shinyapps.io/genobrew/", "GenoBrew Tutorial"),
                 br(), br(),
                 bs4Dash::tabsetPanel(id = "Select_markers_tabset",
                                      tabPanel("Parameters description", value = "Select_markers_par", br(),
                                               includeMarkdown(system.file("help_files/GenoBrew_select_markers_par.Rmd", package = "GenoBrew"))
                                      ),
                                      tabPanel("Results description", value = "Select_markers_results", br(),
                                               includeMarkdown(system.file("help_files/GenoBrew_select_markers_res.Rmd", package = "GenoBrew"))
                                      ),
                                      tabPanel("How to cite", value = "Select_markers_cite", br(),
                                               includeMarkdown(system.file("help_files/GenoBrew_select_markers_cite.Rmd", package = "GenoBrew"))
                                      ))
                 
             ),
             box(title="CNV Profiles", id = "cnv_profiles_box", width = 12, collapsible = TRUE, collapsed = TRUE, status = "info", solidHeader = TRUE,
                 "Here you will find detailed description of the CNV Profiles module inputs and outputs. Please access the tutorial for a step-by-step guide:",
                 tags$a(href="https://cris-taniguti.shinyapps.io/genobrew/", "GenoBrew Tutorial"),
                 br(), br(),
                 bs4Dash::tabsetPanel(id = "CNV_profiles_tabset",
                                      tabPanel("Parameters description", value = "CNV_profiles_par", br(),
                                               includeMarkdown(system.file("help_files/GenoBrew_CNV_profile_par.Rmd", package = "GenoBrew"))
                                      ),
                                      tabPanel("Results description", value = "CNV_profiles_results", br(),
                                               includeMarkdown(system.file("help_files/GenoBrew_CNV_profile_res.Rmd", package = "GenoBrew"))
                                      ),
                                      tabPanel("How to cite", value = "CNV_profiles_cite", br(),
                                               includeMarkdown(system.file("help_files/GenoBrew_CNV_profile_cite.Rmd", package = "GenoBrew"))
                                      ))
             )
      )
    ),
    column(width=2)
    # Add Help content here
  )
}

#' help Server Functions
#'
#' @noRd
mod_help_server <- function(input, output, session, parent_session){
  
  ns <- session$ns
  
}

## To be copied in the UI
# mod_help_ui("help_1")

## To be copied in the server
# mod_help_server("help_1")
