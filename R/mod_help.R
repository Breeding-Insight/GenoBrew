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
                   "Under development. Please access the tutorial for more information:"                   
               ),
                box(title="CNV Profiles", id = "cnv_profiles_box", width = 12, collapsible = TRUE, collapsed = TRUE, status = "success", solidHeader = TRUE,
                    "Under development. Please access the tutorial for more information:"
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
