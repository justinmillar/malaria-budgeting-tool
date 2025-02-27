tab1aUI <- function(id) {
  ns <- NS(id)
  fluidPage(

    # Add the useShinyjs() function
    useShinyjs(),
    tags$head(
      # Add the resetScroll() function - helps with scrolling after upload
      tags$script("
        function resetScroll() {
          window.scrollTo(0, 0);
        }
      ")
    ),
  
    titlePanel("Data Download and Upload"),
    # h3("Content for Tab 1a goes here"),
    page_sidebar(
      # title = "Malaria Control Program Planning Tool",
      sidebar = sidebar(
        width = "400px",
        card(
          "Instructions",
          p("This tool helps you manage both Scenario and Cost templates:"),
          tags$ul(
            tags$li("Scenario templates: Multiple sheets (by year) for intervention plans"),
            tags$li("Cost templates: Single sheet for unit cost information")
          ),
          p("To get started with either template:"),
          tags$ul(
            tags$li("Download an empty template to create a new file, or"),
            tags$li("Download an existing file from the tables below")
          )
        ),
        
        # Scenario Template Section
        card(
          "Scenario Template",
          selectInput(ns("year_filter"), "Select years of interest:",
                     choices = DEFAULT_YEARS,
                     selected = 2025:2027,
                     multiple = TRUE),
          downloadButton(ns("download_scenario_template"), "Download Empty Scenario Template"),
          uiOutput(ns("scenario_download_ui")),
          hr(),
          fileInput(ns("scenario_file"), "Upload Scenario File",
                   accept = c(".xlsx", ".xls")),
          textInput(ns("scenario_name"), "Scenario Name",
                   placeholder = "Give this scenario a name"),
          textAreaInput(ns("scenario_description"), "Description",
                       placeholder = "Add a description (optional)"),
          actionButton(ns("submit_scenario"), "Submit Scenario", class = "btn-primary")
        ),
        
        # Cost Template Section
        card(
          "Cost Template",
          downloadButton(ns("download_cost_template"), "Download Empty Cost Template"),
          uiOutput(ns("cost_download_ui")),
          hr(),
          fileInput(ns("cost_file"), "Upload Cost File",
                   accept = c(".xlsx", ".xls")),
          textInput(ns("cost_name"), "Cost Sheet Name",
                   placeholder = "Give this cost sheet a name"),
          textAreaInput(ns("cost_description"), "Description",
                       placeholder = "Add a description (optional)"),
          actionButton(ns("submit_cost"), "Submit Cost Sheet", class = "btn-primary")
        )
      ),
      
      # Main panel content - stacked layout
      card(
        card_header("Previous Scenario Uploads"),
        DTOutput(ns("scenario_uploads_table")),
        height = "275px"
      ),
      card(
        card_header("Previous Cost Uploads"),
        DTOutput(ns("cost_uploads_table")),
        height = "275px"
      )
    )
  )
}
