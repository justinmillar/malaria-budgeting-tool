tab3UI <- function(id) {
  ns <- NS(id)
  fluidPage(
    titlePanel("Plan Visualization"),
    h5("This tab provides key summaries of the generated budgets including tables and figures along with summary statistics that can be downloaded."),
    br(),
    # User input selections using a card for grouping
    card(
      card_header("User Inputs"),
      card_body_fill(
        layout_column_wrap(
          width = 1/4, # Wrap items into four columns
          selectInput(
            ns("plan_select"),
            "Select the Plan:",
            choices = c("", "Plan 1", "Plan 2", "Plan 3"),
            selected = ""
          ),
          selectInput(
            ns("spatial_scale"),
            "Select Spatial Scale:",
            choices = c("", "National", "State", "LGA"),
            selected = ""
          ),
          selectInput(
            ns("year_select"),
            "Select Years of Interest:",
            choices = c("", "All Years", "2026", "2027", "2028"),
            selected = ""
          ),
          selectInput(
            ns("currency_select"),
            "Select Currency:",
            choices = c("", "USD", "Naira"),
            selected = ""
          )
        ),
        br(),
        actionButton(
          ns("clear_inputs"),
          "Clear Selections",
          icon = icon("eraser"),
          class = "btn-danger"
        )
      )
    )
  )
}
