tab5UI <- function(id) {
  ns <- NS(id)  # This creates the namespace function

  tagList(
    h2("Generate Your Report"),
    p("Customize your report settings below:"),

    card(
      card_header("Report Settings"),
      card_body(
        # Report Title Input
        textInput(ns("report_title"), "Report Title", value = ""),

        # Authors List Input (comma separated list)
        textInput(ns("authors_list"), "Authors List (separate by commas)",
                  value = ""),

        # Multiple Selection for Plans to include
        selectInput(
          ns("plans"),
          "Select Plans to include in report:",
          choices = c("Plan A", "Plan B", "Plan C", "Plan D"),
          selected = NULL,
          multiple = TRUE
        ),

        # Report Format: PDF or Word (PDF is default)
        radioButtons(
          ns("report_format"),
          "Select Report Format",
          choices = c("PDF" = "pdf", "Word" = "word"),
          selected = "word"
        ),

        br(),
        downloadButton(ns("download_report"), "Download Report")
      )
    )
  )
}
