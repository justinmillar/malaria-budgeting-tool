tab5Server <- function(input, output, session) {
  ns <- session$ns

  output$download_report <- downloadHandler(
    filename = function() {
      if (input$report_format == "pdf") {
        paste0("report-", Sys.Date(), ".pdf")
      } else {
        paste0("report-", Sys.Date(), ".docx")
      }
    },
    content = function(file) {
      # Define the path to the R Markdown template
      template_path <- file.path("global", "report-template.Rmd")

      if (!file.exists(template_path)) {
        stop("Template file not found: ", template_path)
      }

      # Copy the template to a temporary file location
      tempReport <- file.path(tempdir(), "report_template.Rmd")
      file.copy(template_path, tempReport, overwrite = TRUE)

      # Determine the output format based on the selection
      output_format <- if (input$report_format == "pdf") {
        "pdf_document"
      } else {
        "word_document"
      }

      # Render the report using rmarkdown::render with passed parameters
      rmarkdown::render(
        tempReport,
        output_format = output_format,
        output_file = file,
        params = list(
          report_title = input$report_title,
          authors_list = input$authors_list,
          plans = input$plans
        ),
        envir = new.env(parent = globalenv())
      )
    }
  )
}
