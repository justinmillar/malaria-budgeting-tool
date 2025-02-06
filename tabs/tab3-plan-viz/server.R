# tab3Server <- function(input, output, session) {
#   observeEvent(input$clear_inputs, {
#     print("Clear button clicked")
#     # Remove the ns() wrapper - just use the input IDs directly
#     updateSelectInput(session, "plan_select", selected = "")
#     updateSelectInput(session, "spatial_scale", selected = "")
#     updateSelectInput(session, "year_select", selected = "")
#     updateSelectInput(session, "currency_select", selected = "")
#   })
# }
tab3Server <- function(input, output, session) {
  observeEvent(input$clear_inputs, {
    print("Clear button clicked")
    # Reset inputs to default values
    updateSelectInput(session, "plan_select", selected = "")
    updateSelectInput(session, "spatial_scale", selected = "")
    updateSelectInput(session, "year_select", selected = "")
    updateSelectInput(session, "currency_select", selected = "")
  })
}
