
tab3Server <- function(input, output, session, lga_outline, state_outline, country_outline, intervention_mix_maps, static_mix_maps) {

  #-Reactive: Filter intervention mix data based on selected plan--------------------------------
  filtered_intervention_mix <- reactive({
    req(input$plan_select)
    intervention_mix_maps[intervention_mix_maps$plan_shortname == input$plan_select, ]
  })

  #-Reactive: Filter static mix data for selected plan & year-------------------------------------
  filtered_static_mix <- reactive({
    req(input$plan_select)

    static_mix_maps |>
      filter(plan_shortname == input$plan_select)

  })

  #-Reactive: Filter LGA list based on selected state----------------------------------------
  lga_list <- reactive({
    req(input$state_select)
    unique(lga_outline$lga[lga_outline$state == input$state_select])
  })

  #-Generate UI for State Selection----------------------------------------------------------
  output$state_ui <- renderUI({
    req(input$spatial_scale %in% c("State", "LGA"))
    selectizeInput(
      session$ns("state_select"),
      "Select State:",
      choices = c("", unique(lga_outline$state)),
      selected = "",
      options = list(placeholder = "Type or select a state", allowEmptyOption = TRUE)
    )
  })

  #-Generate UI for LGA Selection-----------------------------------------------------------
  output$lga_ui <- renderUI({
    req(input$spatial_scale == "LGA", input$state_select)
    selectizeInput(
      session$ns("lga_select"),
      "Select LGA:",
      choices = c("", lga_list()),
      selected = "",
      options = list(placeholder = "Type or select an LGA", allowEmptyOption = TRUE)
    )
  })

  #-Adding plan description-----------------------------------------------------------------
  output$page_description <- renderUI({
    req(input$plan_select, input$year_select, input$spatial_scale)  # Ensure inputs are selected

    # Extract the plan description from the dataset
    plan_desc <-
      intervention_mix_maps |>
      filter(plan_shortname == input$plan_select) |>
      pull(plan_description) |>
      unique()  # Ensure we only get one unique description

    if (length(plan_desc) == 0) plan_desc <- "No description available"  # Default if missing

    # Start with the base description
    description <- paste0("<h4>Displaying results for ", input$plan_select,
                          " at the ", input$spatial_scale, " level for year: ", input$year_select, "</h4>")

    # If "State" is selected, include the state name
    if (input$spatial_scale == "State" && !is.null(input$state_select) && input$state_select != "") {
      description <- paste0(description, "<h4>State: ", input$state_select, "</h4>")
    }

    # If "LGA" is selected, include both state and LGA names
    if (input$spatial_scale == "LGA" && !is.null(input$state_select) && input$state_select != "" &&
        !is.null(input$lga_select) && input$lga_select != "") {
      description <- paste0(description, "<h4>State: ", input$state_select, " | LGA: ", input$lga_select, "</h4>")
    }

    # Add the plan description in a new row
    description <- paste0(description, "<h5><strong>Plan Description:</strong> ", plan_desc, "</h5>")

    # Render the HTML output
    HTML(description)
  })

  #-Generate the interactive map-----------------------------------------------------------
  output$interactive_map <- renderLeaflet({
    req(input$plan_select, input$year_select)

    create_intervention_leaflet(
      lga_outline = lga_outline,
      state_outline = state_outline,
      country_outline = country_outline,
      intervention_mix = filtered_intervention_mix(),
      center_lng = 9,
      center_lat = 4,
      zoom = 5.2
    )
  })

  #-Generate the static maps tabbed-----------------------------------------------------
  # make maps
  observe({
    req(input$plan_select)

    years <- unique(static_mix_maps$year)
    if (length(years) == 0) return(NULL)

    # Create a tab for each available year plus one for "All Years".
    lapply(c("All Years", years), function(y) {
      output[[paste0("static_map_", y)]] <- renderPlot({
        create_static_map(
          lga_outline = lga_outline,
          state_outline = state_outline,
          filtered_data = filtered_static_mix(),
          plan_select = input$plan_select,
          year_value = y
        )
      })
    })
  })

  # UI for multiple static maps, with tabs for each year
  output$static_map_tabs <- renderUI({
    req(input$plan_select)

    years <- unique(static_mix_maps$year)
    if (length(years) == 0) return(NULL)  # Prevent crash if no data

    navset_tab(
      !!!lapply(c("All Years", years), function(y) {
        nav_panel(
          title = paste(y),
          plotOutput(session$ns(paste0("static_map_", y)), height = "500px")
        )
      })
    )
  })


  #-Show Maps UI when Plan & Year are selected------------------------------------------
  output$maps_ui <- renderUI({
    req(input$plan_select != "")

    layout_column_wrap(
      width = 1/2,  # Two columns, each taking 50% width

      # Interactive Map Card
      card(
        card_header("Full Intervtion Mix Map"),
        card_body_fill(
          leafletOutput(session$ns("interactive_map"), height = "500px")
        )
      ),

      # Static Map Card: Show either a **single plot** or **tabs**
      card(
        card_header("Intervention Specific Maps"),
        card_body_fill(
          uiOutput(session$ns("static_map_tabs"))  # Always show tabs
        )
        )

    )
  })

  #-Ribbon Icons------------------------------------------------------------------------
  output$value_boxes <- renderUI({

    req(input$plan_select, input$year_select, input$spatial_scale, input$currency_select)
    # For spatial scales that require a state selection, ensure input$state_select exists
    if (input$spatial_scale %in% c("State", "LGA")) {
      req(input$state_select)
    }
    # For LGA level, require an LGA selection
    if (input$spatial_scale == "LGA") {
      req(input$lga_select)
    }

    create_icon_summaries(
      plan_select = input$plan_select,
      year_select = input$year_select,
      spatial_scale = input$spatial_scale,
      state_select = input$state_select,
      lga_select = input$lga_select,
      currency_select = input$currency_select
    )
  })

  #-Clear selections--------------------------------------------------------------------
  observeEvent(input$clear_inputs, {
    updateSelectInput(session, "plan_select", selected = "")
    updateSelectInput(session, "spatial_scale", selected = "")
    updateSelectInput(session, "year_select", selected = "")
    updateSelectInput(session, "currency_select", selected = "")
    updateSelectizeInput(session, "state_select", selected = "")
    updateSelectizeInput(session, "lga_select", selected = "")
  })
}
