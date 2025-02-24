#-------------------------------------------------------------------------------
# Helper functions to call into server code to keep streamlined
#
#-------------------------------------------------------------------------------

#-Leaflet intervention mix map--------------------------------------------------
create_intervention_leaflet <- function(lga_outline, state_outline,
                                           country_outline, intervention_mix_maps,
                                           center_lng = 9, center_lat = 4,
                                           zoom = 5.2) {

  # Define color palette based on unique values in intervention_summary
  color_pal <- colorFactor(
    palette = colorRampPalette(RColorBrewer::brewer.pal(12, "Paired"))(length(unique(intervention_mix_maps$intervention_summary))),
    domain = unique(intervention_mix_maps$intervention_summary)
  )

  leaflet() %>%
    addTiles() %>%
    # Add LGA polygons with color based on intervention_summary
    addPolygons(
      data = intervention_mix_maps,
      fillColor = ~color_pal(intervention_mix_maps$intervention_summary),
      color = "grey",
      weight = 1,
      fillOpacity = 0.9,
      highlightOptions = highlightOptions(
        weight = 3,
        color = "black",
        fillOpacity = 1,
        bringToFront = TRUE
      ),
      label = ~sprintf(
        "<strong>%s</strong><br>State: %s<br>Intervention mix: %s",
        lga, state, intervention_summary
      ) %>% lapply(htmltools::HTML),
      labelOptions = labelOptions(
        direction = "auto",
        textsize = "10px",
        style = list("font-weight" = "normal", "padding" = "3px 8px"),
        sticky = TRUE
      )
    ) %>%
    # Add state boundaries
    addPolygons(
      data = state_outline,
      fillColor = "transparent",
      color = "black",
      weight = 2,
      options = pathOptions(interactive = FALSE)
    ) %>%
    # Add national boundaries
    addPolygons(
      data = country_outline,
      fillColor = "transparent",
      color = "black",
      weight = 2,
      options = pathOptions(interactive = FALSE)
    ) %>%
    # Add legend (pass the values argument to color_pal)
    addLegend(
      pal = color_pal,
      values = intervention_mix_maps$intervention_summary,
      title = "Intervention Mix",
      position = "bottomright",
      opacity = 0.7
    ) %>%
    setView(lng = center_lng, lat = center_lat, zoom = zoom)
}

#-Static Facet intervention maps------------------------------------------------
create_static_map <- function(lga_outline, state_outline, filtered_data,
                              plan_select, year_value) {

  static_data <-  filtered_data

  ggplot() +
    geom_sf(data = lga_outline, fill = "grey90", col = "grey", alpha = 0.5) +
    geom_sf(data = static_data, aes(fill = intervention_type, col = intervention_type), alpha = 0.6) +
    geom_sf(data = state_outline, fill = NA, linewidth = 0.7, color = "black") +
    scale_fill_brewer(palette = "Spectral", direction = -1) +
    scale_color_brewer(palette = "Spectral", direction = -1) +
    theme_void(base_size = 14) +
    facet_wrap(vars(intervention)) +
    theme(
      legend.position = "bottom",
        ) +
    labs(
      fill = "Intervention Class (if specified)",
      col = "Intervention Class (if specified)",
      title = paste0("Intervention mix for: ", plan_select, " - ", year_value)
    )
}

#-Ribbon icons function---------------------------------------------------------
#Create Icon Summary Cards
create_icon_summaries <- function(plan_select, year_select,
                                    spatial_scale,
                                    state_select,
                                    lga_select, currency_select) {

    # Select the data based on the spatial scale
    if (spatial_scale == "National") {
      data <- national_budget |>
        dplyr::filter(
          plan_shortname == plan_select,
          currency == currency_select
        )
    } else if (spatial_scale == "State") {
      data <- state_budget |>
        dplyr::filter(
          adm1 == state_select,
          plan_shortname == plan_select,
          currency == currency_select
        )
    } else if (spatial_scale == "LGA") {
      data <- lga_budget |>
        dplyr::filter(
          adm1 == state_select,
          adm2 == lga_select,
          plan_shortname == plan_select,
          currency == currency_select
        )
    } else {
      data <- NULL
    }

    # Filter by year or aggregate over all years
    if (year_select == "All Years") {
      data <- data |>
        dplyr::select(pop_total, pop_u5, pop_pw,
                      total_budget, total_budget_per_person) |>
        dplyr::summarise(
          pop_total = mean(pop_total, na.rm = TRUE),
          pop_u5 = mean(pop_u5, na.rm = TRUE),
          pop_pw = mean(pop_pw, na.rm = TRUE),
          total_budget = sum(total_budget, na.rm = TRUE),
          total_budget_per_person = sum(total_budget_per_person, na.rm = TRUE)
        )
    } else {
      data <- data |>
        dplyr::filter(year == year_select)
    }

    # Set the currency symbol based on the currency_select value
    currency_symbol <- if (currency_select == "USD") "$" else "₦"

    # Create the UI: Using layout_column_wrap to space 5 boxes evenly across the width
    layout_column_wrap(
      width = 1/5,
      # Total Population
      card(
        card_header(
          tagList(
            icon("users", class = "fa-2x"),
            " Total Population"
          )
        ),
        card_body_fill(
          h3(formatC(data$pop_total, format = "d", big.mark = ","), style = "font-weight: bold;"),
          p("People")
        ),
        class = "bg-info text-white"
      ),
      # Population u5
      card(
        card_header(
          tagList(
            icon("child", class = "fa-2x"),
            " Population u5"
          )
        ),
        card_body_fill(
          h3(formatC(data$pop_0_5,
                     format = "d",
                     big.mark = ","),
             style = "font-weight: bold;"),
          p("Under 5")
        ),
        class = "bg-success text-white"
      ),
      # Pregnant Women (pop_pw)
      card(
        card_header(
          tagList(
            icon("female", class = "fa-2x"),
            " Pregnant Women"
          )
        ),
        card_body_fill(
          h3(formatC(data$pop_pw,
                     format = "d", big.mark = ","), style = "font-weight: bold;"),
          p("Women")
        ),
        class = "bg-warning text-white"
      ),
      # Total Budget
      card(
        card_header(
          tagList(
            icon("dollar-sign", class = "fa-2x"),
            " Total Budget"
          )
        ),
        card_body_fill(
          h4(paste0(currency_symbol,
                    formatC(as.numeric(data$total_budget),
                            format = "f", digits = 0, big.mark = ","))),
          p(currency_select)
        ),
        class = "bg-primary text-white"
      ),
      # Cost Per Person
      card(
        card_header(
          tagList(
            icon("calculator", class = "fa-2x"),
            " Cost Per Person"
          )
        ),
        card_body_fill(
          h4(paste0(currency_symbol,
                    formatC(as.numeric(data$total_budget_per_person),
                            format = "f", digits = 2, big.mark = ","))),
          p(currency_select)
        ),
        class = "bg-danger text-white"
      )
    )
  }

