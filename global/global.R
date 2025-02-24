#-------------------------------------------------------------------------------
# Global script for reading in data for the demo version of the tool
#
#-------------------------------------------------------------------------------
library(tidyverse)
library(viridisLite)
library(glue)
library(billboarder)
library(treemap)
library(RColorBrewer)
library(htmlwidgets)
library(readxl)
library(leaflet)
library(leaflet.extras2)
library(sf)

#-read in usable data-----------------------------------------------------------

# intervention mix maps
intervention_mix_maps <-
  st_read("data/nga-demo-data-pre-processed/shapefiles/combined_interactive_map.shp") |>
  select(-year) |>
  distinct() |>
  rename(intervention_summary = intrvn_,
         plan_shortname = pln_shr,
         plan_description = pln_dsc)

# static intervention maps
static_mix_maps <-
  st_read("data/nga-demo-data-pre-processed/shapefiles/combined_static_map.shp") |>
  rename(intervention = intrvnt,
         intervention_type = intrvn_,
         plan_shortname = pln_shr,
         plan_description = pln_dsc) |>
  mutate(intervention_type =
           case_when(is.na(intervention_type) ~ "NA" ,
                     TRUE ~ intervention_type))

# Shape files
country_outline <- sf::st_read("data/nga-demo-data-pre-processed/shapefiles/country_shapefile.shp")
state_outline   <- sf::st_read("data/nga-demo-data-pre-processed/shapefiles/state_shapefile_simp.shp")
lga_outline     <- sf::st_read("data/nga-demo-data-pre-processed/shapefiles/lga_shapefile_simp.shp")

state_outline$state[which(state_outline$state == "Akwa-Ibom")] <- "Akwa Ibom"

# plan budget data
national_budget <- read_xlsx("data/nga-demo-data-pre-processed/budgets-generated/combined-plan-budgets.xlsx",
                             sheet = "National")

state_budget <- read_xlsx("data/nga-demo-data-pre-processed/budgets-generated/combined-plan-budgets.xlsx",
                          sheet = "State")

lga_budget <- read_xlsx("data/nga-demo-data-pre-processed/budgets-generated/combined-plan-budgets.xlsx",
                          sheet = "LGA")

# Extract unique plans and their descriptions, excluding 'baseline'
unique_plans <- unique(national_budget$plan_shortname)
plan_descriptions <- unique(national_budget$plan_description)

# Combine plan and plan_description for checkbox labels
plan_labels <- paste(unique_plans, "-", plan_descriptions)

# values to use as the year input selection values
years_to_select <- unique(national_budget$year)
plans_to_select <- plan_labels


