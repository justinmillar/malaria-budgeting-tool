#-----------------------------------------------------------------------------------------
# Example multi year budget generation for NGA
#
# Function will not work if you set different intervention mix for different
# years this is in progress - just change year 2025 and we assume
# this will be the same for each other year
#
# It won't work if you change the intervention type yet
#
# And it won't work if you change the cm_public or cm_private options
#
# The only things we can change and get a budet for is
# 1's and 0s for itns, iptp, smc, pmc, vacc, lsm, irs
#
# Can't alter costs assicated with support services
#
# You could amend the unit cost sheet also if that's of intererst the data
# is in "data/nga-demo-data-pre-processed/uploads/unit_cost_data.csv
# and the overal budget will change
#--------------------------------------------------------------------------------------------

# to create a new budget make sure the intervention mix file is saved in the
# correct place
# "data/nga-demo-data-pre-processed/uploads/intervention-mix-plan[ ].xlsx"
# with the correct Plan[ ] new label - you can remove the old lines as they don't
# need to be run again and the downstream functions works with the data in
# folders
# then add this into the intervention_mix_list and running the rest of the code
# as is

# library
library(tidyverse)
library(openxlsx)
library(readxl)
library(purrr)
library(sf)

#-GENERATE BUDGETS AND SHAPEFILES--------------------------------------------------------------
# source budget gen function
source("data/nga-demo-data-pre-processed/scripts/f1-budget-generation-function.R")

# List of intervention mix datasets
intervention_mix_list <- list(
  PlanA =
    readxl::read_excel("data/nga-demo-data-pre-processed/uploads/intervention-mix-planA.xlsx"),
  PlanB =
    readxl::read_excel("data/nga-demo-data-pre-processed/uploads/intervention-mix-planB.xlsx"),
  PlanC =
    readxl::read_excel("data/nga-demo-data-pre-processed/uploads/intervention-mix-planC.xlsx"),
  PlanD =
    readxl::read_excel("data/nga-demo-data-pre-processed/uploads/intervention-mix-planD.xlsx")
)

# Apply function to each dataset
walk(intervention_mix_list, create_plan_cost_summary)

#-COMBINE ALL DATASETS INTO SINGLE EXCEL FILE----------------------------------------------------

# Define the list of generated budget Excel files
budget_files <- list.files(
  path = "data/nga-demo-data-pre-processed/budgets-generated/",
  pattern = "-budgets.xlsx$",
  full.names = TRUE
)

# Function to read a specific sheet from all budget files and add a `plan_name` column
read_budget_sheet <- function(sheet_name) {
  map_dfr(budget_files, function(file) {
    plan_name <- gsub("-budgets.xlsx", "", basename(file))  # Extract plan name
    read_excel(file, sheet = sheet_name) %>%
      mutate(plan_name = plan_name)  # Add plan identifier
  })
}

# Read and combine data for each sheet
lga_combined <- read_budget_sheet("LGA")
state_combined <- read_budget_sheet("State")
national_combined <- read_budget_sheet("National")

# Create a new Excel workbook
wb <- createWorkbook()

# Add combined sheets
addWorksheet(wb, "LGA")
writeData(wb, "LGA", lga_combined)

addWorksheet(wb, "State")
writeData(wb, "State", state_combined)

addWorksheet(wb, "National")
writeData(wb, "National", national_combined)

# Save the final combined Excel file
saveWorkbook(wb,
             "data/nga-demo-data-pre-processed/budgets-generated/combined-plan-budgets.xlsx",
             overwrite = TRUE)

#-COMBINE ALL SHAPEFILES INTO SINGLE VERSION----------------------------------------------------
# Define the directory containing the shapefiles
shapefile_dir <- "data/nga-demo-data-pre-processed/shapefiles/"

# List all interactive and static shapefiles
interactive_shapefiles <- list.files(
  path = shapefile_dir,
  pattern = "-interactive-map.shp$",  # Adjust pattern if needed
  full.names = TRUE
)

static_shapefiles <- list.files(
  path = shapefile_dir,
  pattern = "-static-map.shp$",  # Adjust pattern if needed
  full.names = TRUE
)

# Function to read and add a plan name column to each shapefile
read_plan_shapefile <- function(file) {
  plan_name <- gsub("-interactive-map.shp|-static-map.shp", "", basename(file))  # Extract plan name
  st_read(file) %>%
    mutate(plan_name = plan_name)  # Add plan identifier
}

# Read and combine interactive shapefiles
if (length(interactive_shapefiles) > 0) {
  combined_interactive_map <- map_dfr(interactive_shapefiles, read_plan_shapefile)
  st_write(combined_interactive_map, paste0(shapefile_dir, "/combined_interactive_map.shp"), delete_dsn = TRUE)
}

# Read and combine static shapefiles
if (length(static_shapefiles) > 0) {
  combined_static_map <- map_dfr(static_shapefiles, read_plan_shapefile)
  st_write(combined_static_map, paste0(shapefile_dir, "combined_static_map.shp"), delete_dsn = TRUE)
}
