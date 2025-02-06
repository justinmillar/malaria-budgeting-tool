# bslib version
tab0UI <- function(id) {
  ns <- NS(id)
  fluidPage(
    titlePanel("Malaria Budget Generation and Comparison Tool"),
    h6("The Malaria Budget Generation and Comparison Tool is designed to support National Malaria Programs
       in easily generating budgets for different operational plans. This tool is intended to support the Costed
       Optimised Operational Plan initiative (CO-OP)."),
    h6("The CO-OP process aims to build a single costed and optimized plan including different funding scenarios that will
       directly inform multiple funding requests (Global Fund applications, PMI Malaria Operational plans,
       National Health budget request, etc.). This approach builds on existing country processes and intends
       to both further strengthen national ownership of the malaria response and reduce the administrative
       burden associated with funding processes of global health partners. CO-OP is part of the “Big Push” initiative which  is supported by the RBM Partnership
       to End Malaria, and all key malaria ecosystem stakeholders. More information can be found on their website **[link if this gets made]**."),
    h6("The following sections include an overview of each section and how to use and interact with this tool. Please make sure to read each
       section before advancing with use of this tool."),
    h3("Instructions"),

    # Collapsible sections using bslib::accordion
    accordion(
      id = ns("instructions"),
      open = NULL, # Keeps all sections collapsed initially
      accordion_panel(
        title = "Overview",
        "This section provides an overview of the tool's purpose, intended audience, and key features."
      ),
      accordion_panel(
        title = "User Inputs",
        "Detailed instructions"
      ),
      # accordion_panel(
      #   title = "Data Upload",
      #   "Detailed instructions"
      # ),
      # accordion_panel(
      #   title = "Input Check",
      #   "Detailed instructions"
      # ),
      accordion_panel(
        title = "Plan Visualization",
        "Detailed instructions"
      ),
      accordion_panel(
        title = "Plan Comparisons",
        "Detailed instructions"
      ),
      accordion_panel(
        title = "Report Generation",
        "Detailed instructions"
      ),
      accordion_panel(
        title = "Methods",
        "See this page for a full summary on the methodology used in the budget generation processes."
      )
    )
  )
}



# #shiny dashboard version
# tab0UI_DB <- function(id) {
#   ns <- NS(id)
#   fluidPage(
#     titlePanel("Malaria Budget Generation and Comparison Tool"),
#     h4("The Malaria Budget Generation and Comparison Tool is designed to support National Malaria Programs
#        in easily generating budgets for different operational plans. This tool is intended to support the Costed
#        Optimised Operational Plan initiative (CO-OP)."),
#     h4("The CO-OP process aims to build a single costed and optimized plan including different funding scenarios that will
#        directly inform multiple funding requests (Global Fund applications, PMI Malaria Operational plans,
#        National Health budget request, etc.). This approach builds on existing country processes and intends
#        to both further strengthen national ownership of the malaria response and reduce the administrative
#        burden associated with funding processes of global health partners. CO-OP is part of the “Big Push” initiative which  is supported by the RBM Partnership
#        to End Malaria, and all key malaria ecosystem stakeholders. More information can be found on their website **[link if this gets made]**."),
#     h4("The following sections include an overview of each section and how to use and interact with this tool. Please make sure to read each
#        section before advancing with use of this tool."),
#     h3("Instructions"),
#     # Collapsible sections using shinyBS
#     bsCollapse(
#       id = ns("instructions"),
#       open = NULL, # Keeps all sections collapsed initially
#       multiple = TRUE, # Allow multiple sections to stay open
#       bsCollapsePanel(
#         title = "Overview",
#         "This section provides an overview of the tool's purpose, intended audience, and key features.",
#         style = NULL
#       ),
#       bsCollapsePanel(
#         title = "User Inputs",
#         "Detailed instructions",
#         style = NULL
#       ),
#       # bsCollapsePanel(
#       #   title = "Data Upload",
#       #   "Detailed instructions",
#       #   style = NULL
#       # ),
#       # bsCollapsePanel(
#       #   title = "Input Check",
#       #   "Detailed instructions",
#       #   style = NULL
#       # ),
#       bsCollapsePanel(
#         title = "Plan Visualization",
#         "Detailed instructions",
#         style = NULL
#       ),
#       bsCollapsePanel(
#         title = "Plan Comparisons",
#         "Detailed instructions",
#         style = NULL
#       ),
#       bsCollapsePanel(
#         title = "Report Generation",
#         "Detailed instructions",
#         style = NULL
#       ),
#       bsCollapsePanel(
#         title = "Methods",
#         "See this page for a full summary on the methodology used in the budget generation processes",
#         style = NULL
#       )
#     )
#   )
# }

