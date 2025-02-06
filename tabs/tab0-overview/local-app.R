#-------------------------------------------------------------------------------------
# App code for specific tab element for local rendering and testing of application
#-------------------------------------------------------------------------------------

library(shiny)
library(bslib)

#-Source UI and server functions for each tab-----------------------------------
source("ui.R")
source("server.R")

#-Define UI---------------------------------------------------------------------
ui <- page_fluid(
  theme = bs_theme(version = 5, bootswatch = "minty"),

  # Add custom CSS for fixed full-height sidebar
  tags$head(
    tags$style(HTML("
      .sidebar-container {
        height: 100vh; /* Full height */
        position: fixed; /* Fix it to the side */
        top: 0;
        left: 0;
        width: 250px; /* Sidebar width */
        overflow-y: auto; /* Scrollable if content overflows */
        background-color: #f8f9fa; /* Light background color */
        border-right: 1px solid #ddd; /* Optional border */
        padding: 15px;
      }
      .main-content {
        margin-left: 250px; /* Adjust margin to match sidebar width */
        padding: 20px;
      }
      .logo-container {
        position: absolute;
        bottom: 10px;
        width: 100%;
        text-align: center;
      }
      .logo-container img {
        max-height: 50px;
      }
    "))
  ),

  #-Sidebar Navigation------------------------------------
  div(
    class = "sidebar-container",
    h3("Malaria CO-OP Tool"),
    tags$a(
      href = 'https://www.path.org/who-we-are/programs/malaria/malaria-control-and-elimination-partnership-in-africa-macepa/',
      icon("house"),
      title = "Go to PATH Malaria Page"
    ),
    br(),
    actionLink("info", label = "", icon = icon("info"), title = "Info"),
    br(),
    tags$a(
      href = "mailto:hthompson@path.org",
      icon("comments"),
      title = "Message Team"
    ),
    br(), br(),
    navset_pill_list(
      id = "sidebar_menu",
      widths = c(12, 1),
      nav_panel(title = tagList(icon("house"), "Overview"), value = "tab0"),
      nav_panel(title = tagList(icon("pen"), "User Inputs"), value = "tab1a"),
      nav_panel(title = tagList(icon("chart-bar"), "Plan Visualization"), value = "tab3"),
      nav_panel(title = tagList(icon("table-columns"), "Plan Comparisons"), value = "tab4"),
      nav_panel(title = tagList(icon("file-pdf"), "Report Generation"), value = "tab5"),
      nav_panel(title = tagList(icon("book"), "Methods"), value = "tab6")
    ),
    # Add logo at the bottom
    div(
      style = "position: absolute; bottom: 0; width: 100%; text-align: left; padding: 10px;",
      img(src = "PATH_Logo_Color.png", height = "50px", alt = "Company Logo")
    )
  ),

  #-Main Content---------------------------------
  div(
    class = "main-content",
    uiOutput("page_content")
  )
)

#-Define Server-----------------------------------------------------------------
server <- function(input, output, session) {

  #-Dynamically render content for each tab------------
  output$page_content <- renderUI({
    switch(input$sidebar_menu,
           "tab0" = tab0UI("tab0")#,
           #"tab1a" = tab1aUI("tab1a"),
           # "tab1b" = tab1bUI("tab1b"),
           # "tab2" = tab2UI("tab2"),
           #"tab3" = tab3UI("tab3"),
           #"tab4" = tab4UI("tab4"),
           #"tab5" = tab5UI("tab5"),
           #"tab6" = tab6UI("tab6")
    )
  })

  #=Call modules for each tab------------------------
  callModule(tab0Server, id = "tab0")
  # callModule(tab1aServer, id = "tab1a")
  # callModule(tab1bServer, id = "tab1b")
  # callModule(tab2Server, id = "tab2")
  # callModule(tab3Server, id = "tab3")
  # callModule(tab4Server, id = "tab4")
  # callModule(tab5Server, id = "tab5")
  # callModule(tab6Server, id = "tab6")
}

#-Run the App------------------------------------------
shinyApp(ui, server)


# #-Define UI using shinydashboard------------------------------------------------
# ui <- dashboardPage(
#   skin = "red",
#   #-Title Nav bar----------------------------
#   dashboardHeader(
#     title = "Malaria COOP Tool",
#     tags$li(class = "dropdown", actionLink("info", icon("info"))),
#     tags$li(
#       class = "dropdown",
#       tags$a(
#         id = "messageTeam",
#         href = "mailto:hthompson@path.org",
#         icon("comments")
#       )
#     ),
#     tags$li(
#       class = "dropdown",
#       tags$a(
#         id = "home",
#         href = 'https://www.path.org/who-we-are/programs/malaria/malaria-control-and-elimination-partnership-in-africa-macepa/',
#         icon("house")
#       )
#     )
#   ),
#
#   #-Sidebar navigation menu------------------
#   dashboardSidebar(
#     sidebarMenu(
#       menuItem("Overview", tabName = "tab0", icon = icon("gauge-high"))#,
#       # menuItem("Data Download", tabName = "tab1a", icon = icon("edit")),
#       # menuItem("Data Upload", tabName = "tab1b", icon = icon("edit")),
#       # menuItem("Input Check", tabName = "tab2", icon = icon("check")),
#       # menuItem("Plan Visualization", tabName = "tab3", icon = icon("chart-bar")),
#       # menuItem("Plan Comparisons", tabName = "tab4", icon = icon("exchange-alt")),
#       # menuItem("Report Generation", tabName = "tab5", icon = icon("file-alt")),
#       # menuItem("Methods", tabName = "tab6", icon = icon("book"))
#     ),
#     #Adding color logo
#     div(
#       style = "position: absolute; bottom: 0; width: 100%; text-align: left; padding: 10px;",
#       img(src = "PATH_Logo_Color.png", height = "50px", alt = "Company Logo")
#     )
#   ),
#
#   #-Main body of each tab--------------------
#   dashboardBody(
#     tabItems(
#       tabItem(tabName = "tab0", tab0UI("tab0")) #,
#       # tabItem(tabName = "tab1a", tab1aUI("tab1a")),
#       # tabItem(tabName = "tab1b", tab1bUI("tab1b")),
#       # tabItem(tabName = "tab2", tab2UI("tab2")),
#       # tabItem(tabName = "tab3", tab3UI("tab3")),
#       # tabItem(tabName = "tab4", tab4UI("tab4")),
#       # tabItem(tabName = "tab5", tab5UI("tab5")),
#       # tabItem(tabName = "tab6", tab6UI("tab6"))
#     )
#   )
# )
#
# #-Define Server-----------------------------------------------------------------
# server <- function(input, output, session) {
#   callModule(tab0Server, id = "tab0")
#   # callModule(tab1aServer, id = "tab1a")
#   # callModule(tab1bServer, id = "tab1b")
#   # callModule(tab2Server, id = "tab2")
#   # callModule(tab3Server, id = "tab3")
#   # callModule(tab4Server, id = "tab4")
#   # callModule(tab5Server, id = "tab5")
#   # callModule(tab6Server, id = "tab6")
# }
#
# #-Run the App-------------------------------------------------------------------
# shinyApp(ui, server)
