
library(shiny)
library(shinydashboard)
library(shinyBS)
library(bslib)

#-Source UI and server functions for each tab-----------------------------------
source("global/source-ui-server-code.R")

#-Define UI using shinydashboard------------------------------------------------
ui <- dashboardPage(
  skin = "red",
  #-Title Nav bar----------------------------
  dashboardHeader(
    title = "Malaria COOP Tool",
    tags$li(class = "dropdown", actionLink("info", icon("info"))),
    tags$li(
      class = "dropdown",
      tags$a(
        id = "messageTeam",
        href = "mailto:hthompson@path.org",
        icon("comments")
      )
    ),
    tags$li(
      class = "dropdown",
      tags$a(
        id = "home",
        href = 'https://www.path.org/who-we-are/programs/malaria/malaria-control-and-elimination-partnership-in-africa-macepa/',
        icon("house")
      )
    )
  ),

  #-Sidebar navigation menu------------------
  dashboardSidebar(
    sidebarMenu(
      menuItem("Overview", tabName = "tab0", icon = icon("gauge-high")),
      menuItem("Data Download", tabName = "tab1a", icon = icon("edit")),
      menuItem("Data Upload", tabName = "tab1b", icon = icon("edit")),
      menuItem("Input Check", tabName = "tab2", icon = icon("check")),
      menuItem("Plan Visualization", tabName = "tab3", icon = icon("chart-bar")),
      menuItem("Plan Comparisons", tabName = "tab4", icon = icon("exchange-alt")),
      menuItem("Report Generation", tabName = "tab5", icon = icon("file-alt")),
      menuItem("Methods", tabName = "tab6", icon = icon("book"))
    ),
    #Adding color logo
    div(
      style = "position: absolute; bottom: 0; width: 100%; text-align: left; padding: 10px;",
      img(src = "PATH_Logo_Color.png", height = "50px", alt = "Company Logo")
    )
  ),

  #-Main body of each tab--------------------
  dashboardBody(
    tabItems(
      tabItem(tabName = "tab0", tab0UI_DB("tab0")),
      tabItem(tabName = "tab1a", tab1aUI("tab1a")),
      tabItem(tabName = "tab1b", tab1bUI("tab1b")),
      tabItem(tabName = "tab2", tab2UI("tab2")),
      tabItem(tabName = "tab3", tab3UI("tab3")),
      tabItem(tabName = "tab4", tab4UI("tab4")),
      tabItem(tabName = "tab5", tab5UI("tab5")),
      tabItem(tabName = "tab6", tab6UI("tab6"))
    )
  )
)

#-Define Server-----------------------------------------------------------------
server <- function(input, output, session) {
  callModule(tab0Server, id = "tab0")
  callModule(tab1aServer, id = "tab1a")
  callModule(tab1bServer, id = "tab1b")
  callModule(tab2Server, id = "tab2")
  callModule(tab3Server, id = "tab3")
  callModule(tab4Server, id = "tab4")
  callModule(tab5Server, id = "tab5")
  callModule(tab6Server, id = "tab6")
}

#-Run the App-------------------------------------------------------------------
shinyApp(ui, server)


