library(shiny)

shinyUI(fluidPage(
  titlePanel(""),
  sidebarLayout(
    position = "left",
    sidebarPanel(
      sliderInput(
        "stevilo",
        label = "Število skupin:",
        min = 2, max = 11, step = 1,
        value = 2
      )
    )
    ,
    mainPanel(plotOutput("graf"))
  ),
  uiOutput("izborTabPanel")
))
