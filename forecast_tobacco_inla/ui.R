
library(shiny)

shinyUI(fluidPage(
  
  tags$br(),
  
  sidebarPanel(
    conditionalPanel(condition = "input.tabs1 == 'Forecasts'",
      selectInput("iso3_input", "Country:", c("USA", "GBR", "CAN"), selected = "USA"),
      selectInput("sex_input", "Sex:", c("female", "male"), selected = "female"),
      selectInput(
        inputId = "age_input", 
        label = "Age:",
        choices = as.character(seq(10, 80, by = 5)),
        selected = "30"
      )
    ),
    conditionalPanel(condition = "input.tabs1 == 'Age distributions'",
      selectInput("iso3_input2", "Country:", c("USA", "GBR", "CAN"), selected = "USA"),
      selectInput("sex_input", "Sex:", c("female", "male"), selected = "female"),
      sliderInput("cohortRange", label = "Cohort range (birth year)", 
        min = 1900, max = 2013, value = c(1900, 2013),
        round = TRUE, ticks = FALSE, sep = ""
      )
    )
  ),
  mainPanel(
    tabsetPanel(id = "tabs1",
      tabPanel(title = "Forecasts",
        plotOutput("plot1")
      ),
      tabPanel(title = "Age distributions",
        plotOutput("plot2"),
        checkboxInput("showActualData", label = "Show actual data", value = TRUE),
        checkboxInput("showForecasts", label = "Show forecasted data", value = TRUE)
      )
    )
  ),
  tags$head(tags$style(
    HTML(".nav-tabs > .active > a, .nav-tabs > .active > a:hover { outline: 0;}")
  ))
))

