
library(shiny)
library(shinythemes)
library(stringr)
library(dplyr)
library(ggplot2)

# df <- read.csv("")

ui <- fluidPage(
  theme = shinytheme("readable"),
  titlePanel(" Education and Unemployment Datasets Nutrition Label"),
  sidebarLayout(
    sidebarPanel(
      h3("Education Rates Dataset:"),
      p("Created By: National Center for Education Statistics (NCES)"),
      p("Who Is In This Dataset?: This dataset is based off of a general information survey and the responses are from United States citizens."),
      p("Data Time Range: 1970 - 2020"),
      p("At A Quick Glance: This dataset looks at rates of enrollment in degree granting postsecondary institutions, in a state by state format."),
      h4("Potential Ways to Use This Data"),
      tags$ul(
        tags$li("Identify different trends regarding education over time"),
        tags$li("Examining Educational Policy Impact"),
        tags$li("Analyzing Educational Spending Impacts"),
        tags$li("Identify states with high college attendance but low completion rates and explore potential reasons")
      ),
      width = 6
    ),
    sidebarPanel(
      h3("Unemployment Rates Dataset:"),
      p("Created By: United States Census Bureau"),
      p("Who is in this dataset?: The data is based on a sample and subject to sampling variability, and is representative of United States Citizens."),
      p("Data Time Range: 1970 - 2020"),
      p("At A Quick Glance: This data looks at unemployment rates in a state by state format - this dataset also covers some useful details about employment status - such as job type - industry - percentage of people with families - and other details to create a fuller picture of that state's demographic."),
      h4("Potential Ways to Use This Data:"),
      tags$ul(
        tags$li("Identify areas that may be experiencing economic growth or decline."),
        tags$li("Identify different trends regarding employment over time"),
        tags$li("Examine the relationship between income inequality and employment rates")
      ),
      width = 6
    )
  )
)


server <- function(input,output){

}

shinyApp(ui = ui, server = server)
