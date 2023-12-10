library(shiny)
library(ggplot2)
library(shinydashboard)

# Assuming your data is in a CSV file named df.csv
df <- read.csv("df.csv")
df <- subset(df, Label == "Percent")

ui <- dashboardPage(
  dashboardHeader(title = "Pie Chart Sectors"),
  dashboardSidebar(
    selectInput("selected_state", "Select State", choices = unique(df$State), multiple = FALSE),
    selectInput("selected_year", "Select Year", choices = unique(df$Year), multiple = FALSE)
  ),
  dashboardBody(
    fluidRow(
      box(
        plotOutput("pieChart"),
        width = 12
      ),
      box(
        HTML('<div style="font-size: 14px; text-align: justify;">
             <p>This pie chart visually represents the different sectors of employment in a selected state and year. Through the proportional size of each slice, it provides a clear depiction of how employment is distributed across various sectors.</p>
             <p>Such data is relevant as it not only helps us understand the overall levels of employment but also offers insight into the types of jobs available in specific areas. This makes pie charts a valuable resource for those studying or working in fields that are impacted by employment trends.</p>
             <p>By analyzing this data, we can adapt to changing employment needs and make decisions accordingly. The pie chart presents each employment sector as a slice of the whole, with the size of each slice correlating to the percentage of employment in that sector.</p>
             <p>Notably, the pie charts ability to display data proportionally makes it an effective tool for visualizing the relative amounts of employment in each sector and comparing data between different sectors, providing an overview of the states workforce</p>
             <p>For example, it can be used in various ways, such as analyzing changes in employment demand across various industries to help in predicting future employment trends and developing policies or programs aimed at addressing those trends.</p>
             </div>'),
        width = 12
      )
    )
  )
)

server <- function(input, output) {
  selected_state_data <- reactive({
    subset(df, State == input$selected_state)
  })
  
  output$pieChart <- renderPlot({
    req(input$selected_state, input$selected_year)  # Ensure both inputs are selected
    
    Category <- c(
      "Arts, Entertainment, Recreation, and Accommodation and Food Services",
      "Educational Services and Health Care and Social Assistance",
      "Professional, Scientific, and Management, and Administrative and Waste Management Services",
      "Finance and Insurance and Real Estate and Rental and Leasing",
      "Information",
      "Transportation and Warehousing and Utilities",
      "Retail Trade",
      "Wholesale Trade",
      "Manufacturing",
      "Construction",
      "Agriculture, Forestry, Fishing and Hunting, and Mining"
    )
    
    values <- c(
      as.numeric(gsub("%", "", selected_state_data()$Arts..entertainment..and.recreation..and.accommodation.and.food.services)),
      as.numeric(gsub("%", "", selected_state_data()$Educational.services..and.health.care.and.social.assistance)),
      as.numeric(gsub("%", "", selected_state_data()$Professional..scientific..and.management..and.administrative.and.waste.management.services)),
      as.numeric(gsub("%", "", selected_state_data()$Finance.and.insurance..and.real.estate.and.rental.and.leasing)),
      as.numeric(gsub("%", "", selected_state_data()$Information)),
      as.numeric(gsub("%", "", selected_state_data()$Transportation.and.warehousing..and.utilities)),
      as.numeric(gsub("%", "", selected_state_data()$Retail.trade)),
      as.numeric(gsub("%", "", selected_state_data()$Wholesale.trade)),
      as.numeric(gsub("%", "", selected_state_data()$Manufacturing)),
      as.numeric(gsub("%", "", selected_state_data()$Construction)),
      as.numeric(gsub("%", "", selected_state_data()$Agriculture..forestry..fishing.and.hunting..and.mining))
    )
    
    # Create a ggplot pie chart
    pie_chart <- ggplot(data.frame(Category, values), aes(x = "", y = values, fill = Category)) +
      geom_bar(stat = "identity", width = 10, color = "white") +
      coord_polar("y", start = 0) +
      theme_minimal() +
      theme(legend.position = "right", legend.title = element_blank()) +
      labs(title = "Pie Chart Sectors of Employment By State", fill = "Category") +
      theme(axis.title = element_blank(), axis.text = element_blank(), axis.ticks = element_blank())
    
    # Print the ggplot pie chart
    print(pie_chart)
  })
}

# Run the application
shinyApp(ui, server)
