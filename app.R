library(shiny)
library(shinydashboard)
library(ggplot2)

theme_set(theme_minimal())
source("data.wrangling.R")

data_df <- read.csv("df.csv")

# Filter the dataset for enrollment data (Liuci)
enrollment_df <- filter(data_df, Label == "Estimate")

# Filter the dataset for employment data (Allie)
employment_df <- filter(df, Label == "Percent", )
employment_df$Employed <- as.numeric(gsub("%", "", employment_df$Employed))

ui <- dashboardPage(
  dashboardHeader(title = "Education and Unemployment"),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("Intro", tabName = "intro"),
      menuItem("Enrollment Trends", tabName = "enrollment_trends"),
      menuItem("Employment Sectors", tabName = "pie_chart"),
      menuItem("Employment Trends", tabName = "employment_trends")
    )
  ),
  
  dashboardBody(
    tabItems(
      tabItem(tabName = "intro",
              fluidPage(
                titlePanel("Mapping the Future: Post-Secondary Education and Statewide Employment Dynamics in the United States"),
                h4("Allison Martin, Liucija Raisys, Sophie Dahl"),
                HTML("<p>This project aims to explore the correlation between higher education enrollment and unemployment and employment 
                      rates across the United States, focusing on the years 2010, 2014, and 2018. The analysis seeks to uncover patterns 
                      and trends within individual states, shedding light on how pursuing higher education may impact local labor markets. 
                      The significance lies in understanding the intricate relationship between educational attainment and workforce outcomes, 
                      which has implications for economic development, policy formulation, and the well-being of individuals and communities.</p>
                    <p>Our dataset comprises detailed information for each state, encompassing total enrollment in higher education institutions, 
                      total unemployment and employment estimates with corresponding percentages, and the distribution of the workforce across 
                      various industries. By examining these variables over the specified years, the project aims to identify associations 
                      between higher education participation and employment outcomes at the state level. This analysis is of particular interest 
                      due to the potential insights it can offer into the effectiveness of higher education in addressing unemployment challenges, 
                      informing state-level policies, and promoting economic resilience. Ultimately, the findings may contribute to a deeper 
                      understanding of the role of education in shaping local labor markets and facilitate evidence-based decision-making in 
                      education and workforce development.</p>
                    <p>Explore the interactive pages to further examine trends and compelling data stories.</p>"),
                HTML("<p>For more details, check out our <a href='#'>full report</a>.</p>")
              )
      ),
      
      # Liuci
      tabItem(tabName = "enrollment_trends",
              fluidPage(
                titlePanel("Post-Secondary Enrollment Trends by State"),
                h5("This page will be examining trends of higher education enrollment and how they have fluctuated over the years in each individual state"),
                
                sidebarLayout(
                  sidebarPanel(
                    selectInput("State", "Select State",
                                choices = unique(enrollment_df$State)),
                    selectInput("Year", "Select Year",
                                choices = unique(enrollment_df$Year))
                  ),
                  mainPanel(
                    plotOutput("line_plot", height = "500px"),
                    HTML("<p>Based on this data, we are able to look at each individual state to see how higher education trends have either 
                      increased or decreased over our 8 year span. We can see that in majority of the states, the enrollment rates have
                      decreased from 2010-2018. However, in some states, including: <strong> Washington, New Hampshire, Utah, Texas, Delaware, 
                      District of Columbia, Georgia, Kentucky, California, Colorado, Idaho, Maine </strong> and <strong> Nevada </strong>, 
                      we saw increased enrollment rates. </p>"),
                    plotOutput("scatter_all_states", height = "500px"),
                    HTML("<p>Based on this data, we can see that in general, college enrollment rates remained relatively steady in most states,
                      however, in other states there is more fluctuation. We see higher rates of college enrollment across the board in the year
                      2010, and slight decreases in both 2014 and 2018.</p>
                      <p>In all three years, you can see that there are three states, <strong> California, New York </strong> and <strong> Texas </strong> 
                      with significantly higher enrollment rates compared to other states. This is likely due to the fact that these three states have the 
                      most colleges within their state. <strong> California </strong> has the most at <strong> 658 </strong> colleges, <strong> New York </strong> 
                      is second with <strong> 418 </strong> colleges and <strong> Texas </strong> is third with <strong> 410 </strong> colleges. </p>")
                  )
                )
              )
      ),
      
      # Allie
      tabItem(tabName = "employment_trends",
              fluidPage(
                titlePanel("Employment Trends by State"),
                h5("This page will be examining the percentage of people who are employed and how it has fluctuated over the years in each individual state"),
                
                sidebarLayout(
                  sidebarPanel(
                    selectInput("State", "Select State",
                                choices = unique(employment_df$State)),
                    selectInput("Year", "Select Year",
                                choices = unique(enrollment_df$Year))
                  ),
                  mainPanel(
                    plotOutput("line_plot", height = "500px"),
                    HTML("<p>Based on this data, we are able to look at each individual state to see how the percentage of the state population that is employed has either 
                      increased or decreased over our 8 year span from 2010 to 2018. We can see that in majority of the states, the employment percentage has 
                      increased from 2010-2018. However, in some states, including: <strong> Alaska, Louisiana, New Mexico, North Dakota, </strong> and <strong> Wyoming </strong>,
                      we saw a decrease in the percentage of the state population that is employed. </p>"),
                    plotOutput("scatter_all_states", height = "500px"),
                    HTML("<p>Based on this data, we can see that in general, the percentage of people who are employed in each state increased slightly.</p>
                      <p>In all three years, you can see that there are two states, <strong> West Virginia </strong> and <strong> Mississippi </strong> 
                      with significantly lower employment percentages than other states. However, we see in 2018, <strong> New Mexico </strong> has a large decrease
                      in their employment percentage, joining <strong> West Virginia </strong> and <strong> Mississippi </strong> in being the three states
                      with the lowest employment percentages. </p>")
                  )
                )
              )
      ),
      
      # Sophie
      tabItem(tabName = "pie_chart",
              fluidPage(
                titlePanel("Pie Chart Sectors"),
                sidebarLayout(
                  sidebarPanel(
                    selectInput("selected_state", "Select State", choices = unique(df$State), multiple = FALSE),
                    selectInput("selected_year", "Select Year", choices = unique(df$Year), multiple = FALSE),
                  ),
                  mainPanel(
                    plotOutput("pieChart", width = 12),
                    HTML("<p>This pie chart visually represents the different sectors of employment in a selected state and year. Through the proportional size of each slice, it provides a clear depiction of how employment is distributed across various sectors.</p>
                        <p>Such data is relevant as it not only helps us understand the overall levels of employment but also offers insight into the types of jobs available in specific areas. This makes pie charts a valuable resource for those studying or working in fields that are impacted by employment trends.</p>
                        <p>By analyzing this data, we can adapt to changing employment needs and make decisions accordingly. The pie chart presents each employment sector as a slice of the whole, with the size of each slice correlating to the percentage of employment in that sector.</p>
                        <p>Notably, the pie charts ability to display data proportionally makes it an effective tool for visualizing the relative amounts of employment in each sector and comparing data between different sectors, providing an overview of the states workforce</p>
                        <p>For example, it can be used in various ways, such as analyzing changes in employment demand across various industries to help in predicting future employment trends and developing policies or programs aimed at addressing those trends.</p>")
                  )
                )
              )
      )
    )
  )
)


server <- function(input, output) {
  
  # Liuci
  enrollment_state_data <- reactive({
    subset(enrollment_df, State == input$State)
  })
  enrollment_year_data <- reactive({
    subset(enrollment_df, Year == input$Year)
  })
  
  output$line_plot <- renderPlot({
    ggplot(enrollment_state_data(), aes(x = Year, y = Enrollment, color = State, group = 1)) +
      geom_line(size = 1.25) +
      labs(title = paste("Post-Secondary Enrollment for", input$State),
           x = "Year",
           y = "Enrollment")
  })
  
  output$scatter_all_states <- renderPlot({
    data <- enrollment_year_data()
    data$Enrollment <- as.numeric(data$Enrollment)
    ggplot(data, aes(x = State, y = Enrollment, color = State)) +
      geom_point(size = 3) +
      scale_y_continuous(labels = scales::comma) +
      labs(title = paste("Post-Secondary Enrollment for All States in", input$Year),
           x = "State",
           y = "Enrollment") +
      theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 8),
            axis.title.x = element_text(margin = margin(t = 20)))
  }, width = 1000)
  
  
  # Sophie 
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
    
    pie_chart <- ggplot(data.frame(Category, values), aes(x = "", y = values, fill = Category)) +
      geom_bar(stat = "identity", width = 10, color = "white") +
      coord_polar("y", start = 0) +
      theme_minimal() +
      theme(legend.position = "right", legend.title = element_blank()) +
      labs(title = "Pie Chart Sectors of Employment By State", fill = "Category") +
      theme(axis.title = element_blank(), axis.text = element_blank(), axis.ticks = element_blank())
    
    print(pie_chart)
  })
  
  
  # Allie
  output$line_plot <- renderPlot({
    selected_data <- subset(employment_df, State == input$state)
    
    ggplot(selected_data, aes(x = Year, y = Employed, group = 1)) +
      geom_line() +
      labs(title = paste("Employment Trends for", input$state),
           x = "Year", y = "Employment (%)")
  })
  
  output$scatter_all_states <- renderPlot({
    selected_data <- subset(employment_df, Year == input$Year)
    
    ggplot(selected_data, aes(x = as.factor(State), y = as.numeric(Employed), group = 1, color = State)) +
      geom_point(size = 3) +
      scale_y_continuous(labels = scales::comma) +
      labs(title = paste("Employment Percentage for All States in", input$Year),
           x = "State",
           y = "Employment Percentage") +
      theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 8),
            axis.title.x = element_text(margin = margin(t = 20)))
  }, width = 1000)
}

shinyApp(ui = ui, server = server)