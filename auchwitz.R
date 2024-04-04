
library(babynames)
library(leaflet)
library(mapdeck)
library(shiny)
library(tidyverse)
library(troopdata)
library(usethis)

file_path <- "Auschwitzdeaths.csv"

data <- read.csv(file_path)

colnames(data) <- c("Nationality_or_Category", "Number_of_deportees", "Percentage_of_total_deportees",
                    "Number_of_victims", "Percentage_of_murdered_within_category_or_nationality",
                    "Percentage_of_all_victims")

data <- data[-c(7), ]



# Define UI for application that draws a histogram
ui <- fluidPage(
  # Application title
  titlePanel("Holocaust Victims at Auschwitz"),
  
  # Sidebar with a slider input for nationality of victims
  sidebarLayout(
    sidebarPanel(
      selectInput("nationality", "Select Nationality/Category:",
                  choices = c("Jews", "Poles", "Other groups", "Roma (Gypsies)", "Soviet POWs"),
                  multiple = TRUE)
    ),
    # Show a plot of the generated distribution
    mainPanel(
      plotOutput("plot"),
      dataTableOutput("table")
    )
  )
)
    


# Code for server and for graph.
server <- function(input, output) {
  
  filtered_data <- reactive({
    if (length(input$nationality) > 0) {
      data %>% filter(Nationality_or_Category %in% input$nationality)
    } else {
      data
    }
  })
  
  output$plot <- renderPlot({
    ggplot(filtered_data(), aes(x = Nationality_or_Category, y = Number_of_victims)) +
      geom_bar(stat = "identity") +
      labs(title = "Number of People Murdered by Nationality/Category")
  })
  
  output$table <- renderDataTable({
    filtered_data()
  })
  
}

# Run the application
shinyApp(ui = ui, server = server)