library(shiny)
library(tidyverse)
library(plotly)

# Load your data
diabetes_new <- read.csv("data/diabetes_new.csv")

# Define UI
ui <- fluidPage(
  titlePanel("Shiny Dashboard"),
  sidebarLayout(
    sidebarPanel(
      selectInput("variable_choice",
                  "Please choose a variable to explore its relationship with diabetes status",
                  choices = names(diabetes_new)[-which(names(diabetes_new) == "Diabetes_status")], # Exclude 'Diabetes_status'
                  selected = "HighBP")
    ),
    mainPanel(
      plotlyOutput("plot")
    )
  )
)

# Define Server
server <- function(input, output) {
  
  output$plot <- renderPlotly({
    # Sample a subset of data
    sampled_data <- sample_n(diabetes_new, 100)
    
    # Generate the plot based on input
    selected_variable <- input$variable_choice
    
    if (selected_variable %in% c("HighBP", "HighChol", "Sex", "HeartDiseaseorAttack", "Smoker", "PhysActivity", "Fruits", "Veggies", "HvyAlcoholConsump", "weight_status")) {
      # Categorical variable bar chart
      p <- ggplot(sampled_data, aes_string(x = "Diabetes_status", fill = selected_variable)) + 
        geom_bar(position = "dodge") +
        scale_fill_brewer(palette = 'Pastel1') +
        ylab("Frequencies") +
        xlab("Diabetes Status") +
        ggtitle(paste(selected_variable, "over Diabetes Status")) +
        theme_minimal()
    } else {
      # Numerical variable histogram
      p <- ggplot(sampled_data, aes_string(x = selected_variable, fill = "Diabetes_status")) +
        geom_histogram(aes(y = ..count..), position = "identity", alpha = 0.5, binwidth = 1) +
        scale_fill_brewer(palette = "Pastel1") +
        ylab("Count") +
        xlab(selected_variable) +
        ggtitle(paste(selected_variable, "Distribution")) +
        theme_minimal()
    }
    ggplotly(p)
  })
}

# Run the app
shinyApp(ui = ui, server = server)
