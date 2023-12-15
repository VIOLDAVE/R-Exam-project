#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

#7.Dashboard to allow users easily interact with the data and insights.

ui <- fluidPage(
  titlePanel("Student Performance Dashboard"),

  # Sidebar for input controls
  sidebarLayout(
    sidebarPanel(
      # Add input controls like dropdowns, sliders, etc.
      selectInput("variable", "Choose a Variable:",
                  choices = c("Math Score", "Reading Score", "Writing Score")),
      selectInput("factor", "Choose a Socio-Economic Factor:",
                  choices = c("Parental Education", "Lunch Type", "Test Preparation"))
    ),

    # Main panel for displaying outputs
    mainPanel(
      # Output elements like plots and tables
      plotOutput("scorePlot")
    )
  )
)

server <- function(input, output) {
  output$scorePlot <- renderPlot({
    # Loading the dataset to shiny app.
    Student_data<-read_csv("StudentsPerformance.csv")
    # Store dataset in a reactive expression
    reactiveDataset <- reactive({  Student_data })

    # Creating dynamic plots based on user input
    if(input$variable == "Math Score") {
      score_var <- "math.score"
    } else if(input$variable == "Reading Score") {
      score_var <- "reading.score"
    } else {
      score_var <- "writing.score"
    }

    ggplot(data, aes_string(x = input$factor, y = score_var)) +
      geom_boxplot() +
      labs(title = paste(input$variable, "by", input$factor),
           x = input$factor,
           y = input$variable)
  })
}

shinyApp(ui = ui, server = server)




library(shiny)
library(ggplot2)
library(readr)

# Read the dataset (adjust the path as needed)
Student_data <- read_csv("StudentsPerformance.csv")

ui <- fluidPage(
  titlePanel("Student Performance Dashboard"),
  sidebarLayout(
    sidebarPanel(
      selectInput("variable", "Choose a Variable:",
                  choices = c("Math Score", "Reading Score", "Writing Score")),
      selectInput("factor", "Choose a Socio-Economic Factor:",
                  choices = c("Parental Education", "Lunch Type", "Test Preparation"))
    ),
    mainPanel(
      plotOutput("scorePlot")
    )
  )
)

server <- function(input, output) {
  output$scorePlot <- renderPlot({
    # Determine which score to use
    score_var <- switch(input$variable,
                        "Math Score" = "math_score",
                        "Reading Score" = "reading_score",
                        "Writing Score" = "writing_score")

    # Ensure column names in `aes` match your dataset's column names
    ggplot(Student_data, aes_string(x = input$factor, y = score_var)) +
      geom_boxplot() +
      labs(title = paste(input$variable, "by", input$factor),
           x = input$factor,
           y = input$variable)
  })
}

shinyApp(ui = ui, server = server)

