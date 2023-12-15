library(shiny)
library(ggplot2)
library(readr) # Ensure this library is loaded for read_csv

ui <- fluidPage(
  titlePanel("Student Performance Dashboard"),
  sidebarLayout(
    sidebarPanel(
      selectInput("variable", "Choose a Variable:",
                  choices = c("Math Score", "Reading Score", "Writing Score")),
      selectInput("factor", "Choose a Socio-Economic Factor:",
                  choices = c("Parental Education" = "parental.level.of.education",
                              "Lunch Type" = "lunch",
                              "Test Preparation" = "test.preparation.course"))
    ),
    mainPanel(
      plotOutput("scorePlot")
    )
  )
)

server <- function(input, output) {
  # Loading the dataset outside renderPlot
  Student_data <- read_csv("StudentsPerformance.csv")

  output$scorePlot <- renderPlot({
    # Create a reactive expression for the dataset
    reactiveDataset <- reactive({ Student_data })

    # Determine the score variable based on user input
    score_var <- ifelse(input$variable == "Math Score", "math.score",
                        ifelse(input$variable == "Reading Score", "reading.score", "writing.score"))

    # Plot
    ggplot(reactiveDataset(), aes(x = !!sym(input$factor), y = !!sym(score_var))) +
      geom_boxplot() +
      labs(title = paste(input$variable, "by", input$factor),
           x = input$factor,
           y = input$variable)
  })
}

shinyApp(ui = ui, server = server)
