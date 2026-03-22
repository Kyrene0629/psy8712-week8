library(shiny)
library(tidyverse)

ui <- fluidPage(
  titlePanel("shiny_week8"),
  sidebarLayout(
    sidebarPanel(
      selectInput("gender_choice",
                  "Display Participants",
                  choices = c("All", "Male", "Female"),
                  selected = "All"),
      radioButtons("band_choice",
                   "Regression Error Band",
                   choices = c("Display Error Band", "Suppress Error Band"),
                   selected = "Display Error Band"),
      radioButtons("date_choice",
                   "Participants Before July 1, 2017",
                   choices = c("Include", "Exclude"),
                   selected = "Include")
    ),
    mainPanel(
      plotOutput("meanPlot")
      )
    )
  )

server <- function(input, output) {
  week8_tbl_skinny <- readRDS("week8_tbl_skinny.rds")
  filtered_tbl <- reactive({
    plot_tbl <- week8_tbl_skinny
    if (input$gender_choice != "All") {
      plot_tbl <- filter(plot_tbl, gender == input$gender_choice)
    }
    if (input$date_choice == "Exclude") {
      plot_tbl <- filter(plot_tbl, as.Date(timeStart) >= as.Date("2017-07-01"))
    }
    plot_tbl
  })
  
output$meanPlot <- renderPlot({
  ggplot(filtered_tbl(), aes(x = mean_q1q6, y = mean_q8q10)) +
    geom_point() +
    geom_smooth(method = "lm", 
                color = "purple", 
                se = input$band_choice == "Display Error Band") +
    scale_x_continuous(name = "Mean Scores on Q1-Q6") +
    scale_y_continuous(name = "Mean Scores on Q8-Q10")
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
