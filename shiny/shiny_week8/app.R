library(shiny)
library(tidyverse)

ui <- fluidPage(
  titlePanel("shiny_week8"), # have the app title at the top, match the assignment
  sidebarLayout( # make sure it is a two part layout with a sidebar and a main part. Shiny struture: separate control and output, make sure the structure is organized
    sidebarPanel( # start with the side bar, this is what is important the most
      selectInput("gender_choice", # drop down menu for choosing gender, and give them labels and all the options, all is default, this is required gender display options 
                  "Display Participants",
                  choices = c("All", "Male", "Female"),
                  selected = "All"),
      radioButtons("band_choice", # create radio bottom for showing the regression error band, give the choices labels, and show the regression error band is the default. So people can choose whether to display the error band or suppress 
                   "Regression Error Band",
                   choices = c("Display Error Band", "Suppress Error Band"),
                   selected = "Display Error Band"),
      radioButtons("date_choice", # create another radio bottom for whether to show include or exclude participants that completed the assessment before July 1, 2017. So, people can choose whether they want to display early data before july 1 2017
                   "Participants Before July 1, 2017",
                   choices = c("Include", "Exclude"),
                   selected = "Include")
    ),
    mainPanel( # move to the main panel, so graph output can be hold for the scatterplot
      plotOutput("meanPlot") # show the ggplot as the main panel
      )
    )
  )

server <- function(input, output) {
  week8_tbl_skinny <- readRDS("week8_tbl_skinny.rds") # read the skinney version of the full dataset that we created earlier. only need to do it once
  filtered_tbl <- reactive({ # reactive() can update changes in the input
    plot_tbl <- week8_tbl_skinny # store it to plot_tbl for filtering
    if (input$gender_choice != "All") { # filter if it is a specific gender (male or female) not all, then only keep the rows that match with that specific gender, skips filtering for all
      plot_tbl <- filter(plot_tbl, gender == input$gender_choice)
    }
    if (input$date_choice == "Exclude") { # exclude early participants, uses date rule (before July 1, 2017)
      plot_tbl <- filter(plot_tbl, as.Date(timeStart) >= as.Date("2017-07-01"))
    }
    plot_tbl # after the filtering is done, it returns to the filtered dataset, and this is ready to be used in the plot
  })
  
output$meanPlot <- renderPlot({ # create reactive plot, use ggplot to draw the scatterplot, mean_q1q6 is the x axis and mean_q8q10 is the y axis, organize the display
  ggplot(filtered_tbl(), aes(x = mean_q1q6, y = mean_q8q10)) +
    geom_point() + # add the points
    geom_smooth(method = "lm", # add the OLS regression line, below is to match the requirement, organize the display, make it consistent
                color = "purple", # make the line purple
                se = input$band_choice == "Display Error Band") + # show the error band when they choosse display error band
    scale_x_continuous(name = "Mean Scores on Q1-Q6") + # label x axis
    scale_y_continuous(name = "Mean Scores on Q8-Q10") # label y axis
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
