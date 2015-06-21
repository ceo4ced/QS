library(shiny)

# Define UI for application that draws a histogram
shinyUI(fluidPage(
  
  # Application title
  titlePanel("Quality Score Visualizer!"),
  
  # Sidebar with a slider input for the number of bins
  sidebarLayout(
    sidebarPanel(
      h3("Pick a Date Range"),
      p("Dates must be between May 27, 2015 and Yesterday"),
      dateRangeInput (
        "daterange", "Date range:",
        start = "5/27/2015",
        end = Sys.Date()-1, format="m/d/yyyy")
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      h3("Non & Weighted Quality Scores Daily"),
      textOutput("text1"),
      plotOutput("graph1")
    )
  )
))