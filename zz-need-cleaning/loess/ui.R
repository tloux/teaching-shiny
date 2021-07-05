library(shiny)

# Define UI for application that draws a histogram
shinyUI(fluidPage(

  # Application title
  titlePanel('Loess'),

  # Sidebar with a slider input for the number of bins
  sidebarLayout(
    sidebarPanel(
      sliderInput('alpha', 'Smoothing parameter:', value=0.2, min=0, max=1, step=0.01,
                  animate=animationOptions(interval=500,loop=FALSE)),
      radioButtons('deg', label = h3('Degree'),
                   choices = list("1" = 1, "2" = 2), 
                   selected = 1)
    ),

    # Show a plot of the generated distribution
    mainPanel(
      plotOutput("plot")
    )
  )
))
