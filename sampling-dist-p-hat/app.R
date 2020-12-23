library(shiny)


ui = fluidPage(
  
  titlePanel('Sampling Distribution of "p-hat"'),
  
  sidebarLayout(
    sidebarPanel(
      sliderInput('size', label = "Size:", min=1, max=200, 
                  value=1, step=1, 
                  animate=animationOptions(interval=500)),
      sliderInput('prob', label = "True p:", min=0, max=1, 
                  value=0.5, step=0.01), 
      checkboxInput('norm', label="Show normal approximation", value=FALSE)
    ),
    
    mainPanel(
      plotOutput("plot")
    )
  )
)


server = function(input, output){
  
  library(ggplot2)
  
  output$plot <- renderPlot({
    
    y = 0:input$size
    py = dbinom(y, size=input$size, prob=input$prob)
    
    mydat = data.frame(y=y/input$size, py)
    
    myplot = ggplot(data=mydat, aes(x=y, y=py)) + 
      geom_point(color='navy', size=2) + 
      geom_segment(data=mydat, aes(x=y, xend=y, y=0, yend=py), color='navy', size=1) + 
      xlab('Sample proportion') + 
      ylab('Probability of observing proportion') + 
      ylim(c(0, NA)) + 
      theme_minimal()
    
    if(input$norm){
      mynorm = function(x, ...) dnorm(x, ...) / input$size
      
      myplot = myplot + 
        stat_function(fun=mynorm,
                      args=list(mean=input$prob,
                                sd=sqrt(input$prob * (1-input$prob) / input$size)), 
                      color='steelblue', size=1)
    }
    
    myplot
    
  })
  
}


shinyApp(ui=ui, server=server)