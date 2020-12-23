library(shiny)


ui = fluidPage(
  
  titlePanel('Chi-squared Distribution'),
  
  sidebarLayout(
    sidebarPanel(
      sliderInput('df', label = "Degrees of freedom:", min=1, max=10, 
                  value=1, step=1, 
                  animate=animationOptions(interval=500))
    ),
    
    mainPanel(
      plotOutput("plot")
    )
  )
)


server = function(input, output){
  
  library(ggplot2)
  
  output$plot <- renderPlot({
    
    ggplot(data=NULL) + 
      geom_hline(yintercept=0, col='grey') + 
      stat_function(fun=dchisq, args=list(df=input$df), color='navy', size=1) + 
      xlab(expression(X^2)) + 
      xlim(c(0,25)) + 
      ylab('') + 
      scale_y_continuous(breaks=NULL) + 
      theme_minimal()
    
  })
  
}


shinyApp(ui=ui, server=server)