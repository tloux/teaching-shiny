library(shiny)


ui = fluidPage(
  
  titlePanel('t Distribution'),
  
  sidebarLayout(
    sidebarPanel(
      sliderInput('df', label="Degrees of freedom:", min=1, max=50, 
                  value=1, step=1, 
                  animate=animationOptions(interval=300)), 
      checkboxInput('norm', label='Show standard normal curve', value=FALSE)
    ),
    
    mainPanel(
      plotOutput("plot")
    )
  )
)


server = function(input, output){
  
  library(ggplot2)
  
  output$plot <- renderPlot({
    
    myplot = ggplot(data=NULL) + 
      geom_hline(yintercept=0, col='grey') + 
      stat_function(fun=dt, args=list(df=input$df), color='navy', size=1) + 
      xlab('t') + 
      xlim(c(-4,4)) + 
      ylab('') + 
      scale_y_continuous(breaks=NULL, limits=c(0,0.4)) + 
      theme_minimal()
    
    if(input$norm){
      myplot = myplot + 
        stat_function(fun=dnorm, args=list(mean=0, sd=1), color='steelblue', size=1)
    }
    
    myplot
    
  })
  
}


shinyApp(ui=ui, server=server)