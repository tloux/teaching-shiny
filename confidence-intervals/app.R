library(shiny)


ui = fluidPage(
  
  titlePanel('Confidence Intervals'),
  
  sidebarLayout(
    sidebarPanel(
      numericInput('n', label='Sample size:', value=30, min=1),
      numericInput('level', label='Confidence level:', 
                   value=95, min=0.01, max=99.9),
      actionButton('resample', label='Resample')
    ),
    
    mainPanel(
      plotOutput("plot")
    )
  )
)


server = function(input, output){
  
  library(ggplot2)
  
  output$plot <- renderPlot({
    
    count = input$resample
    
    x = rnorm(n=input$n, mean=20, sd=5)
    mydat = data.frame(x)
    
    stats = t.test(x, conf.level=(input$level)/100)
    myint = data.frame(mn= mean(x), lo = stats$conf.int[1], hi=stats$conf.int[2])
    
    sample_cover = round(mean(stats$conf.int[1] < x & x < stats$conf.int[2]), 3)
    
    myplot = ggplot(data=mydat, aes(x=x)) + 
      geom_histogram(fill='lightblue', color='grey', bins=30) + 
      xlim(c(0,40)) + 
      ggtitle(paste('Proportion of sample covered by CI:', sample_cover)) + 
      theme_minimal()
    
    maxht = max(ggplot_build(myplot)[['data']][[1]][['count']])
    
    myplot = myplot + 
      geom_pointrange(data=myint, aes(x=mn, xmin=lo, xmax=hi, y=maxht/10), color='navy', size=1)
    
    myplot
    
  }, height = 600, width = 650)
  
}


shinyApp(ui=ui, server=server)