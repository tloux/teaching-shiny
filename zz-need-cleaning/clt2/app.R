library(shiny)


ui = fluidPage(
  
  titlePanel('Central Limit Theorem'),
  
  sidebarLayout(
    sidebarPanel(
      sliderInput('n_obs', 'Sample size (n):',
                  value=30, min=1, max=100, step=1),
      numericInput('n_samp', "Number of samples to take:", 
                   value=1, min=1, max=1000, step=1),
      actionButton('action', 'Take samples', icon('bar-chart'))
    ),
    
    mainPanel(
      plotOutput("plot")
    )
  )
)


server = function(input, output){
  
  means = numeric(0)
  
  
  output$plot <- renderPlot({
    input$action
    
    hbreaks = 3 + c(-10:10)*sqrt(6)/sqrt(input$n_obs)
    
    
    for(i in 1:input$n_samp){
      x = rchisq(n=input$n_obs, df=3)
      means = c(means, mean(x))
    }
    
    par(mfrow=c(2,1))
    
    hist(x, 
         xlab='', xlim=c(0,15), 
         yaxt='n', ylab='', 
         col='lightblue', border='white', 
         main=paste('Histogram of most recent sample of',length(x)), 
         prob=TRUE)
    points(x=mean(x), y=0, pch=25, col='blue', bg='blue', cex=2, xpd=TRUE)
    text(x=mean(x), y=0, labels=round(mean(x),1), col='blue', pos=3)
    legend(x='topright',
           pch=25, lty=0, col='blue', pt.bg='blue',
           legend='Sample mean',
           bty='n')
    
    hist(means, breaks=hbreaks,
         xlab='', xlim=c(0,15), 
         yaxt='n', ylab='', 
         col='pink', border='white', 
         main=paste('Histogram of all',length(means),'sample means'), 
         prob=TRUE)
    curve(dnorm(x,mean=3,sd=sqrt(6/input$n_obs)), col='red', 
          from=3-4*sqrt(6/input$n_obs), to=3+4*sqrt(6/input$n_obs), add=TRUE)
    legend(x='topright',
           lty=1:0, col='red',
           legend=c('Normal distribution','(by CLT)'),
           bty='n')
    
  }, height = 600, width = 600)
  
}


shinyApp(ui=ui, server=server)