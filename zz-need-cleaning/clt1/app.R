library(shiny)


ui = fluidPage(
  
  titlePanel('Central Limit Theorem'),
  
  sidebarLayout(
    sidebarPanel(
      sliderInput('n',
                  'Sample size:',
                  min=1, max=50, step=1, value=1,
                  animate=animationOptions(interval=300,loop=FALSE))
    ),
    
    mainPanel(
      plotOutput("plot")
    )
  )
)


server = function(input, output) {
  
  output$plot <- renderPlot({
    
    x = seq(0, 20, 0.001)
    y = dchisq(x, df=2)
    z = dnorm(x, mean=2, sd=2/sqrt(input$n))
    
    getmeans = function(n){
      mean(rchisq(n, df=2))
    }
    
    means = replicate(n=1000, getmeans(input$n))
    
    hist(means, col='grey', border='white',
         xlab='',
         xlim=c(0,15),
         yaxt='n',
         ylab='',
         ylim=c(0,1.5),
         main=ifelse(input$n==1,'Population distribution',
                     paste('Sampling distribution for n =',input$n)),
         prob=TRUE)
    abline(h=0, col='grey')
    lines(x, y, col='blue')
    lines(density(means, from=0), col='red')
    lines(x, z, col='darkgreen')
    legend(x='topright',
           lty=1,
           col=c('blue','darkgreen','red'),
           legend=c('Population','Normal','Sampling distribution'),
           bty='n')
    axis(side=1, at=2)
  })
  
}


shinyApp(ui=ui, server=server)