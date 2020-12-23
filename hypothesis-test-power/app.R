library(shiny)


ui = fluidPage(
  
  titlePanel('Hypothesis Test Power'),
  
  sidebarLayout(
    sidebarPanel(
      numericInput('p', label='Hypothesized population proportion:', 
                   value=0.5, min=0, max=1, step=0.01),
      numericInput('alpha', label='Alpha:',
                   value=0.05, min=0.01, max=0.20, step=0.01),
      numericInput('n', label='Sample size:', 30, step=1),
      checkboxInput('showpower', label='Show alternative and power', 
                    value=FALSE), 
      numericInput('p1', label='True population proportion:',
                   value=0.4, min=0, max=1, step=0.01),
      sliderInput('xbounds', 'Range:',
                  min = 0, max = 1, step=0.01, value = c(0,1))
    ),
    
    mainPanel(
      plotOutput("plot")
    )
  )
)


server = function(input, output){
  
  library(ggplot2)
  
  output$plot <- renderPlot({
    
    p = as.numeric(input$p)
    se = sqrt(p * (1 - p) / input$n)
    
    crit = qnorm(input$alpha, mean=p, sd=se)
    
    x = seq(0, 1, 0.001)
    y = dnorm(x, mean=p, sd=se)
    mydat = data.frame(x=x[x>=crit], y=y[x>=crit])
    mydat_rej = data.frame(x=x[x<=crit], y=y[x<=crit])
    
    if(input$showpower){
      
      p1 = as.numeric(input$p1)
      se1 = sqrt(p1 * (1 - p1) / input$n)
      pwr = pnorm(crit, mean=p1, sd=sqrt(p1*(1-p1)/input$n))
      
      y1 = dnorm(x, mean=p1, sd=se1)
      mydat1 = data.frame(x=x[x<=crit], y=y1[x<=crit])
      
      myplot = ggplot(data=mydat, aes(x=x, y=y)) + 
        geom_area(fill='lightblue') + 
        stat_function(fun=dnorm, args=list(mean=p, sd=se), 
                      xlim=c(crit,1), col='blue', size=1) + 
        geom_area(data=mydat1, aes(x=x, y=y), fill='yellow') + 
        geom_area(data=mydat_rej, aes(x=x, y=y), fill='pink') + 
        stat_function(fun=dnorm, args=list(mean=p, sd=se), 
                      xlim=c(0,crit), col='red', size=1) + 
        stat_function(fun=dnorm, args=list(mean=p1, sd=se1), 
                      color='orange', size=1) + 
        xlab('') + 
        xlim(input$xbounds) + 
        scale_y_continuous(breaks=NULL) + 
        ylab('') + 
        ggtitle(label=paste('Reject if sample proportion below',round(crit,3)), 
                sub=paste('Power:', round(pwr,3))) + 
        theme_minimal()
      
      
    }else{
      
      myplot = ggplot(data=mydat, aes(x=x, y=y)) + 
        geom_area(fill='lightblue') + 
        stat_function(fun=dnorm, args=list(mean=p, sd=se), 
                      xlim=c(crit,1), col='blue', size=1) + 
        geom_area(data=mydat_rej, aes(x=x, y=y), fill='pink') + 
        stat_function(fun=dnorm, args=list(mean=p, sd=se), 
                      xlim=c(0,crit), col='red', size=1) + 
        xlab('') + 
        xlim(input$xbounds) + 
        scale_y_continuous(breaks=NULL) + 
        ylab('') + 
        ggtitle(paste('Reject if sample proportion below',round(crit,3))) + 
        theme_minimal()
      
    }
    
    myplot
    
  })
  
}


shinyApp(ui=ui, server=server)