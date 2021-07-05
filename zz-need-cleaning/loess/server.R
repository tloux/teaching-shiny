library(shiny)


shinyServer(function(input, output) {


  N = 75
  x = runif(n=N, min=0, max=100)
  y = rnorm(n=N, mean=0.25*x + 15*sin(x/10) + log(x), sd=20)
    
  
  output$plot <- renderPlot({
    
    ## fit data and residuals
    fit = loess(y ~ x, span=input$alpha, degree=input$deg)
    resfit = loess(fit$residuals ~ x, span=0.5, degree=2)
    
    
    par(mfrow=c(2,2))
    
    ## plot data
    plot(x, y, 
         main='Scatterplot of data',
         bty='n')
    lines(x=sort(fit$x), y=fit$fitted[order(fit$x)], 
          col='blue')
    
    ## plot residuals
    plot(x, fit$residuals,
         ylab='Loess residuals',
         main='Residual plot',
         bty='n')
    abline(h=0, col='grey')
    lines(x=sort(resfit$x), y=resfit$fitted[order(resfit$x)], col='red')
    
    ## residuals histogram
    h = hist(fit$residuals, prob=TRUE,
             col='grey', border='white',
             xlab='Residuals',
             main='Residuals histogram')
    z = seq(h$breaks[1], h$breaks[length(h$breaks)], length.out=100)
    dz = dnorm(z, mean=0, sd=sd(fit$residuals))
    lines(z, dz, col='blue')
    lines(density(fit$residuals), col='darkgreen')
    
    ## residuals qq
    qqnorm(fit$residuals, 
           main='Residuals QQ plot',
           bty='n')
    qqline(fit$residuals)
    
    
  })
      
})
