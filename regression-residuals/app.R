library(shiny)


ui = fluidPage(
  
  titlePanel('Regression errors'),
  
  sidebarLayout(
    sidebarPanel(
      sliderInput('x0', 'x:',
                  value=15, min=10, max=30, step=0.1,
                  animate=animationOptions(interval=150,loop=FALSE)), 
      sliderInput('caliper', 'Window:', 
                  value=0.25, min=0.05, max=3, step=0.05), 
      checkboxInput('show_curve', 'Residual normal curve', value=FALSE)
    ),
    
    mainPanel(
      plotOutput("plot")
    )
  )
)


server = function(input, output){
  
  nobs = 100
  x = runif(nobs, min=10, max=30)
  sigma = 5
  f = function(x) 5 + 2*x
  y = rnorm(nobs, mean=f(x), sd=sigma)
  
  yhist = hist(y, breaks=12, plot=FALSE)
  y_low = min(yhist$breaks)
  y_hi = max(yhist$breaks)
  
  
  output$plot <- renderPlot({
    
    caliper = input$caliper * sd(x)
    
          
    ## set plot zones
    
    zones <- matrix(c(1,1,1, 2,5,4, 0,3,0), ncol = 3, byrow = TRUE)
    layout(zones, widths=c(0.5,4,1), heights = c(1,10,.75))
    
    
    ## for all three titles: 
    ## drop the axis titles and omit boxes, set up margins
    
    par(xaxt='n', yaxt='n', bty='n',  mar = c(.3,2,.3,0) +.05)
    
    # fig 1: plot title
    plot(x=1, y=1, type='n',ylim=c(-1,1), xlim=c(-1,1))
    text(x=0, y=0, 'Regression and residual histogram', cex=2)
    
    # fig 2: y label
    plot(x=1, y=1, type='n', ylim=c(-1,1), xlim=c(-1,1))
    text(x=0, y=0, 'y', cex=1.5, srt=90)
    
    # fig 3: xlabel
    plot(x=1, y=1, type='n', ylim=c(-1,1), xlim=c(-1,1))
    text(x=0, y=0, 'x', cex=1.5)
    
    
    ## fig 4: histogram - needs different margins
    
    x_low = input$x0 - caliper
    x_hi = input$x0 + caliper
    
    y0_hist = hist(y[x_low < x & x < x_hi], breaks=yhist$breaks, plot=FALSE)
    
    # no margin on the left
    par(mar = c(2,0,1,1))
    barplot(yhist$counts, axes = FALSE, xlim=c(0,max(yhist$counts)), 
            col='grey', border='white', 
            space = 0, horiz = TRUE)
    
    # empirical error dist
    b0 = barplot(y0_hist$counts, axes=FALSE, xlim=c(0,max(yhist$counts)), 
                 col='lightblue', border='white', 
                 space=0, horiz=TRUE, add=TRUE)
    
    # theoretical error dist
    if(input$show_curve){
      norm_x = seq(min(y0_hist$mids), max(y0_hist$mids), length.out=100)
      norm_y = dnorm(norm_x, mean=f(input$x0), sd=sigma)
      
      curve_x = seq(min(b0), max(b0), length.out=100)
      curve_y = norm_y * max(y0_hist$counts)/dnorm(f(input$x0),f(input$x0),sigma)
      lines(x=curve_y, y=curve_x, col='blue')
    }
    
    
    ## fig 5: scatterplot -- needs regular axes, different margins
    
    par(mar=c(2,2,0.5,0.5), xaxt='s', yaxt='s', bty='n')
    
    plot(x, y, xlim=c(8,32), ylim=range(y0_hist$breaks), 
         pch=19, col='#00000022')
    
    polygon(x=c(x_low,x_hi,x_hi,x_low), y=c(y_low,y_low,y_hi,y_hi), 
            col='lightblue', border='lightblue')
    
    lines(x=rep(input$x0,2), y=c(min(y0_hist$breaks),y_hi), 
          lty=2, col='blue')
    points(x=input$x0, y=min(y0_hist$breaks), 
           pch=17, col='blue', cex=2)
    
    points(x, y, pch=19, col='#00000022')
    
    
  }, height = 600, width = 600)
  
}


shinyApp(ui=ui, server=server)