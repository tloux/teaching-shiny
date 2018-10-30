#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

# Define UI for application that draws a histogram
ui <- fluidPage(
   
   # Application title
   titlePanel("Diagnostic tests"),
   
   # Sidebar with a slider input for number of bins 
   sidebarLayout(
      sidebarPanel(
         sliderInput("cutoff",
                     "Diagnostic cutoff:",
                     min = 1,
                     max = 10,
                     value = 1, 
                     step=0.1, 
                     animate=animationOptions(interval=500))
      ),
      
      # Show a plot of the generated distribution
      mainPanel(
         plotOutput("distPlot")
      )
   )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
   
  sensitivity = function(dat, group, cut){
    mean(dat[group==1] >= cut)
  }
  
  specificity = function(dat, group, cut){
    mean(dat[group==0] < cut)
  }
  
  grp = rep(0:1, each=50)
  y = rnorm(n=100, mean=5+2*grp, sd=1)
  
  roc_range = c(floor(min(y)), ceiling(max(y)))
  roc_steps = seq(roc_range[1], roc_range[2], by=0.01)
  
  output$distPlot <- renderPlot({
    
    par(mfrow=c(1,2))
    
    set.seed(1)
    plot(jitter(grp), y, col=ifelse(y<input$cutoff, 'steelblue', 'darkred'), 
         xaxt='n', xlab=paste('Specificity:', specificity(y, grp, input$cutoff), 
                              '; Sensitivity:', sensitivity(y, grp, input$cutoff)), 
         ylab='Biomarker expression', 
         main='Biomarker scatterplot', 
         frame=FALSE)
    axis(side=1, at=0:1, lwd=0, labels=c('True\nHealthy', 'True\nDiseased'))
    text(x=rep(0.5,2), y=c(min(4,input$cutoff), max(8,input$cutoff)),
         pos=c(1,3), 
         labels=c('Test\nNegative', 'Test\nPositive'), 
         col=c('steelblue','darkred'), xpd=TRUE)
    abline(h=input$cutoff, col='red', lty=2, lwd=2, xpd=TRUE)
    
    
    roc_curve = sapply(roc_steps, function(ct){
      sens = sensitivity(dat=y, group=grp, cut=ct)
      spec = specificity(dat=y, group=grp, cut=ct)
      ret = c(sens, spec)
      return(ret)
    })
    
    plot(x=1-roc_curve[2, ], y=roc_curve[1, ], type='s', 
         xaxt='n', xlab='Specificity', 
         ylab='Sensitivity', 
         main='ROC curve', 
         frame=FALSE)
    axis(side=1, at=seq(0,1,0.2), labels=seq(1,0,-0.2))
    abline(a=0, b=1, col='grey')
    
    m = which(roc_steps == input$cutoff)
    points(x=1-roc_curve[2, m], y=roc_curve[1, m], col='red', pch='X', cex=1.5)
    
    
  })
}

# Run the application 
shinyApp(ui = ui, server = server)

