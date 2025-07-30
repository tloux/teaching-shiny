#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

library(ggplot2)

# Define UI for application that draws a histogram
ui <- fluidPage(
   
   # Application title
   titlePanel("Diagnostic tests"),
   
   # Sidebar with a slider input for number of bins 
   sidebarLayout(
      sidebarPanel(
         sliderInput("cutoff",
                     "Diagnostic cutoff:",
                     min = 2,
                     max = 10,
                     value = 1, 
                     step=0.1, 
                     animate=animationOptions(interval=750))
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
    mean(dat[group=='Sick'] >= cut)
  }
  
  specificity = function(dat, group, cut){
    mean(dat[group=='Healthy'] < cut)
  }
  
  grp = rep(0:1, each=50)
  set.seed(2)
  marker = rnorm(n=100, mean=5+2*grp, sd=1)
  grp = ifelse(grp==1, 'Sick', 'Healthy')
  biom = data.frame(grp, marker)

  roc_steps = seq(floor(min(marker)), ceiling(max(marker)), by=0.001)
  
  roc = data.frame(
    sens = sapply(roc_steps, function(x) sensitivity(dat=biom$marker, 
                                                     group=biom$grp, cut=x)), 
    spec = sapply(roc_steps, function(x) specificity(dat=biom$marker, 
                                                     group=biom$grp, cut=x)))
  
  output$distPlot <- renderPlot({
    
    biom$`Test decision` = factor(ifelse(biom$marker>input$cutoff, 'Positive', 'Negative'), 
                                  levels=c('Positive', 'Negative'))
    
    set.seed(1)
    p1 = ggplot(data=biom, aes(x=grp, y=marker, color=`Test decision`)) + 
      geom_jitter(width=0.2, alpha=0.5) + 
      geom_hline(yintercept=input$cutoff, color='red', linetype=2) + 
      ggtitle('Biomarker scatterplot') + 
      xlab('Patient condition') +
      ylab('Biomarker expression') + 
      ylim(2,10) + 
      theme_minimal() + 
      theme(legend.position="top")
    
    
    # ROC curve
    
    p2 = ggplot(data=roc, aes(x=spec, y=sens)) + 
      geom_path() + 
      scale_x_reverse() + 
      annotate(x=specificity(dat=biom$marker, 
                             group=biom$grp, cut=input$cutoff), 
               y=sensitivity(dat=biom$marker, 
                             group=biom$grp, cut=input$cutoff), 
               geom='point', pch='X', color='red', size=7, alpha=0.5) + 
      ggtitle('ROC curve') + 
      xlab('Specificity (scale reversed)') + 
      ylab('Sensitivity') + 
      theme_minimal()
    
    
    p3 = gridExtra::grid.arrange(p1, p2, ncol=2)
    p3
    
  })
}

# Run the application 
shinyApp(ui = ui, server = server)

