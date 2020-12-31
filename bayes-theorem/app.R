library(shiny)


ui = fluidPage(
  
  titlePanel("Bayes' Theorem"),
  
  sidebarLayout(
    sidebarPanel(
      sliderInput('p_dis', 'Prevalence of disease in population:',
                  value=0.10, min=0, max=1, step=0.01,
                  animate=animationOptions(interval=300,loop=FALSE)),
      sliderInput('sens', 'Sensitivity:',
                  value=0.90, min=0, max=1, step=0.01,
                  animate=animationOptions(interval=300,loop=FALSE)),
      sliderInput('spec', 'Specificity:',
                  value=0.80, min=0, max=1, step=0.01,
                  animate=animationOptions(interval=300,loop=FALSE))
    ),
    
    mainPanel(
      plotOutput("plot")
    )
  )
)


server = function(input, output){
  
  output$plot <- renderPlot({
    
    true_pos = input$p_dis * input$sens
    false_neg = input$p_dis * (1 - input$sens)
    true_neg = (1 - input$p_dis) * input$spec
    false_pos = (1 - input$p_dis) * (1 - input$spec)
    
    mat = matrix(c(true_pos, false_neg, false_pos, true_neg), byrow=TRUE, nrow=2)
    rownames(mat) = c('Disease', 'No disease')
    colnames(mat) = c('Positive', 'Negative')
    
    mid_x = matrix(rep(c(input$p_dis/2, (1+input$p_dis)/2), each=2), 
                   byrow=TRUE, ncol=2) + 0.01
    mid_y = matrix(c((2-input$sens)/2, (1-input$sens)/2, 
                     (1+input$spec)/2, input$spec/2), 
                   byrow=TRUE, ncol=2) - 0.02
    
    ppp = true_pos / (true_pos + false_pos)
    
    par(mar=c(4,2,2,1))
    mosaicplot(mat, col=c('pink3','lightblue3'), border='transparent', 
               main='', xlab='True disease status', ylab='Test result')
    
    text(x=mid_x, y=mid_y, labels=round(mat,3))
    
    title(main=paste('Positive predictive probability:',round(ppp,3)))
  }, height = 600, width = 650)
  
}


shinyApp(ui=ui, server=server)