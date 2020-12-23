library(shiny)

ui <- fluidPage(
   
   titlePanel("Parameters of a Normal Distribution"),
   
   sidebarLayout(
      sidebarPanel(
         sliderInput("mu",
                     "Mean (mu):",
                     min = -2,
                     max = 2,
                     value = 0, 
                     step = 0.1, 
                     animate=animationOptions(interval=500)),
         sliderInput("sig", 
                     "Standard deviation (sigma):", 
                     min = 0.5, 
                     max = 2, 
                     value = 1, 
                     step = 0.1, 
                     animate=animationOptions(interval=500))
      ),
      
      mainPanel(
         plotOutput("distPlot")
      )
   )
)


server <- function(input, output) {
   
   library(ggplot2)
   
   output$distPlot <- renderPlot({
      
      ggplot(data=NULL) + 
         stat_function(fun=dnorm, args=list(mean=0,sd=1), 
                       col='steelblue', size=1, xlim=c(-4,4)) + 
         stat_function(fun=dnorm, args=list(mean=input$mu,sd=input$sig), 
                       col='navy', size=2, n=1600, xlim=c(input$mu-4*input$sig, input$mu+4*input$sig)) + 
         xlim(c(-8,8)) + 
         ylim(c(0,0.8)) + 
         theme_minimal()
      
   })
}

shinyApp(ui = ui, server = server)

