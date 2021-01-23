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
    titlePanel("Leverage and Influence"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            selectInput('obs', 
                        'Observation', 
                        choices=c('median','outlier'), 
                        selected='median'), 
            sliderInput("y",
                        "Y value",
                        min = 1,
                        max = 100,
                        value = 30)
        ),

        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("distPlot")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    
    library(ggplot2)
    
    x = runif(n=19, min=10, max=20)
    x = c(sort(x),25)
    y = rnorm(n=20, mean=-50 + 5*x, sd=8)
    
    mydat = data.frame(x,y,sel=FALSE)
    
    output$distPlot <- renderPlot({
        
        if(input$obs == 'median'){
            mydat$sel[10] = TRUE
            mydat$sel[20] = FALSE
            orig_y = y[10]
            mydat$y[10] = input$y
        }
        
        if(input$obs == 'outlier'){
            mydat$sel[20] = TRUE
            mydat$sel[10] = FALSE
            orig_y = y[20]
            mydat$y[20] = input$y
        }
        
        myreg = lm(y ~ x, data=mydat)
        myeqn = paste('y =', round(coef(myreg)[1],1), '+', round(coef(myreg)[2],1), 'x')
        
        ggplot(data=mydat, aes(x=x, y=y)) + 
            geom_point(aes(col=sel), alpha=0.5, size=4) + 
            stat_smooth(data=mydat, aes(x=x, y=y), method='lm') + 
            scale_color_manual(values=c('black','red')) + 
            xlim(c(5,30)) + 
            ylim(c(-10,100)) + 
            geom_text(x=5, y=90, label=myeqn, hjust='left', size=8) + 
            theme_minimal() + 
            theme(legend.position="none")
        
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
