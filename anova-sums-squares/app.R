library(shiny)


ui = fluidPage(
  
  titlePanel('One-way ANOVA sums of squares'),
  
  sidebarLayout(
    sidebarPanel(
      checkboxInput('showcis', 'Show 95% confidence intervals', value=FALSE), 
      sliderInput('ssb', 'SSB:',
                  value=0, min=0, max=30, step=1,
                  animate=animationOptions(interval=1000,loop=FALSE)),
      sliderInput('sse', 'SSW (SSE):',
                  value=50, min=1, max=150, step=1,
                  animate=animationOptions(interval=1000,loop=FALSE))
    ),
    
    mainPanel(
      plotOutput("plot")
    )
  )
)


server = function(input, output){
  
  library(ggplot2)
  library(ggpubr)
  
  grp = rep(1:4, each=20)
  plot_grp = jitter(grp)
  errs = rnorm(80, mean=0, sd=1)
  errs = c(scale(errs[1:20]), scale(errs[21:40]), 
           scale(errs[41:60]), scale(errs[61:80]))
  
  
  output$plot <- renderPlot({
    
    
    # create data
    
    k_between = sqrt(input$ssb / 50)
    
    mn1 = 10 + 0.5*k_between
    mn2 = 10 - 1*k_between
    mn3 = 10 + 1*k_between
    mn4 = 10 - 0.5*k_between
    mn = rep(c(mn1, mn2, mn3, mn4), each=20)
    
    y = mn + sqrt(input$sse/76)*errs
    
    mydat = data.frame(grp, plot_grp, y)
    
    
    # scatterplot
    
    ggscatter = ggplot(data=mydat, aes(x=plot_grp, y=y)) +
      geom_point(size=3, alpha=0.2) +
      geom_hline(yintercept=mean(mydat$y), col='blue', linetype=2) +
      xlab('Group') +
      ylab('Outcome') +
      ylim(c(5,15)) +
      ggtitle('Raw data') +
      theme_minimal()
    
    if(input$showcis){

      get_ci = function(x){
        c(mean(x), t.test(x)$conf.int)
      }
      
      mycis = aggregate(y~grp, data=mydat, FUN=get_ci)
      mycis = data.frame(grp=mycis$grp, est=mycis$y[, 1], 
                         lo=mycis$y[, 2], hi=mycis$y[, 3])
      
      ggscatter = ggscatter + 
        geom_point(data=mycis, aes(x=grp, y=est), color='steelblue', size=2) +
        geom_errorbar(data=mycis, aes(x=grp, y=est, ymin=lo, ymax=hi),
                      color='steelblue', width=0.1, size=1)
    }else{
      
      grpmeans = aggregate(y~grp, data=mydat, FUN=mean)
      grpmeans$xstart = grpmeans$grp - 0.2
      grpmeans$xend = grpmeans$grp + 0.2
      
      ggscatter = ggscatter + 
        geom_linerange(data=grpmeans, aes(y=y, x=grp, xmin=xstart, xmax=xend), 
                       col='steelblue', size=1)
        
      
    }

    
    # F distribution curve with F statistic
    
    f = (input$ssb/3) / (input$sse/(4*(20-1)))
    fx = min(f, 6)
    flab = paste('F =',round(f,2))
    px = ifelse(fx==6, 6, max(2, f))
    py = mean(c(df(2, df1=3, df2=4*(20-1)),df(px, df1=3, df2=4*(20-1))))
    pval = 1 - pf(f, df1=3, df2=4*(20-1))
    plab = paste('p =',round(pval,3))
    
    
    ggfcurve = ggplot(data=NULL) + 
      geom_hline(yintercept=0, color='grey') + 
      stat_function(fun=df, args=list(df1=3, df2=4*(20-1)), size=1) + 
      geom_point(data=data.frame(x=f, y=0), aes(x=x,y=y), 
                 size=3, color='blue', shape=17) + 
      geom_text(data=data.frame(x=c(fx,px),y=c(-0.05,py+0.05),lab=c(flab,plab)), 
                aes(x=x, y=y, label=lab), color='blue', fontface=2) + 
      xlab('F') + 
      xlim(c(0,6)) + 
      ylab('') + 
      scale_y_continuous(breaks=NULL) + 
      ggtitle('F distribution') + 
      theme_minimal()
    
    
    ggarrange(ggscatter, ggfcurve, ncol=1, nrow=2)
    
  }, height = 600, width = 600)
}


shinyApp(ui=ui, server=server)