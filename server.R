library(shiny)
library(markdown)

shinyServer(function(input, output, session) {

  #Plot of Standard Normal Distribution with t
  output$distPlot<-renderPlot({

    #get inputs
    df <- input$df

    #plotting sequence
    x<-seq(from = -6, to = 6, length = 50000)
    

    ##Graph of Z distribution
    plot(x = x, y = dnorm(x, mean = 0, sd = 1), type = 'l', 
         col = "blue", xaxt = "n", ylab = "PDF", 
         lwd = 3, xlab = "z or t Values", main = paste("Distribution of Z and \n t with", df, "Degrees of Freedom"))
    axis(side=1)
    
    ##Overlay t distribution
    lines(x = x, y = dt(x, df = df), type = "l", col = "Red", lwd = 3)
    
    legend(x = 2.75, y = 0.375, legend = c("Standard Normal", paste("t with", df, "df")), lty = 1, lwd = 3, col = c("blue", "red"))
    if(!input$prob) { 
      #add nothing
    } else { #add shading
      print(input$probL)
      if(input$probType == "Less" & !is.na(input$probL)){
        shortseq <- seq(from = -6, to = input$probL, length = 10000)
        polygon(c(shortseq, rev(shortseq)), c(rep(0, length(shortseq)), rev(dnorm(shortseq))),
                col = rgb(0.3, 0.1, 0.5, 0.3))
        polygon(c(shortseq, rev(shortseq)), c(rep(0, length(shortseq)), rev(dt(shortseq, df))),
                col = rgb(0.7, 0.1, 0.5, 0.3))
      } else if(input$probType == "Greater" & !is.na(input$probU)) {
        shortseq <- seq(from = input$probU, to = 6, length = 10000)
        polygon(c(shortseq, rev(shortseq)), c(rep(0, length(shortseq)), rev(dnorm(shortseq))),
                col = rgb(0.3, 0.1, 0.5, 0.3))
        polygon(c(shortseq, rev(shortseq)), c(rep(0, length(shortseq)), rev(dt(shortseq, df))),
                col = rgb(0.7, 0.1, 0.5, 0.3))
      } else if(input$probType == "Between" & !is.na(input$probLB) & !is.na(input$probUB)){
        shortseq <- seq(from = input$probLB, to = input$probUB, length = 10000)
        polygon(c(shortseq, rev(shortseq)), c(rep(0, length(shortseq)), rev(dnorm(shortseq))),
                col = rgb(0.3, 0.1, 0.5, 0.3))
        polygon(c(shortseq, rev(shortseq)), c(rep(0, length(shortseq)), rev(dt(shortseq, df))),
                col = rgb(0.7, 0.1, 0.5, 0.3))
      }
    }
  
  })
  

  #Latex of conversion
  output$text<-renderUI({

    df <- input$df 
    
    if(!input$prob){
      NULL
    } else {
      if(input$probType == "Less"){
        tprob <- round(pt(input$probL, df), 6)
        zprob <- round(pnorm(input$probL), 6)
        
        #output (must call withMathJax() on this since it is rendered dynamically: https://shiny.rstudio.com/gallery/mathjax.html)
        t <- withMathJax(paste("$$P\\left(Z\\leq", input$probL, "\\right) = ", zprob,
              "$$$$ P\\left(T_{", df, "} \\leq", input$probL, "\\right) = ", tprob, "$$"))
      } else if(input$probType == "Greater") {
        tprob <- round(pt(input$probU, df, lower.tail = FALSE), 6)
        zprob <- round(pnorm(input$probU, lower.tail = FALSE), 6)
        
        #output
        t <- withMathJax(paste("$$P\\left(Z\\geq", input$probU, "\\right) = ", zprob,
          "$$$$ P\\left(T_{", df, "} \\geq", input$probU, "\\right) = ", tprob, "$$"))
      } else {
        tprob <- round(pt(input$probUB, df) - pt(input$probLB, df), 6)
        zprob <- round(pnorm(input$probUB) - pnorm(input$probLB), 6)
        
        #output
        t <- withMathJax(paste("$$P\\left(", input$probLB, "\\leq Z\\leq", input$probUB, "\\right) = ", zprob, 
              "$$$$ P\\left(", input$probLB, "\\leq T_{", df, "} \\leq", input$probUB, "\\right) = ", tprob, "$$"))
      }
    }
    tagList(withMathJax(), t)
  })
  
})    