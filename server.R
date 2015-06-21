library(shiny)
source("qs.R")
clean.gd()

shinyServer(function(input, output) {
  
 
  mean.qs()
  output$text1 <- renderText({ 
    paste("You have selected Dates", input$daterange[1], " through ", input$daterange[2])
  })
  output$text2 <- renderText({
    format(input$daterange[1])
  })
  output$graph1 <- renderPlot({
    date.qs$Date <- mdy(date.qs$Date, tz="")
    date1 <- format(input$daterange[1], tz="")
    date2 <- format(input$daterange[2], tz="")
    #date1 <- daterange
    data1 <- date.qs[date.qs$Date <= date2 & date.qs$Date >= date1,]
    ggplot(data=data1, aes(x=Date, colour=variable))+geom_line(aes(y=value,group=variable))+geom_line(aes(y=value,group=variable))+geom_point(aes(y=value, group=variable))
    })
})