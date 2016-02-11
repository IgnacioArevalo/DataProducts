FinishTime <- function (distance, pace) {
  time <- as.numeric(pace)*as.numeric(distance)/60
  s<-as.character(round(((time%%1*60)%%1)*60))
  m<-as.character((time%%1*60)%/%1)
  h<-as.character(time%/%1)
  paste(h,"hour(s)",m,"minute(s)",s, "second(s)",sep=" ")
}

Calories <- function (distance, pace) {
  time <- as.numeric(pace)*as.numeric(distance)/60
  calories <- round(((9-as.numeric(pace))/(9-2.25))*1500*time)
}
library(shiny)
shinyServer(
  function(input, output) {
    currentFinishTime <- reactive({ FinishTime(input$distance,input$pace) })
    currentCalories <- reactive({ Calories(input$distance,input$pace) })
    output$FinishTime <- renderText({currentFinishTime()})
    output$Calories <- renderText({currentCalories()})
    output$Histogram <- renderPlot({
      set.seed(123)
      hist(rnorm(10000,12,3), col='lightgrey', axes=TRUE, main  = "Long Distance Runners' Distribution", xlab = "Average Speed (km/h)", ylab="Frequency",breaks = 24)
      speed <- input$speed
      lines(c(speed,speed), c(0, 2000),col="red",lwd=5)
      percentile <- round(pnorm(speed,mean=12,sd=3)*100)
      text(3, 1000, paste("Speed = ", speed, "km/h"))
      text(3, 900, paste("Percentile = ", percentile,"%"))
      axis(side=1,at=c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24), 
          labels=c("1","2","3","4","5","6","7","8","9","10","11","12","13","14","15","16","17","18","19","20","21","22","23","24"))
      
      })
  }
  )