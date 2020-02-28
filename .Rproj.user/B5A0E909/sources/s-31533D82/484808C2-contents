#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(dplyr)
library(lubridate)
library(R0)

confirmed <- read.csv(url("https://raw.githubusercontent.com/CSSEGISandData/2019-nCoV/master/csse_covid_19_data/csse_covid_19_time_series/time_series_19-covid-Confirmed.csv"))

sums <- colSums(confirmed[,-match(c("Province.State", "Country.Region", "Lat", "Long"), names(confirmed))], na.rm=TRUE)
sumsByDateCode <- as.data.frame(t(t(sums)))
colnames(sumsByDateCode) <- c("count")
sumsByDateCode$datecode <- rownames(sumsByDateCode)
dailyCounts <- mutate(sumsByDateCode, date = mdy(substring(datecode,2)))
dailyCounts$day <- seq.int(nrow(dailyCounts))

mgt <- generation.time("gamma", c(8.4, 3.8))
est <- estimate.R(dailyCounts$count, methods=c("TD"), GT=mgt)


dailyCountAndPrediction <- merge(dailyCounts, est$estimates$TD$pred, by="row.names", sort=FALSE, all=TRUE)
dailyCountAndPrediction <- dailyCountAndPrediction[-c(1)]
names(dailyCountAndPrediction)[5] <- "TD"

# plot(dailyCountAndPrediction$day, dailyCountAndPrediction$count, xlab="days",ylab="count", col="red")
# lines(dailyCountAndPrediction$day, dailyCountAndPrediction$TD, col="green")
# legend(1, max(dailyCountAndPrediction$count)-10, legend=c("Actual", "TD"), col=c("red", "green"), lty=c(0,1), pch=c(1,NA), cex=0.8)

estimatedR <- est$estimates$TD$R[1:length(est$estimates$TD$R)-1]

estDf <- as.data.frame(estimatedR)
colnames(estDf) <- c("R")
estDf$day <- rownames(estDf)
lf <- loess(R ~ day, estDf)

#plot(estDf$day, estDf$R, xlab="days", ylab="R", ylim=c(0,20), yaxt="n")
#abline(h=1, col="gray60")
#axis(2, at=seq(0:max(estimatedR)))

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("COVID-19 Explorer"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
           # sliderInput("bins",
        #                "Number of bins:",
        #                min = 1,
        #                max = 50,
        #                value = 30)
        ),

        # Show a plot of the generated distribution
        mainPanel(
           # plotOutput("distPlot")
            plotOutput("dailyConfirmedPlot"),
            plotOutput("estimatedR")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    
    output$dailyConfirmedPlot <- renderPlot({
        plot(dailyCountAndPrediction$day, dailyCountAndPrediction$count, xlab="days",ylab="count", col="red")
        lines(dailyCountAndPrediction$day, dailyCountAndPrediction$TD, col="green")
        legend(1, max(dailyCountAndPrediction$count)-10, legend=c("Actual", "TD"), col=c("red", "green"), lty=c(0,1), pch=c(1,NA), cex=0.8)
    })
    
    output$estimatedR <- renderPlot({
        plot(estDf$day, estDf$R, xlab="days", ylab="R", ylim=c(0,20), yaxt="n")
        abline(h=1, col="gray60")
        axis(2, at=seq(0:max(estimatedR)))
    })

  #  output$distPlot <- renderPlot({
  #      # generate bins based on input$bins from ui.R
  #      x    <- faithful[, 2]
  #      bins <- seq(min(x), max(x), length.out = input$bins + 1)

#        # draw the histogram with the specified number of bins
#        hist(x, breaks = bins, col = 'darkgray', border = 'white')
#    })
}

# Run the application 
shinyApp(ui = ui, server = server)
