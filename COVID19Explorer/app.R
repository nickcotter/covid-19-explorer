library(shiny)
library(dplyr)
library(lubridate)
library(R0)

# load the latest data
confirmed <- read.csv(url("https://raw.githubusercontent.com/CSSEGISandData/2019-nCoV/master/csse_covid_19_data/csse_covid_19_time_series/time_series_19-covid-Confirmed.csv"))
confirmed <- subset(confirmed, select = -c(Lat, Long))    
countries <- unique(confirmed$Country.Region)
countries <- factor(append("Global", as.character(countries)))

# TODO make this configurable
mgt <- generation.time("gamma", c(5, 1.9))


getDailyCounts <- function(country) {
    
    filteredConfirmed <- confirmed %>%
        filter(country == "Global" | Country.Region == country)

    sums <- colSums(filteredConfirmed[,-match(c("Province.State", "Country.Region"), names(filteredConfirmed))], na.rm=TRUE)
    sumsByDateCode <- as.data.frame(t(t(sums)))
    colnames(sumsByDateCode) <- c("count")
    sumsByDateCode$datecode <- rownames(sumsByDateCode)
    dailyCounts <- mutate(sumsByDateCode, date = mdy(substring(datecode,2)))
    dailyCounts$day <- seq.int(nrow(dailyCounts))
    dailyCounts %>%
        filter(count > 0)
}

generateEstimate <-function(dailyCounts) {
    estimate.R(dailyCounts$count, methods=c("TD"), GT=mgt)
} 

getDailyPredictions <- function(dailyCounts, est) {
    dailyCountAndPrediction <- merge(dailyCounts, est$estimates$TD$pred, by="row.names", sort=FALSE, all=TRUE)
    dailyCountAndPrediction <- dailyCountAndPrediction[-c(1)]
    names(dailyCountAndPrediction)[5] <- "TD"
    dailyCountAndPrediction %>%
      mutate(countDiff = count - lag(count))
}

getEstimatedRByDay <- function(est) {
    estimatedR <- est$estimates$TD$R[1:length(est$estimates$TD$R)-1]
    estDf <- as.data.frame(estimatedR)
    colnames(estDf) <- c("R")
    estDf$day <- rownames(estDf)
    estDf
}

RMSE = function(m, o){
  sqrt(mean((m - o)^2))
}

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("COVID-19 Explorer"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
          
            helpText("Plot case counts and estimated reproduction number globally and by country for COVID-19"),
          
            selectInput("countries", "Country", countries),
            
            
            hr(),
            
            fluidRow(align="center", tableOutput("estimatedRSummary")),
            
            hr(),
          
            p("created by",
            a("Nick Cotter", href="https://nickcotter.com/")),
            
            a("GitHub Repo", href="https://github.com/nickcotter/covid-19-explorer")
        ),

        # Show a plot of the generated distribution
        mainPanel(
            plotOutput("estimatedR"),
            plotOutput("dailyConfirmedPlot"),
            plotOutput("dailyDiffPlot")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    
    reactiveDailyCounts <- reactive({
        getDailyCounts(input$countries)
    })

    reactiveEstimate <- reactive({
        generateEstimate(reactiveDailyCounts())
    })
    
    reactiveDailyCountAndPrediction <- reactive({
        tryCatch({
            getDailyPredictions(reactiveDailyCounts(), reactiveEstimate())
        }, error=function(e) {
            reactiveDailyCounts()
        })
    })
    
    reactiveEstimatedRByDay <- reactive({
        getEstimatedRByDay(reactiveEstimate())
    })

    output$dailyConfirmedPlot <- renderPlot({
        
        dailyCountAndPrediction <- reactiveDailyCountAndPrediction()
        
        plot(dailyCountAndPrediction$day, dailyCountAndPrediction$count, xlab="days",ylab="count", col="red", main="Confirmed Case Count")
        
        if("TD" %in% names(dailyCountAndPrediction)) {
            lines(dailyCountAndPrediction$day, dailyCountAndPrediction$TD, col="green")
        }
        
        legend(1, max(dailyCountAndPrediction$count)-10, legend=c("Actual", "Estimated"), col=c("red", "green"), lty=c(0,1), pch=c(1,NA), cex=0.8)
    })
    
    output$dailyDiffPlot <- renderPlot({
      
      dailyCountAndPrediction <- reactiveDailyCountAndPrediction()
      
      barplot(dailyCountAndPrediction$countDiff ~ dailyCountAndPrediction$day, xlab="days", ylab="difference", main="Confirmed Case Count Increase")
    })
    
    output$estimatedR <- renderPlot({
        
        tryCatch({
            estimatedRByDay <- reactiveEstimatedRByDay()
            plot(estimatedRByDay$day, estimatedRByDay$R, xlab="days", ylab="R", ylim=c(0,20), pch=3, main="Estimated R")
            abline(h=1, col="gray60")
        }, error=function(e) {})
    })
    
    output$estimatedRSummary <- renderTable({
      
      tryCatch({
        est <- reactiveEstimate()
        estimatedR <- est$estimates$TD$R[1:length(est$estimates$TD$R)-1]
        as.array(summary(estimatedR))
      }, error=function(e) {
      })
    
    }, colnames = FALSE, caption="Estimated R Range", caption.placement = getOption("xtable.caption.placement", "top"))
}

# Run the application 
shinyApp(ui = ui, server = server)
