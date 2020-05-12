library(shiny)
library(dplyr)
library(lubridate)
library(R0)

# load the latest data
confirmed <- read.csv(url("https://raw.githubusercontent.com/CSSEGISandData/2019-nCoV/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_global.csv")) %>%
              dplyr::select(-c(Lat, Long, ))   
countries <- unique(confirmed$Country.Region)
countries <- factor(append("Global", as.character(countries)))

getGenerationTime <- function(m, s) {
  generation.time("gamma", c(m, s))
}

getDailyCounts <- function(country) {
    
    filteredConfirmed <- confirmed %>%
        filter(country == "Global" | Country.Region == country)

    sums <- as.integer(colSums(filteredConfirmed[,-match(c("Province.State", "Country.Region"), names(filteredConfirmed))], na.rm=TRUE))
    sumsByDateCode <- as.data.frame(sums)
    rm(sums)
    rm(filteredConfirmed)
    
    colnames(sumsByDateCode) <- c("count")
    sumsByDateCode$datecode <- rownames(sumsByDateCode)
    
    dailyCounts <- mutate(sumsByDateCode, date = mdy(substring(datecode,2)))
    rm(sumsByDateCode)
    
    gc()
    
    dailyCounts$day <- seq.int(nrow(dailyCounts))
    dailyCounts %>%
        dplyr::select(-c("datecode")) %>%
        filter(count > 0)
}

generateEstimate <-function(dailyCounts, mgt) {
  estimate.R(dailyCounts$count, methods=c("TD"), GT=mgt)$estimates$TD
} 

getDailyPredictions <- function(dailyCounts, est) {
    dailyCountAndPrediction <- merge(dailyCounts, est$pred, by="row.names", sort=FALSE, all=TRUE) %>%
      dplyr::select(-c("Row.names")) %>%
      dplyr::rename(TD = y) %>%
      mutate(countDiff = count - lag(count))
}

getEstimatedRByDay <- function(est) {
    estimatedR <- est$R[1:length(est$R)-1]
    #estimatedR <- est$estimates$TD$R[1:length(est$estimates$TD$R)-1]
    estDf <- as.data.frame(estimatedR)
    rm(estimatedR)
    gc()
    colnames(estDf) <- c("R")
    estDf$day <- as.numeric(rownames(estDf))
    estDf
}

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("COVID-19 Explorer"),

    # Sidebar with a country selector 
    sidebarLayout(
        sidebarPanel(
          
            helpText("Plot cases and effective reproduction number globally or by country for COVID-19"),
          
            selectInput("countries", "Country", countries),
            
            fluidRow(align="center", 
                     helpText("Estimated Latest R"),
                     textOutput("estimatedLatestR")),
            
            fluidRow(align="center", tableOutput("effectiveRSummary")),
            
            helpText("Estimated Peak/Plateau Ends"),
            
            fluidRow(align="center", textOutput("estimatedPeak")),
            
            helpText("Generation Time Distribution"),
            
            numericInput("genTimeMean", "Mean", 5, min=0, step=0.5),
            numericInput("genTimeStdDev", "Standard Deviation", 1.9, min=0, step=0.1),
            
            a("Why 5,1.9?", href="https://www.medrxiv.org/content/10.1101/2020.03.08.20032946v1.full.pdf"),
            
            helpText("Links"),
            
            tags$ul(
              tags$li("created by",a("Nick Cotter", href="https://nickcotter.com/")),
              tags$li("made with", a("R0", href="https://bmcmedinformdecismak.biomedcentral.com/articles/10.1186/1472-6947-12-147")),
              tags$li(a("What is generation time?", href="https://en.wikipedia.org/wiki/Generation_time")),
              tags$li(a("GitHub Repo", href="https://github.com/nickcotter/covid-19-explorer"))
            )
        ),

        # Show a plot of the generated distribution
        mainPanel(
            plotOutput("effectiveR"),
            plotOutput("dailyConfirmedPlot"),
            plotOutput("dailyDiffPlot")
        )
    )
)

server <- function(input, output) {
  
    reactiveGenerationTime <- reactive({
      getGenerationTime(input$genTimeMean, input$genTimeStdDev)
    })
    
    reactiveDailyCounts <- reactive({
        getDailyCounts(input$countries)
    })

    reactiveEstimate <- reactive({
        generateEstimate(reactiveDailyCounts(), reactiveGenerationTime())
    })
    
    reactiveDailyCountAndPrediction <- reactive({
        tryCatch({
            getDailyPredictions(reactiveDailyCounts(), reactiveEstimate())
        }, error=function(e) {
          print(e)
            reactiveDailyCounts()
        })
    })
    
    reactiveEstimatedRByDay <- reactive({
        getEstimatedRByDay(reactiveEstimate())
    })
    
    reactiveEstimatedPeak <- reactive({
       estimatedR <- reactiveEstimatedRByDay();
       if(length(estimatedR$day) > 10) {
         last10Days <- tail(estimatedR, n=10)
         fit <- lm(R ~ day, data=last10Days)
         fitSummary <- summary(fit)
         rm(fit)
         intercept <- fitSummary$coefficients[1,1]
         slope <- fitSummary$coefficients[2,1]
         if(slope < 0) {
           sig <- round(fitSummary$sigma, digits=2)
           crossingDay <- (1 - intercept)/slope
           daysFromNow <- round(crossingDay - tail(last10Days, n=1)$day, digits=0)
           if(daysFromNow < 0) {
             paste("~ ", daysFromNow, " days ago")
           } else {
             paste("in ~ ", daysFromNow, " days")
           }
         } else {
           "-"
         }
       } else {
         "-"
       }
    })

    output$dailyConfirmedPlot <- renderPlot({
    
        dailyCountAndPrediction <- reactiveDailyCountAndPrediction()
        
        plot(dailyCountAndPrediction$day, dailyCountAndPrediction$count, xlab="days",ylab="count", col="red", main="Confirmed Cases")
        
        if("TD" %in% names(dailyCountAndPrediction)) {
            lines(dailyCountAndPrediction$day, dailyCountAndPrediction$TD, col="green")
        }
        
        legend(1, max(dailyCountAndPrediction$count)-10, legend=c("Actual", "Estimated"), col=c("red", "green"), lty=c(0,1), pch=c(1,NA), cex=0.8)
    })
    
    output$dailyDiffPlot <- renderPlot({
      tryCatch( {
        dailyCountAndPrediction <- reactiveDailyCountAndPrediction()
        barplot(dailyCountAndPrediction$countDiff ~ dailyCountAndPrediction$day, xlab="days", ylab="difference", main="Daily Increase")
      }, error=function(e) {})
    })
    
    output$effectiveR <- renderPlot({
      
        tryCatch({
            estimatedRByDay <- reactiveEstimatedRByDay()
            plot(estimatedRByDay$day, estimatedRByDay$R, xlab="days", ylab="R", type="l", main="Effective R")
            abline(h=1, col="gray60")
        }, error=function(e) {})
    })
    
    output$estimatedPeak <- renderText({
      
      tryCatch({
        reactiveEstimatedPeak()
      }, error=function(e) {})
    })
    
    output$estimatedLatestR <- renderText({
      
      tryCatch({
        e <- reactiveEstimatedRByDay()
        round(tail(e$R, n=1), digits=2)
      }, error=function(e) {
      })
      
    })
    
    output$effectiveRSummary <- renderTable({
      
      tryCatch({
        est <- reactiveEstimate()
        effectiveR <- est$R[1:length(est$R)-1]
        rm(est)
        as.array(summary(effectiveR))
      }, error=function(e) {
      })
    
    }, colnames = FALSE, caption="Effective R Range", caption.placement = getOption("xtable.caption.placement", "top"))
}

# Run the application 
shinyApp(ui = ui, server = server)
