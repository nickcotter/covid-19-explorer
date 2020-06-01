library(shiny)
library(dplyr)
library(lubridate)
library(shinycssloaders)
library(ggplot2)
library(R0)

# load the latest data
confirmed <- read.csv(url("https://raw.githubusercontent.com/CSSEGISandData/2019-nCoV/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_global.csv")) %>%
              dplyr::select(-c(Lat, Long, ))   
recovered <- read.csv(url("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_recovered_global.csv")) %>%
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
    colnames(sumsByDateCode) <- c("count")
    sumsByDateCode$day <- as.integer(rownames(sumsByDateCode))
    
    filteredRecovered <- recovered %>%
      filter(country == "Global" | Country.Region == country)
    recoveredSums <- as.integer(colSums(filteredRecovered[,-match(c("Province.State", "Country.Region"), names(filteredRecovered))], na.rm=TRUE))
    recoveredSumsByDateCode <- as.data.frame(recoveredSums)
    colnames(recoveredSumsByDateCode) <- c("count")
    recoveredSumsByDateCode$day <- as.integer(rownames(recoveredSumsByDateCode))
    
    dailyCounts <- sumsByDateCode %>%
      merge(recoveredSumsByDateCode, by=c("day")) %>%
      rename("count" = "count.x") %>%
      rename("recovered" = "count.y") %>%
      mutate(active = count - recovered) %>%
      mutate(new_cases = count - lag(count, default=count[1])) %>%
      filter(new_cases > 0)
}

generateEstimate <-function(dailyCounts, mgt) {
  estimate.R(dailyCounts$new_cases, end=length(dailyCounts$new_cases), methods=c("TD"), GT=mgt)$estimates$TD
} 

getDailyPredictions <- function(dailyCounts, est) {
    dailyCountAndPrediction <- merge(dailyCounts, est$pred, by="row.names", sort=FALSE, all=TRUE) %>%
      dplyr::select(-c("Row.names")) %>%
      dplyr::rename(TD = y) %>%
      mutate(countDiff = count - lag(count))
}

getEstimatedRByDay <- function(est) {
    estimatedR <- est$R[1:length(est$R)-1]
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
                     h1(htmlOutput("estimatedLatestR"))),
            
            fluidRow(align="center", tableOutput("effectiveRSummary")),
            
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
            plotOutput("effectiveR") %>% withSpinner(color="#0dc5c1", type=4),
            plotOutput("dailyConfirmedPlot") %>% withSpinner(color="#0dc5c1", type=4)
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

    output$dailyConfirmedPlot <- renderPlot({
        dailyCountAndPrediction <- reactiveDailyCountAndPrediction()
        ggplot(dailyCountAndPrediction) + geom_col(aes(day, new_cases, col="actual")) + geom_line(aes(day, TD, col="estimated")) +
          xlab("days since first case") + ylab("new cases") + ggtitle("New Cases - Actual & Estimated") + scale_colour_manual(values=c("grey", "blue")) + 
          theme(plot.title = element_text(hjust = 0.5))
    })
    
    output$effectiveR <- renderPlot({
      
        tryCatch({
            estimatedRByDay <- reactiveEstimatedRByDay()
            ggplot(estimatedRByDay) + geom_line(aes(day, R)) + xlab("days since first case") + ylab("R") + ggtitle("Effective R") + geom_hline(yintercept = 1, color="gray60") +
              theme(plot.title = element_text(hjust = 0.5))
        }, error=function(e) {})
    })
    
    output$estimatedLatestR <- renderText({
      
      tryCatch({
        e <- reactiveEstimatedRByDay()
        print(length(e$R))
        latestR <- tail(e$R, n=1)
        latestRoundedR <- round(latestR, digits=2)
        
        if(length(e$R) > 1) {
          penultimateR <- tail(e$R, n=2)[1]
          if(penultimateR > latestR) {
            paste("<font color=\"#00FF00\"><b>", latestRoundedR, "</b></font>")
          } else if(penultimateR < latestR) {
            paste("<font color=\"#FF0000\"><b>", latestRoundedR, "</b></font>")
          } else {
            latestRoundedR
          }
        } else {
          latestRoundedR
        }
        
        
        #paste("<font color=\"#FF0000\"><b>", latestRoundedR, "</b></font>")
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
