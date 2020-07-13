library(shiny)
library(dplyr)
library(shinycssloaders)
library(ggplot2)
library(lubridate)
library(R0)
library(zoo)

# load the latest data
confirmed <- read.csv(url("https://raw.githubusercontent.com/CSSEGISandData/2019-nCoV/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_global.csv")) %>%
              dplyr::select(-c(Lat, Long, ))   

deaths <- read.csv(url("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_global.csv")) %>%
              dplyr::select(-c(Lat, Long))

countries <- unique(confirmed$Country.Region)
countries <- factor(append("Global", as.character(countries)))


getNewDeaths <- function(country) {
  filteredDeaths <- deaths %>%
    filter(country == "Global" | Country.Region == country)
  
  sums <- as.integer(colSums(filteredDeaths[,-match(c("Province.State", "Country.Region"), names(filteredDeaths))], na.rm=TRUE))
  newDeaths <- diff(sums)
  dateCodes <- sub(".", "", colnames(filteredDeaths)[4:(length(newDeaths)+3)])
  rm(filteredConfirmed)
  gc()
  
  dates <- mdy(dateCodes)
  
  data.frame(date=dates, deaths=newDeaths)
}

getGenerationTime <- function(m, s) {
  generation.time("gamma", c(m, s), truncate = 36)
}

getEpidemicCurve <- function(country) {
  filteredConfirmed <- confirmed %>%
    filter(country == "Global" | Country.Region == country)
  sums <- as.integer(colSums(filteredConfirmed[,-match(c("Province.State", "Country.Region"), names(filteredConfirmed))], na.rm=TRUE))
  newCases <- diff(sums)
  dateCodes <- sub(".", "", colnames(filteredConfirmed)[4:(length(newCases)+3)])
  
  rm(filteredConfirmed)
  gc()
  
  dates <- mdy(dateCodes)
  structure(newCases, names = as.character(dates))
}

estimateEffectiveR <- function(epidemicCurve, generationTime) {
  
  meanRange <- 1
  while(meanRange < 10) {
    meanEpidemicCurve <- rollmean(epidemicCurve, meanRange, fill=0)
    tryCatch({
      firstIndex <- which(meanEpidemicCurve > 0)[[1]]
      print(firstIndex)
      r <- estimate.R(meanEpidemicCurve[firstIndex:length(meanEpidemicCurve)], methods=c("TD"), GT=generationTime, nsim=100)$estimates$TD
      return(r)
    }, error=function(e) {
      print(e)
    })
    rm(meanEpidemicCurve)
    gc()
    meanRange <- meanRange + 1
  }
}

plotEffectiveR <- function(r) {
  plot(r$R[1:length(r$R)-1], type="l", ylab = "R", xlab = "Days Since First Case")
  abline(h=1)
}

plotNewCases <- function(r, newDeaths) {
  rDf <- as.data.frame(r$epid)
  
  ggplot() + geom_col(data=rDf, aes(x=t, y=incid, fill="cases")) +
    geom_col(data=newDeaths, aes(x=date, y=deaths, fill="deaths")) +
    xlab("Date") + ylab("New Cases/Deaths") +
    scale_fill_manual(name = "", values = c("cases" = "grey", "deaths" = "red")) +
    theme(legend.position = c(0.15, 0.85), legend.direction = "horizontal")
    
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

            div(icon("arrow-up"), style="display:none"),
            
            fluidRow(align="center",
                     helpText("Estimated Latest R"),
                     h2(htmlOutput("estimatedLatestR"))),
            
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
            plotOutput("newCases") %>% withSpinner(color="#0dc5c1", type=4),
            plotOutput("effectiveR") %>% withSpinner(color="#0dc5c1", type=4)
        )
    )
)

server <- function(input, output) {
  
    reactiveGenerationTime <- reactive({
      getGenerationTime(input$genTimeMean, input$genTimeStdDev)
    })
    
    reactiveEpidemicCurve <- reactive({
      getEpidemicCurve(input$countries)
    })
    
    reactiveNewDeaths <- reactive({
      getNewDeaths(input$countries)
    })
    
    reactiveEstimateEffectiveR <- reactive({
      estimateEffectiveR(reactiveEpidemicCurve(), reactiveGenerationTime())
    })
    
    output$effectiveR <- renderPlot({
      
      tryCatch({
        plotEffectiveR(reactiveEstimateEffectiveR())
      }, error=function(e) {
        print(e)
      })
      
    })
    
    output$newCases <- renderPlot({
      
      tryCatch({
        plotNewCases(reactiveEstimateEffectiveR(), reactiveNewDeaths())
      }, error=function(e) {
        print(e)
      })
      
    })
    
    output$estimatedLatestR <- renderText({
      
      tryCatch({
        
        e <- reactiveEstimateEffectiveR()
        
        latestR <- e$R[length(e$R)-1]
        
        col <- "black"
        if(latestR > 1) {
          col <- "red"
        } else if(latestR < 1) {
          col <- "green"
        }
        
        latestRoundedR <- round(latestR, digits=2)
        
        codedR <- paste("<font color='", col, "'>", latestRoundedR, "</font>")
        
        if(length(e$R > 2)) {
          penultimateR <- e$R[length(e$R)-2]
          if(penultimateR < latestR) {
            paste(codedR, "<i class='fas fa-arrow-up'></i>")
          } else if(penultimateR > latestR) {
            paste(codedR, "<i class='fas fa-arrow-down'></i>")
          } else {
            codedR
          }
        } else {
          codedR
        }
      }, error=function(e) {
        print(e)
      })
    })
    
    output$effectiveRSummary <- renderTable({
      
      tryCatch({
        as.array(summary(reactiveEstimateEffectiveR()$R))
      }, error=function(e) {
      })
    
    }, colnames = FALSE, caption="Effective R Range", caption.placement = getOption("xtable.caption.placement", "top"))
}

# Run the application 
shinyApp(ui = ui, server = server)
