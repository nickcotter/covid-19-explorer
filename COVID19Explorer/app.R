library(shiny)


ui <- fluidPage(

    # Application title
    titlePanel("COVID-19 Explorer"),

    
    sidebarLayout(
        sidebarPanel(
          
            helpText("The COVID-19 Explorer has moved here:"),
          
            fluidRow(align="center", a("https://covid19-explorer.cosmo-opticon.net/", href="https://covid19-explorer.cosmo-opticon.net/"))
        ),

        
        mainPanel(
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
}

# Run the application 
shinyApp(ui = ui, server = server)
