library(shiny)


ui <- fluidPage(

    # Application title
    titlePanel("COVID-19 Explorer"),

    
    sidebarLayout(
        sidebarPanel(
          
            helpText("The COVID-19 Explorer has moved here:"),
          
            fluidRow(align="center", a("https://covid-explorer-pa6ye47i4a-ew.a.run.app/", href="https://covid-explorer-pa6ye47i4a-ew.a.run.app/"))
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
