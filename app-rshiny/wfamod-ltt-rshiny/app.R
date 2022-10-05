

library(shiny)

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("WFAMOD-LTT"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
          fileInput("tax_schedule", "1. Upload tax schedule"),
          fileInput("forecast_parameters", "2. Upload forecast parameters")
        ),

        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("taxForecast")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {


  
  
  
  
  
}

# Run the application 
shinyApp(ui = ui, server = server)
