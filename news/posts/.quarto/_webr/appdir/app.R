library(shiny)
library(tidyr)
library(dplyr)

# Define UI for application that draws a histogram
ui <- fluidPage(
  
  # Application title
  titlePanel("Dropout Per Dose"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
      numericInput("dropoutRate", "Desired Dropout Rate:", value = 0.1),
      sliderInput("numVisits", "Number of visits:", 
                  min = 1, max = 20, value = 5)
      
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      tableOutput("visitsGuide")
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  output$visitsGuide <- renderTable({
    data.frame(Visits = 1:input$numVisits,
               ConditionalDropoutRate = 1-(1-input$dropoutRate)^(1/input$numVisits)) %>%
      mutate(TotalDropoutRate = 1-cumprod(1-ConditionalDropoutRate),
             MarginalDropoutRate = c(TotalDropoutRate[1], TotalDropoutRate[-1] - TotalDropoutRate[-n()])) %>%
      select(Visits, ConditionalDropoutRate, MarginalDropoutRate, TotalDropoutRate) %>%
      rename("Visit number" = "Visits",
             "Prob. dropout given not dropped before previous visit" = "ConditionalDropoutRate",
             "Prob. dropout between previous visit and current visit" = "MarginalDropoutRate",
             "Prob. dropout before visit" = "TotalDropoutRate")
  }, digits = 4)
}

# Run the application 
shinyApp(ui = ui, server = server)
