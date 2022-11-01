library(shiny)

dia_data <- diamonds
dia_data <- dia_data[order(dia_data$carat),]

## Define UI for dataset viewer app 
ui <- fluidPage(
  
  # App title ----
  titlePanel("Prices and Information of 50k Round Cut Diamonds"),
  
  # Sidebar layout with a input and output definitions
  sidebarLayout(
    
    # Sidebar panel for inputs
    sidebarPanel(
      
      # Input: Selector for choosing diamonds information
      selectInput(inputId = "diamonds",
                  label = "Choose a Analysis Selection:",
                  choices = c("Carat Information", "Depth Information", "Carat and Price in USD", "Carat Plot")),
      
      conditionalPanel("input.diamonds == 'Carat Information' || input.diamonds == 'Depth Information' || input.diamonds == 'Carat and Price in USD'",
      
        # Input: Numeric entry for number of obs to view
        numericInput(inputId = "obs",
                    label = "Number of observations to view:",
                    value = 10)
      )
    ),
    
    # Main panel for displaying outputs ----
    mainPanel(
      
      
      ## Output: Verbatim text for data summary
      ## Always show the summary
      verbatimTextOutput("summary"),
      
      
      conditionalPanel("input.diamonds == 'Carat Information' || input.diamonds == 'Depth Information' || input.diamonds == 'Carat and Price in USD'",
                       
                       
                       
                       # Output: HTML table with requested number of observations
                       tableOutput("view"),
                       
                       ),
      
      conditionalPanel("input.diamonds == 'Carat Plot'", 
                       
                       
                       plotOutput("plot")
                       )
      
      
    )
  )
)

# Define server logic to summarize and view selected dataset ----
server <- function(input, output) {
  
  # Return the requested dataset
  datasetInput <- reactive({
    switch(input$diamonds,
           "Carat Information" = dia_data$carat,
           "Depth Information" = dia_data$depth,
           "Carat and Price in USD" = dia_data[c('carat', 'price')],
           "Carat Plot" = dia_data$carat
    )
    
  })
  

  # Generate a summary of the dataset
  output$summary <- renderPrint({
    dataset <- datasetInput()
    summary(dataset)
  })
  
  # Show the first "n" observations
  output$view <- renderTable({
    head(datasetInput(), n = input$obs)
  })
  
  output$plot <- renderPlot({
    
    plot(datasetInput(), type = "l", col = "orange", main = "Plot of Diamond Carat Information", ylab = "Cara (Weight) Value from 0.2 - 5.01", xlab = "Individual Diamonds Tested")
    
  })
  
}

# Create Shiny app ----
shinyApp(ui = ui, server = server)