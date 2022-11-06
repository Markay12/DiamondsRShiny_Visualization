library(shiny)
library(ggplot2)
library(tidyverse)
library(plotly)
library(ggpubr)


dia_data <- diamonds
dia_data <- dia_data[order(dia_data$carat),]

## Define UI for dataset viewer app 
ui <- fluidPage(
  
  # App title
  titlePanel("Prices and Information of 50k Round Cut Diamonds"),
  
  # Sidebar layout with a input and output definitions
  sidebarLayout(
    
    # Sidebar panel for inputs
    sidebarPanel(
      
      # Input: Selector for choosing diamonds information
      selectInput(inputId = "diamonds",
                  label = "Choose a Analysis Selection:",
                  choices = c("Carat Information", "Depth Information", "Carat and Price in USD", "Histogram of Prices",
                              "Carat Plot", "Carat vs Price Plot", "Depth and Clarity vs Price")),
      
      conditionalPanel("input.diamonds == 'Carat Information' || input.diamonds == 'Depth Information' || input.diamonds == 'Carat and Price in USD'",
      
        # Input: Numeric entry for number of obs to view
        numericInput(inputId = "obs",
                    label = "Number of observations to view:",
                    value = 10)
      ),
      
      conditionalPanel("input.diamonds == 'Histogram of Prices'",
                       
                       sliderInput(inputId = "bins",
                                   label = "Number of Bins:",
                                   min = 1,
                                   max = 30,
                                   value = 15
                                   )
                       
                       ),
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
      
      conditionalPanel("input.diamonds == 'Carat Plot' || input.diamonds == 'Carat vs Price Plot' || input.diamonds == 'Depth and Clarity vs Price' || input.diamonds == 'Histogram of Prices'", 
                       
                       
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
           "Histogram of Prices" = "price_hist",
           "Carat Plot" = "carat_plot",
           "Carat vs Price Plot" = "carat_vs_price",
           "Depth and Clarity vs Price" = "depth_clarity_price"
    )
    
  })
  
  

  # Generate a summary of the dataset
  output$summary <- renderPrint({
    
    if (identical(datasetInput(), "carat_plot"))
    {
      
      dataset <- dia_data$carat
      summary(dataset)
      
    }
    else if (identical(datasetInput(), "carat_vs_price"))
    {
      
      dataset1 <- dia_data[,c("carat", "price")]
      summary(dataset1)
      
    }
    else if (identical(datasetInput(), "depth_clarity_price"))
    {
      
      dataset2 <- dia_data[,c("carat", "depth", "price")]
      summary(dataset2)
      
    }
    else if(identical(datasetInput(), "price_hist"))
    {
      
      dataset3 <- dia_data$price
      summary(dataset3)
      
    }
    else
    {
      
      dataset <- datasetInput()
      summary(dataset)
      
    }
    
  })
  
  # Show the first "n" observations
  output$view <- renderTable({
    
    if  (identical(datasetInput(), "carat_plot"))
    {
      
      head(dia_data$carat, n = input$obs)
      
    }
    else
    {
      
      head(datasetInput(), n = input$obs)
      
      
    }
      
  })
  
  output$plot <- renderPlot({
    
    
    if (identical(datasetInput(), "carat_plot"))
    {
      
      plot(dia_data$carat, type = "l", col = "orange", main = "Plot of Diamond Carat Information", ylab = "Carat (Weight) Value from 0.2 - 5.01", xlab = "Individual Diamonds Tested")
      
      
    }
    else if (identical(datasetInput(), "carat_vs_price"))
    {
      
      ## carat vs price plot
      
      thisplot <- ggplot(data = dia_data, aes(x = carat, y = price)) + 
        geom_point(size = 0.5,aes(color = clarity)) + geom_smooth(formula = y ~ x, method = "lm")
      
      thisplot
      
      
      
    }
    else if (identical(datasetInput(), "depth_clarity_price"))
    {
      
      thisPlot2 <- ggplot(data, aes(x = price, y = depth, color = clarity)) + 
        geom_point(alpha = 0.25) +
        geom_smooth(formula = y ~ x, method = "loess",  se = FALSE)
      
      thisPlot2
    }
    else if (identical(datasetInput(), "price_hist"))
    {
      
      plotly <- dia_data$price
      bins <- seq(min(plotly), max(plotly), length.out =  input$bins + 1)
      
      thisPlot3 <- hist(plotly, breaks = bins, col = "orange", xlab = "Prices in USD ($)", main = "Histogram of Prices")
      
      thisPlot3
      
      
    }
    else
    {
      
      ## no plot
      
    }
    
  })
  
}

# Create Shiny app ----
shinyApp(ui = ui, server = server)