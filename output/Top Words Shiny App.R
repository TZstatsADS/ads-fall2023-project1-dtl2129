library(shiny)
library(countrycode)

# Define the UI for the Shiny app
ui <- fluidPage(
  titlePanel("Happy Words Analysis"),
  sidebarLayout(
    sidebarPanel(
      textInput("country_input", "Enter Country Name or ISO3C Code:", ""),
      actionButton("analyze_button", "Analyze")
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Top Common Words", plotOutput("freq_plot")),
        tabPanel("Top Distinctive Words", plotOutput("distinctive_plot")),
      )
    )
  )
)

# Define the server logic

server <- function(input, output) {
  
  # Load precomputed results
  
  precomputed_results <- readRDS("../output/precomputed_results.rds")
  
  # Analyze the country and update plots
  
  observeEvent(input$analyze_button, {
    country <- input$country_input
    
    # Check if the entered country name is in the dictionary
    if (country %in% countryname_dict$country.name.en) {
      iso3c <- countrycode(country, "country.name", "iso3c")
    } else if (country %in% codelist$iso3c) {
      iso3c <- country
    } else {
      # Invalid country name
      iso3c <- NULL
      showModal(modalDialog(
        title = "Error",
        "Invalid Country Name",
        footer = NULL
      ))
    }
    
    # Proceed only if a valid country code is obtained
    if (!is.null(iso3c)) {
      # Check if precomputed results exist for the country
      if (iso3c %in% names(precomputed_results)) {
        results <- precomputed_results[[iso3c]]
      } else {
        # Calculate results in real-time
        results <- c("No Results", "No Results")
      }
      
      output$freq_plot <- renderPlot(results[[1]])
      output$distinctive_plot <- renderPlot(results[[2]])
    }
  })
}
