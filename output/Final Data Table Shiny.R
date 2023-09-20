library(shiny)

ui <- fluidPage(
  titlePanel("Searchable Data Table"),
    mainPanel(
      DTOutput("data_table")
    )
  )

server <- function(input, output) {
  
  count_gamma_df <- read.csv("../output/count_gamma_df.csv")
  
  output$data_table <- renderDT({
    datatable(
      count_gamma_df,
      options = list(
        searching = TRUE,  # Enable search functionality
        pageLength = 10    # Number of rows per page
      )
    )
  })
}
