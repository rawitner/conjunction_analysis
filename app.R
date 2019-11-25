library(shiny)

# Define UI for miles per gallon app ----
ui <- pageWithSidebar(
  
  # App title ----
  headerPanel("Conjunction Analysis"),
  
  # Sidebar panel for inputs ----
  sidebarPanel(),
  
  # Main panel for displaying outputs ----
  mainPanel(
    tableOutput('table')
  )
)

# Define server logic to plot various variables against mpg ----
server <- function(input, output) {
  output$table <- renderTable(iris)
}

# uses the ui object and the server function we defined to build a Shiny app object.
shinyApp(ui, server)

#runApp("~/shinyapp")