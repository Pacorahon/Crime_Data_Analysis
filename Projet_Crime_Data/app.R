library(shiny)

ui <- fluidPage(
  titlePanel(),
  sidebarLayout(
    sidebarPanel(
    ),      
    mainPanel(
      
    ),
  )
)


server <- function(input, output) {
}
shinyApp(ui = ui, server = server)
