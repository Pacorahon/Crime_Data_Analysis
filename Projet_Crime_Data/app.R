source(file = "global.R")
source(file = "Packages.R")

# UI
ui <- fluidPage(
  useShinyjs(),
  div(
    h1("Evolutions temporelles des crimes commis à LA ", align = "center", style = "text-decoration: underline;"),
    br(),
    br(),
    
    sidebarLayout(
      sidebarPanel(
        h3("Sélectionnez la période d'étude"),
        
        # Widget pour la sélection de l'année
        h5("Sélectionnez l'année"),
        checkboxGroupInput("selectedYear", "Année :", choices = unique(data$year), inline = TRUE),
        br(),
        
        # Widget pour la sélection du mois
        h5("Sélectionnez le mois"),
        checkboxGroupInput("selectedMonth", "Mois :", choices = sort(unique(data$month), 
                                                                     by = month(order(data$month, 
                                                                                      unique(data$month))))),
      ),
      mainPanel(
        tabsetPanel(
          id = "onglets",
          type = "tabs",
          
          tabPanel("Statistiques", icon = icon("percent"),
                   br(),
                   textOutput("totalCrimesText"),
                   br(),
                   textOutput("percentageText"),
                   br(),
                   style = "text-align: center;",
          ),
          
          tabPanel("Graphique des crimes", icon = icon("gun"),
                   br(),
                   br(),
                   br(),
                   plotOutput("crimeBarChart")
          ),
          
          tabPanel("Graphique des zones", icon = icon("chart-area"),
                   br(),
                   br(),
                   br(),
                   br(),
                   plotOutput("crimeAreaChart"),
                   br(),
                   br(),
                   leafletOutput("heatmap"),
                   br(),
                   br()
          )
        )
      )
    )
  )
)

# server
server <- function(input, output, session) {
  

  # Résultats basés sur la sélection temporelle
  # Résultats basés sur la sélection temporelle
  filteredData <- reactive({
    print("Function filteredData called")  # Ajout du print pour vérifier l'appel de la fonction
    
    subset_data <- subset(data,
                          month(data$datetime) %in% input$selectedMonth &
                            hour(data$datetime) >= input$selectedTime[1] & hour(data$datetime) <= input$selectedTime[2])
    
    # Conversion de l'année en facteur après le filtrage
    subset_data$year <- as.factor(subset_data$year)
    
    # Comptage des occurrences de chaque type de crime
    crime_counts <- table(subset_data$`Crm Cd Desc`)
    
    # Afficher les colonnes disponibles dans subset_data
    print(names(subset_data))
    
    # Sélection des 15 types de crime les plus fréquents
    top_crimes <- names(sort(crime_counts, decreasing = TRUE)[1:15])
    
    # Filtrer les données uniquement pour les 15 types de crime les plus fréquents
    subset_data <- subset_data[subset_data$`Crm Cd Desc` %in% top_crimes, ]
    
    print("Subset data created")  # Ajout du print pour vérifier la création de subset_data
    
    return(subset_data)
  })
  
  
  
  output$totalCrimesText <- renderText({
    totalCrimes <- nrow(filteredData())
    paste("Nombre total de crimes commis pendant cette période :", totalCrimes)
  })
  
  output$percentageText <- renderText({
    totalCrimes <- nrow(filteredData())
    totalRowsFiltered <- nrow(data)
    
    # Vérifier si le dénominateur est différent de zéro
    if (totalRowsFiltered != 0) {
      percentage <- (totalCrimes / totalRowsFiltered) * 100
      paste("Pourcentage des crimes sélectionnés par rapport au total :", round(percentage, 2), "%")
    } else {
      "Pas de données disponibles."
    }
  })
  
  
  # Code pour créer le graphique à barres
  output$crimeBarChart <- renderPlot({
    ggplot(filteredData(), aes(x = `Crm Cd Desc`, fill = `Crm Cd Desc`)) +
      geom_bar() +
      labs(title = "Répartition des crimes", x = "Type de Crime", y = "Nombre de Crimes") +
      theme(axis.text.x = element_text(angle = 45, hjust = 1),
            legend.position = "none") +
      coord_cartesian(ylim = c(0, max(table(filteredData()$`Crm Cd Desc`)) * 1.2))  # Ajuster la plage y
  }, height = 600)
  
  center_lat_h <- reactive({median(filteredData()$LAT)})
  center_lon_h <- reactive({median(filteredData()$LON)})
  
  output$heatmap <- renderLeaflet({
    leaflet(filteredData()) %>%
      addTiles() %>%
      addHeatmap(
        lat = ~LAT,
        lng = ~LON,
        blur = 20,
        max = 0.2,
        radius = 10
      ) %>%
      setView(lat = center_lat_h(), lng = center_lon_h(), zoom = 9)
  })
  
}

    
# Lancement de l'application Shiny
shinyApp(ui = ui, server = server)
    

crime_counts <- table(subset_data$`Crm.Cd.Desc`)
print(names(subset_data))
