library(plotly)
library(timevis)
library(shiny)
library(plotly)
library(readxl)
#Libraries for maping
library(rworldmap)
library(leaflet)
library(maps)

# UI
    ui <- fluidPage(
      titlePanel("Interactive Time Line"),
      sidebarLayout(
        sidebarPanel(
          
          selectInput("localizationEvent", "Filtrate by localization:",
                      choices = c("All" = "All"))  
        ),
        mainPanel(
          plotlyOutput("lineaTiempo")
        )
      )
    )
    
 # Create Server
 server <- function(input, output, session) {
      # Upload data
    datos <- read_excel("~/GitHub/RstudioPV/StaelTimeline.xlsx") #Change to the localization of the file
    datos$Date <- as.character(datos$Date) 
      
       observe({
        localizaciones <- sort(unique(datos$Localization))
        updateSelectInput(session, "localizationEvent",
                          choices = c("All", localizaciones))
      })
      
      output$lineaTiempo <- renderPlotly({
        
# Filtrate data based in usuer selection
        datosFiltrados <- if(input$localizationEvent != "All") {
          datos[datos$Localization == input$localizationEvent, ]
        } else {
          datos
        }
        
        # CCreate GRaphic  
        p <- plot_ly(datosFiltrados, x = ~Date, y = ~rep(1, nrow(datosFiltrados)), type = 'scatter', mode = 'markers+lines',
                     text = ~paste(Event, Description, sep = "<br>"), hoverinfo = 'text',
                     marker = list(size = 10)) %>%
          layout(title = 'Event thorugh time',
                 titlefont = list(size = 24),
                 paper_bgcolor = 'rgba(245,246,249,1)',
                 plot_bgcolor = 'rgba(245,246,249,1)',
                 xaxis = list(title = 'Año',
                              tickangle = 45),
                 yaxis = list(zeroline = FALSE, showline = FALSE, showticklabels = FALSE, showgrid = FALSE))
        
        return(p)
      })
    }
    
    # Execute 
    shinyApp(ui = ui, server = server)
    
 ######################    trying to Do the Map#################################
    
     #Data gathering 
    datos <- read_excel("~/GitHub/RstudioPV/StaelTimeline.xlsx")

    #Obtener coord
      coordenadas <- map_data("world") %>%
      group_by(region) %>%
      summarise(lon = mean(long), lat = mean(lat)) %>%
      ungroup()
    
   datos_con_coordenadas <- merge(datos, coordenadas, by.x = "Localization", by.y = "region", all.x = TRUE)
    
    mapa <- leaflet(datos_con_coordenadas) %>%
      addTiles() %>%
      addMarkers(lng = ~lon, lat = ~lat, popup = ~paste(Event, Description))
    
    mapa
    
##################### Using Shiny ################
########## Obtain coord
    
    # Obtener datos de los países, incluyendo latitud y longitud 
    data(world.cities)
    paises <- unique(world.cities[, c("country.etc", "lat", "long")])
    colnames(paises) <- c("Localization", "lat", "lon")
    
    # extracting and merging tthe data
    datos <- read_excel("~/GitHub/RstudioPV/StaelTimeline.xlsx")
    datos$Localization <- as.character(datos$Localization) # Convertir a caracter si es necesario
    
    datos_con_coordenadas <- merge(datos, paises, by = "Localization", all.x = TRUE)
    
#View(datos_con_coordenadas)  This is a huge problem, because we only have the year, so I ahve to trate that column as integer instead
    datos_con_coordenadas$Year <- as.integer(as.character(datos_con_coordenadas$Date))
    years_sorted <- sort(unique(datos_con_coordenadas$Year))
# Definición de UI
    ui <- fluidPage(
      titlePanel("Visualización de Eventos por Año"),
      sidebarLayout(
        sidebarPanel(
          # Input: Selector de año
          selectInput("yearInput",
                      "Seleccione un año:",
                      choices = years_sorted,
                      selected = min(years_sorted))
        ),
        mainPanel(
          # Output: Mapa renderizado
          leafletOutput("map")
        )
      )
    )
    

        # Definición del servidor
    server <- function(input, output) {
      output$map <- renderLeaflet({
        # Filtrar los datos basados en el año seleccionado por el usuario
        datos_filtrados <- datos_con_coordenadas %>%
          filter(Year == as.integer(input$yearInput))
        
        # Crear y renderizar el mapa
        leaflet(datos_filtrados) %>%
          addTiles() %>%
          addMarkers(
            lng = ~lon, lat = ~lat,
            popup = ~paste("<strong>Año:</strong>", Year, 
                           "<br><strong>Evento:</strong>", Event, 
                           "<br><strong>Descripción:</strong>", Description)
          )
      })
    }
    # Correr la aplicación
    shinyApp(ui = ui, server = server)
    
    