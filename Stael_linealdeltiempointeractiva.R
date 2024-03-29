library(plotly)
library(timevis)
library(shiny)
library(plotly)
library(readxl)


# UI
    ui <- fluidPage(
      titlePanel("Línea de Tiempo de Eventos Interactiva"),
      sidebarLayout(
        sidebarPanel(
          # filtrar por localización
          selectInput("localizationEvent", "Filtrar por localización:",
                      choices = c("Todos" = "Todos"))  
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
                          choices = c("Todos", localizaciones))
      })
      
      output$lineaTiempo <- renderPlotly({
        
# Filtrate data based in usuer selection
        datosFiltrados <- if(input$localizationEvent != "Todos") {
          datos[datos$Localization == input$localizationEvent, ]
        } else {
          datos
        }
        
        # CCreate GRaphic  
        p <- plot_ly(datosFiltrados, x = ~Date, y = ~rep(1, nrow(datosFiltrados)), type = 'scatter', mode = 'markers+lines',
                     text = ~paste(Event, Description, sep = "<br>"), hoverinfo = 'text',
                     marker = list(size = 10)) %>%
          layout(title = 'Eventos a lo largo del tiempo',
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
    