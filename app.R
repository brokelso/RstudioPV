library(plotly)
library(timevis)
library(shiny)
library(plotly)
library(readxl)


datos <- read_excel("C:/Users/Samuel David/Desktop/StaelTimeline.xlsx")

ui <- fluidPage(
  titlePanel("Narrativa de Eventos"),
  plotlyOutput("timePlot") 
)

# Define el servidor
server <- function(input, output) {
  output$timePlot <- renderPlotly({
    datos <- read_excel("C:/Users/Samuel David/Desktop/StaelTimeline.xlsx")
    datos$Date <- as.character(datos$Date)
    Date <- sort(unique(datos$Date))
    
# Crear el gráfico plotly
p <- plot_ly(datos, x = ~Date, y = ~rep(1, nrow(datos)), type = 'scatter', mode = 'markers+lines',
                 text = ~paste(Event, Description, sep = "<br>"), hoverinfo = 'text',
                 marker = list(size = 10)) %>%
      layout(title = 'Eventos a lo largo del tiempo',
             xaxis = list(title = 'Date',
                          tickvals = Date,  # Establecer las marcas para cada año único
                          ticktext = Date,  # Etiquetas para cada marca
                          tickangle = 45),  # Ángulo de las etiquetas para mejor lectura
             yaxis = list(zeroline = FALSE, showline = FALSE, showticklabels = FALSE, showgrid = FALSE)) # Eliminar completamente el eje Y
    
    return(p)
  })
}


 

# Ejecutar la aplicación
shinyApp(ui = ui, server = server)




###### intento mas complejo ####

