### Network letters about Stael ###

library(visNetwork)
library(igraph)
library(readr)
library(dplyr)
library(tidyr)


datos <- read_csv("~/GitHub/RstudioPV/data/US_LettersMentioningStael_Ver2.csv")
datos$Date <- as.Date(datos$Date, format = "%m/%d/%Y")


##Create graph##
grafo <- graph_from_data_frame(d=datos[, c("Author", "Recipient", "Date")], directed=TRUE)
V(grafo)$label <- V(grafo)$name
E(grafo)$date <- datos$Date

#nodes_df <- data.frame(id = V(grafo)$name, label = V(grafo)$name)
#edges_df <- data.frame(from = ends(grafo, E(grafo))[,1], to = ends(grafo, E(grafo))[,2])

nodes_df <- data.frame(id = V(grafo)$name, label = V(grafo)$label)
edges_df <- data.frame(from = ends(grafo, E(grafo))[,1], to = ends(grafo, E(grafo))[,2], title = E(grafo)$date)

#Visualización simple sin fecha
#visNetwork(nodes = nodes_df, edges = edges_df) %>%
#visOptions(highlightNearest = TRUE, nodesIdSelection = TRUE) %>%
#visEdges(arrows = 'to')

visNetwork(nodes = nodes_df, edges = edges_df) %>%
  visOptions(highlightNearest = TRUE, nodesIdSelection = TRUE) %>%
  visEdges(arrows = 'to') %>%
  visInteraction(navigationButtons = TRUE, tooltipDelay = 100)



