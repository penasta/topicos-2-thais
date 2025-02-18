# Exemplo didatico 

# Pacote necessário
#install.packages("dbscan")

library(dbscan)

# Criando dados de exemplo

set.seed(42)
data <- rbind(
  matrix(rnorm(100, mean = 5, sd = 1), ncol = 2),
  matrix(rnorm(100, mean = 10, sd = 1), ncol = 2),
  matrix(rnorm(50, mean = 15, sd = 1), ncol = 2)
)
colnames(data) <- c("x", "y")

# Visualizando os dados
plot(data, col = "gray", pch = 19, main = "Dados de Exemplo")

#### DBSCAN  ####

# Definição de hiperparametros Arbitraria ###

eps <- 1.5  # Raio
minPts <- 5 # Número mínimo de pontos para formar um cluster

db <- dbscan(data, eps = eps, minPts = minPts)

# Ver os resultados
print(db)

# Visualizando os clusters

plot(data, col = db$cluster + 1, pch = 19, main = "Clusters identificados pelo DBSCAN")
legend("topleft", legend = c("Cluster 1", "Cluster 2", "Cluster 3", "Ruído"),
       col = c(2, 3, 4, 1), pch = 19, bty = "n")

## Definição ideal de hiperparametros  ##

## 1.  minPts = dim + 1

minPts <- dim(data)[2]+1

## 2.  k-NN distance plot for k = minPts - 1 = 2

kNNdistplot(data, minPts = minPts)
abline(h=1, col = "red", lty = 2)

eps <- 1

db <- dbscan(data, eps = eps, minPts = minPts)
print(db)

# Visualizando os clusters
plot(data, col = db$cluster + 1, pch = 19, main = "Clusters identificados pelo DBSCAN")
legend("topleft", legend = c("Cluster 1", "Cluster 2", "Cluster 3", "Ruído"),
       col = c(2, 3, 4, 1), pch = 19, bty = "n")

###  Kmeans ####

# Número de clusters estimado visualmente como 3 (baseado nos dados)

set.seed(42) 
k <- 3
kmeans_result <- kmeans(data, centers = k, nstart = 25)

# Ver os resultados do K-means
print(kmeans_result)

# Visualizando os clusters do K-means
plot(data, col = kmeans_result$cluster, pch = 19, main = "Clusters identificados pelo K-means")
points(kmeans_result$centers, col = 1:k, pch = 4, cex = 2, lwd = 2)
legend("topleft", legend = c("Cluster 1", "Cluster 2", "Cluster 3"),
       col = c(1, 2, 3), pch = 19, bty = "n")


### Shiny ####

# Instalar pacotes necessários
#install.packages("shiny")
#install.packages("dbscan")
#install.packages("plotly")

# Carregar as bibliotecas
library(shiny)
library(dbscan)
library(plotly)

# Gerar dados
set.seed(42)
data <- rbind(
  matrix(rnorm(100, mean = 5, sd = 1), ncol = 2),
  matrix(rnorm(100, mean = 10, sd = 1), ncol = 2),
  matrix(rnorm(50, mean = 15, sd = 1), ncol = 2)
)
colnames(data) <- c("x", "y")

# Função para ajustar DBSCAN e plotar os resultados
plot_dbscan <- function(eps, minPts) {
  # Ajuste do modelo DBSCAN
  db <- dbscan(data, eps = eps, minPts = minPts)
  
  # Criar um gráfico interativo
  plot_ly(x = data[, 1], y = data[, 2], type = 'scatter', mode = 'markers', 
          color = as.factor(db$cluster), colors = c('red', 'green', 'blue', 'yellow')) %>%
    layout(title = paste("DBSCAN: eps =", eps, "minPts =", minPts),
           xaxis = list(title = 'X'),
           yaxis = list(title = 'Y'))
}

# Interface do aplicativo shiny
ui <- fluidPage(
  titlePanel("DBSCAN - Ajuste de Hiperparâmetros"),
  
  sidebarLayout(
    sidebarPanel(
      sliderInput("eps", "Valor de Eps:", min = 0.1, max = 10, value = 2, step = 0.1),
      sliderInput("minPts", "Número mínimo de pontos (minPts):", min = 1, max = 20, value = 5)
    ),
    
    mainPanel(
      plotlyOutput("dbscan_plot")
    )
  )
)

# Servidor do aplicativo shiny
server <- function(input, output) {
  output$dbscan_plot <- renderPlotly({
    plot_dbscan(input$eps, input$minPts)
  })
}

# Rodar o aplicativo shiny
shinyApp(ui = ui, server = server)
