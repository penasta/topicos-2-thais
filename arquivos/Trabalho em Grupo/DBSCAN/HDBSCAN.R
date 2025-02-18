
#HDBSCAN ####

library(dbscan) # Para HDBSCAN e DBSCAN
library(ggplot2)


set.seed(42)

# Criando três clusters distintos com distribuição normal

cluster1 <- matrix(rnorm(100, mean = 5, sd = 1), ncol = 2)
cluster2 <- matrix(rnorm(100, mean = 10, sd = 1), ncol = 2)
cluster3 <- matrix(rnorm(50, mean = 15, sd = 1), ncol = 2)

# Criando um conjunto de pontos aleatórios (ruído) distribuídos uniformemente
# noise <- matrix(runif(50, min = 0, max = 15), ncol = 2)

# Combinando os clusters 
data <- rbind(cluster1, cluster2, cluster3#, noise)
)

# Aplicando o HDBSCAN
hdbscan_result <- hdbscan(data, minPts = 5)

# Exibição de resultados
print(hdbscan_result)

# Convertendo os dados para um data frame e adicionando os clusters como colunas.
data_with_labels <- as.data.frame(data)
data_with_labels$cluster <- as.factor(hdbscan_result$cluster)

# Visualizando os clusters

plot(data, 
     col = hdbscan_result$cluster + 1L, 
     pch = 19, 
     main = "Clusters Identificados pelo HDBSCAN", 
     xlab = "X", 
     ylab = "Y")
legend("topleft", legend = c("Cluster 1", "Cluster 2", "Cluster 3", "Ruído"),
       col = c(2, 3, 4, 1), pch = 19, bty = "n")

# ggplot(data_with_labels, aes(x = V1, y = V2, color = cluster)) +
#   geom_point(size = 2) +
#   labs(title = "Clusters identificados pelo HDBSCAN", x = "X", y = "Y") +
#   theme_minimal() +
#   scale_color_manual(values = c("black", rainbow(length(unique(hdbscan_result$cluster)) - 1)))

# OPTICS ####

# Mesmos pacotes

# Gerando dados artificiais

set.seed(42)
cluster1 <- matrix(rnorm(100, mean = 5, sd = 1), ncol = 2)
cluster2 <- matrix(rnorm(100, mean = 10, sd = 1), ncol = 2)
cluster3 <- matrix(rnorm(50, mean = 15, sd = 1), ncol = 2)
dados <- rbind(cluster1, cluster2, cluster3)

# Aplicando o algoritmo OPTICS, ordenando os pontos e calculando suas distâncias de acessibilidade
resultado <- optics(dados, eps = 1.5, minPts = 5)

# Visualizando o gráfico de ordenamento/acessibilidade
plot(resultado)

# Extraindo clusters do OPTICS com limiar de densidade definido

clusters <- extractDBSCAN(resultado, eps_cl = 3)

# Corrigindo extração dos rótulos

clusters <- clusters$cluster

# Visualizando os clusters

plot(dados, 
     col = clusters + 1L, 
     pch = 19, 
     main = "Clusters Identificados pelo OPTICS", 
     xlab = "X", 
     ylab = "Y")
legend("topleft", legend = c("Cluster 1", "Cluster 2", "Cluster 3", "Ruído"),
       col = c(2, 3, 4, 1), pch = 19, bty = "n")
