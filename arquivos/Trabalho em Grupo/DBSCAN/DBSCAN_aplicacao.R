# Aplicação - Topicos 2 - DBSCAN

# Importar os pacotes necessários.

library(dbscan)
library(dplyr)
library(ggplot2)

library(cluster)
library(GGally)
library(plotly)

# Exemplo 1 - Faithful Dataset ----

# 2 variáveis: Eruptions e Waiting

# Eruptions: duração da erupção do gêiser (minutos).
# Waiting: tempo de espera entre erupções (minutos).


data("faithful")
df_faithful <- faithful  


head(df_faithful)
summary(df_faithful)


# Normalizar os dados 
df_faithful_normalized <- scale(df_faithful)

# Estimativa Heurística de Parâmetros:

# 1. MinPts = dim + 1  (Valores maires podem ser usados)

# 2. Inspecionar o k-NN distance plot para k = minPts - 1 

minPts = dim(df_faithful_normalized)[2]+1

?kNNdistplot

kNNdistplot(df_faithful_normalized, minPts = minPts)

abline(h=.1, col = "red", lty = 2)
abline(h=.15, col = "red", lty = 2)
abline(h=.19, col = "red", lty = 2)

# Aplicar DBSCAN

?dbscan
db <- dbscan(df_faithful_normalized, eps = .19, minPts = minPts)
db

# Visualizar
ggplot(df_faithful_normalized, aes(x = eruptions, y = waiting, color = as.factor(db$cluster))) +
  geom_point(size = 3, alpha = 0.7) +
  theme_minimal() +
  labs(title = "DBSCAN no dataset Faithful", color = "Clusters")


# Exemplo 2 - IRIS Dataset ----

# sepal_length: Comprimento da sépala (geralmente em centímetros). A sépala é a parte externa da flor que protege os botões antes de florescerem.
# sepal_width: Largura da sépala (em centímetros). Representa a medida transversal da sépala.
# petal_length: Comprimento da pétala (em centímetros). A pétala é a parte interna e colorida da flor, geralmente usada para atrair polinizadores.
# petal_width: Largura da pétala (em centímetros). Mede a largura transversal da pétala.


# Carregar dados

data(iris)

iris_data <- as.matrix(iris[, 1:4])
iris_species <- as.matrix(iris[, 5])  # A coluna de species


head(iris_data)
summary(iris_data)

# Normalizar os dados 
iris_normalized <- scale(iris_data)

# Estimativa Heurística de Parâmetros:
# 1. MinPts = dim + 1 = 5   (Valores maires podem ser usados)

# 2. Inspecionar o k-NN distance plot para k = minPts - 1 = 4

minPts = dim(iris_normalized)[2]+1
kNNdistplot(iris_normalized, minPts = minPts)

## O ruído parece começar em torno de uma distância de 4-NN de entre 0,7 e 1

abline(h=.7, col = "red", lty = 2)
abline(h=1, col = "red", lty = 2)

## Agrupamento com os parametros definidos

db <- dbscan(iris_normalized, eps = .7, minPts = 5)
db

# Gráfico de dispersão para cada par de variáveis

# Como dbscan() pode rotular alguns pontos como ruído (cluster = 0)
# somamos 1L para evitar que o ruído tenha a cor preta (cor padrão de pontos não especificados no pairs()).

pairs(iris_normalized, col = db$cluster + 1L) 

# Calcular a correlação entre as variáveis
correlation_matrix <- cor(iris_normalized)

#  Identificar as duas variáveis mais correlacionadas
max_cor <- which(correlation_matrix == max(correlation_matrix[lower.tri(correlation_matrix)]), arr.ind = TRUE)
var1 <- colnames(iris_normalized)[max_cor[1]]
var2 <- colnames(iris_normalized)[max_cor[2]]

# Exibir as variáveis mais correlacionadas
cat("As duas variáveis mais correlacionadas são:", var1, "e", var2, "\n")

#  Visualizar o agrupamento em 2D usando as variáveis mais correlacionadas
plot(iris_normalized[, var1], iris_normalized[, var2], col = db$cluster + 1L, pch = 19, xlab = var1, ylab = var2)
legend("topleft", legend = unique(db$cluster), col = unique(db$cluster) + 1L, pch = 19)

# Verificar se os agrupamentos fazem sentido com base na variável 'Species'
table(db$cluster, iris_species)

# PCA
# Aplicar PCA para reduzir para 2 dimensões

# Carregar dataset
data(iris)
df <- as.matrix(iris[, 1:4])  # Apenas variáveis numéricas

# Aplicar PCA para reduzir para 2 dimensões

pca <- prcomp(df, scale = T)

#  Variância explicada por cada componente principal.
# Contribuição de cada variável para cada componente principal

pca

# Variância explicada
variancia_explicada <- (pca$sdev^2) / sum(pca$sdev^2) * 100
df_var <- data.frame(PC = paste0("PC", 1:length(variancia_explicada)),
                     Variancia = variancia_explicada)

# Scree Plot
ggplot(df_var, aes(x = PC, y = Variancia)) +
  geom_bar(stat = "identity", fill = "steelblue", alpha = 0.7) +
  geom_line(aes(group = 1), color = "red", linetype = "dashed", size = 1) +
  geom_point(color = "red", size = 3) +
  theme_minimal() +
  labs(title = "Scree Plot - Variância Explicada por Componente",
       x = "Componentes Principais",
       y = "Variância Explicada (%)") +
  ylim(0, max(variancia_explicada) + 5)

# Dois primeiros componentes

df_pca <- as.data.frame(pca$x[, 1:2])  
df_pca


# Estimativa Heurística de Parâmetros:
# 1. MinPts = dim + 1 = 3
  
# 2. Inspecionar o k-NN distance plot para k = minPts - 1 = 2
  
minPts = dim(df_pca)[2]+1
kNNdistplot(df_pca, minPts = minPts)

## O ruído parece começar em torno de uma distância de 4-NN de entre 0,4 e 0,7

abline(h=.4, col = "red", lty = 2)
abline(h=.5, col = "red", lty = 2)
abline(h=.7, col = "red", lty = 2)
# Aplicar DBSCAN no espaço PCA

db <- dbscan(df_pca, eps = 0.5, minPts = minPts)
db

# Adicionar os clusters ao dataframe
df_pca$Cluster <- as.factor(db$cluster)

# Visualizar clusters no espaço PCA
ggplot(df_pca, aes(x = PC1, y = PC2, color = Cluster)) +
  geom_point(size = 3, alpha = 0.7) +
  theme_minimal() +
  labs(title = "DBSCAN aplicado ao Iris Dataset (PCA)", color = "Clusters")


# Exemplo 3 ####

# https://rpubs.com/TusVasMit/DBSCANExploration
