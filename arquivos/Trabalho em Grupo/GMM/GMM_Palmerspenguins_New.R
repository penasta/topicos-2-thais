install.packages("palmerpenguins")
install.packages("mclust")
install.packages("ggplot2")
install.packages("gridExtra")
library(palmerpenguins)
library(mclust)
library(ggplot2)
library(gridExtra)


# Dados carregados, limpeza e seleção de variáveis
data("penguins")
penguins_clean <- na.omit(penguins[, c("bill_length_mm", "bill_depth_mm", "flipper_length_mm", "body_mass_g")])
penguins_scaled <- scale(penguins_clean)

# Modelo GMM
gmm_model <- Mclust(penguins_scaled)
gmm_model$classification
gmm_model$parameters
gmm_model$BIC

# Adicionar os clusters ao dataset original
penguins_clean$Cluster <- gmm_model$classification

# Gráficos
grafico1 <- ggplot(penguins_clean, aes(x = bill_length_mm, y = bill_depth_mm, color = as.factor(Cluster), fill = as.factor(Cluster))) +
  geom_point(size = 3) +
  stat_ellipse(geom = "polygon", alpha = 0.2, level = 0.95) + 
  stat_ellipse(geom = "polygon", alpha = 0.3, level = 0.80) + 
  stat_ellipse(geom = "polygon", alpha = 0.4, level = 0.50) +
  labs(
    title = "Clusters GMM - Bill Length vs Bill Depth",
    x = "Bill Length (mm)",
    y = "Bill Depth (mm)",
    color = "Cluster",
    fill = "Cluster"
  ) +
  theme_minimal()

grafico2 <- ggplot(penguins_clean, aes(x = flipper_length_mm, y = body_mass_g, color = as.factor(Cluster), fill = as.factor(Cluster))) +
  geom_point(size = 3) +
  stat_ellipse(geom = "polygon", alpha = 0.2, level = 0.95) +
  stat_ellipse(geom = "polygon", alpha = 0.3, level = 0.80) +
  stat_ellipse(geom = "polygon", alpha = 0.4, level = 0.50) +
  labs(
    title = "Clusters GMM - Flipper Length vs Body Mass",
    x = "Flipper Length (mm)",
    y = "Body Mass (g)",
    color = "Cluster",
    fill = "Cluster"
  ) +
  theme_minimal()

grafico3 <- ggplot(penguins_clean, aes(x = bill_length_mm, y = flipper_length_mm, color = as.factor(Cluster), fill = as.factor(Cluster))) +
  geom_point(size = 3) +
  stat_ellipse(geom = "polygon", alpha = 0.2, level = 0.95) +
  stat_ellipse(geom = "polygon", alpha = 0.3, level = 0.80) +
  stat_ellipse(geom = "polygon", alpha = 0.4, level = 0.50) +
  labs(
    title = "Clusters GMM - Bill Length vs Flipper Length",
    x = "Bill Length (mm)",
    y = "Flipper Length (mm)",
    color = "Cluster",
    fill = "Cluster"
  ) +
  theme_minimal()

grafico4 <- ggplot(penguins_clean, aes(x = bill_depth_mm, y = body_mass_g, color = as.factor(Cluster), fill = as.factor(Cluster))) +
  geom_point(size = 3) +
  stat_ellipse(geom = "polygon", alpha = 0.2, level = 0.95) +
  stat_ellipse(geom = "polygon", alpha = 0.3, level = 0.80) +
  stat_ellipse(geom = "polygon", alpha = 0.4, level = 0.50) +
  labs(
    title = "Clusters GMM - Bill Depth vs Body Mass",
    x = "Bill Depth (mm)",
    y = "Body Mass (g)",
    color = "Cluster",
    fill = "Cluster"
  ) +
  theme_minimal()
?mclust
# Organizando-os em um grid 2x2
grid.arrange(grafico1,grafico2,grafico3,grafico4, ncol = 2)



