# Pacotes necessários
library(readxl)
library(naivebayes)
library(caret)

# Importação de dados
df_treino <- read_excel("Exemplo - naive bayes.xlsx", 
                        sheet = "Tabela", range = "B3:F21")
df_teste <- read_excel("Exemplo - naive bayes.xlsx", 
                        sheet = "Tabela", range = "B24:F30")

# Rodando o modelo
?naive_bayes()
modelo <- naive_bayes(Chuva ~ ., data = df_treino, laplace = 0, prior = NULL, usekernel = F) 
modelo

# Tabela com as probabilidades determinadas pelo modelo
probs <- predict(modelo, df_teste, type = 'prob')
probs

# Criando vetor com a previsão mais provável 
predicoes = character(nrow(probs))
for (i in 1:nrow(probs)){
  if (probs[i,1] > probs[i,2]){ predicoes[i] = 'Não' }
  else{ predicoes[i] = 'Sim' }
}
predicoes

# Apresentando os resultados do modelo na etapa de teste
resultados <- confusionMatrix(data = as.factor(predicoes),
                              reference = as.factor(df_teste$Chuva))
resultados