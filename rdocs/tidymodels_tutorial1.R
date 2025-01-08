################################################################################
#                                                                              #
#                              TIDYMODELS                                      #
#                                                                              #
################################################################################

if(!require('pacman')){install.packages('pacman')}
p_load(tidymodels)

# ::rsample: dividir o conjunto de dados
# ::parsnip: ajustar o modelo
# ::tune: ajustar hiperparâmetros do modelo
# ::yardstick: ?
# ::modeldata: banco de dados
# ::recipes: Ajustes nos dados originais

# Priorizando o tidymodels em relação as funções com conlitos
tidymodels_prefer()

# Conjunto de dados:
#?ames

ggplot(ames, aes(x=Sale_Price)) +
  geom_histogram(bins = 30, fill = "lightblue"#, color = ""
  ) +
  labs(title = "Distribution os Sales Prices",
       x = "Sale Price",
       y = "Number of Houses")

ggplot(ames, aes(x=Sale_Price)) +
  geom_histogram(bins = 30, fill = "lightblue"#, color = ""
  ) +
  labs(title = "Distribution os Sales Prices",
       x = "Sale Price",
       y = "Number of Houses") +
  scale_x_log10()

# Alterando a escala da variável
ames = ames |> mutate(Sale_Price = log10(Sale_Price))

# split (AAS) the data into training and testing sets
set.seed(123)
ames_split = initial_split(ames, prop = .8)
#ames_split
ames_train = training(ames_split)
ames_test = testing(ames_split)
dim(ames_train)
dim(ames_test)

# split (AAS) the data into training, validation and testing sets
set.seed(123)
ames_split = initial_validation_split(ames, prop = c(.6,.2))
ames_train = training(ames_split)
ames_test = testing(ames_split)
ames_validation = validation(ames_split)
dim(ames_train)
dim(ames_test)
dim(ames_validation)

# Verificando a representatividade dos dados no conjunto de treino
ggplot(ames_train, aes(x=Sale_Price)) +
  geom_histogram(bins = 30, fill = "lightblue"#, color = ""
  ) +
  labs(title = "Distribution os Sales Prices in training set",
       x = "Sale Price",
       y = "Number of Houses") +
  scale_x_log10()

# AAS Não vai servir para este contexto, para garantir a representatividade da cauda
# da distribuição no conjunto de treino

# Portanto, será necessária uma amostragem estratificada 

# split (AE) the data into training and testing sets
set.seed(123)
ames_split = initial_split(ames, prop = .8,
                           strata = Sale_Price # Com este parâmetro, estamos estratificando a amostra por esta variável, segundo seus 4 quantis (dá pra mudar isso com outro parâmetro)
                           )
ames_train = training(ames_split)
ames_test = testing(ames_split)
dim(ames_train)
dim(ames_test)

ggplot(ames_train, aes(x=Sale_Price)) +
  geom_histogram(bins = 30, fill = "lightblue"#, color = ""
  ) +
  labs(title = "Distribution os Sales Prices in training set",
       x = "Sale Price",
       y = "Number of Houses") +
  scale_x_log10()

###### ::Parsnip: Definição do modelo:
# 1) Técnica estatística ("Nome do modelo") [Regressão linear, reg. poisson, KNN, ...]
# 2) "Engine" (pacote que será utilizado para o ajuste)
# 3) "Mode" (Tipo de inferência. Ex: Regressão; Classificação)

## Model definition
# Regressão linear
linear_reg() %>%
  set_engine("lm") %>%
  set_mode("regression") # Neste caso, este parâmetro é opcional

# Modelo especificado, sem nem informar o conjunto de dados!
# Primeiro 'seta' o modelo, e ajusta após indicando o conjunto de dados!

# Outras alternativas ao lm
linear_reg() %>%
  set_engine("glmnet") %>% 
  set_mode("regression")

linear_reg() %>%
  set_engine("stan") %>%
  set_mode("regression")

#####
linear_reg() %>%
  set_engine("lm") %>%
  translate() # Detalha o código utilizado para o ajuste

linear_reg() %>%
  set_engine("stan") %>%
  translate() # Detalha o código utilizado para o ajuste

#####
# Fit the model
lm_model = linear_reg() %>%
  set_engine("lm") %>%
  set_mode("regression")

lm_fit = lm_model %>% fit(Sale_Price ~ Longitude + Latitude,
                          data = ames_train)

lm_fit
#view(lm_fit)
tidy(lm_fit)

lm_fit %>% extract_fit_engine()
lm_fit %>% extract_fit_engine() %>% coef()
lm_fit %>% extract_fit_engine() %>% summary()

######
# Outra forma
lm_fit = lm_model %>%
  fit_xy(y = ames_train %>% pull(Sale_Price),
         x = ames_train %>% select(Longitude,Latitude))

## Predição
ames_test_small = ames_test %>% slice(1:5)
predict(lm_fit, new_data = ames_test_small)

## Addin
p_load(shiny, miniUI, rstudioapi)
parsnip_addin()

decision_tree_rpart_spec <-
  decision_tree(tree_depth = tune(), min_n = tune(), cost_complexity = tune()) %>%
  set_engine('rpart') %>%
  set_mode('classification')

decision_tree_rpart_spec <-
  decision_tree() %>%
  set_engine('rpart') %>%
  set_mode('classification')


rm(list = ls())
