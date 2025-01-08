#################################################################
############ Aula 2: Tidymodels #################################
######## Profa. Thais - 11/2024  ################################
#################################################################


##### Recipes for pre-processing and feature engineering tasks
library(tidymodels)
tidymodels_prefer()

## Transformando: log10(Sale_Price) 
ames <- ames %>% mutate(Sale_Price = log10(Sale_Price))


## Divisão treino/teste
## Stratified sampling
set.seed(502)
ames_split <- initial_split(ames, prop = 0.80, strata = Sale_Price)
ames_train <- training(ames_split)
ames_test  <- testing(ames_split)
dim(ames_train)


## exemplo lm()
lm1 <- lm(Sale_Price ~ Neighborhood + log10(Gr_Liv_Area) + Year_Built + Bldg_Type,
          data = ames_train)
predict(lm1, ames_test %>% slice(1:5))
# When this function is executed, the data are converted from a data frame to a numeric design matrix (also called a model matrix)
# Unlike the formula method inside a modeling function, the recipe defines the steps via step_*() functions without immediately executing them;

## Exemplo sem recipe()
lm_model <- 
  linear_reg() %>% 
  set_engine("lm")

lm_form_fit <- 
  lm_model %>% 
  fit(Sale_Price ~ Neighborhood + log10(Gr_Liv_Area) + Year_Built + Bldg_Type,
      data = ames_train)

lm_form_fit
predict(lm_form_fit, ames_test %>% slice(1:5))


## Exemplo com recipe()

# 1. Definir pré-processamento -> recipe
# 2. Definir o modelo (modelo;motor;modo)
# 3. Ajuste

simple_ames <- 
  recipe(Sale_Price ~ Neighborhood + Gr_Liv_Area + Year_Built + Bldg_Type,
         data = ames_train) %>%
  step_log(Gr_Liv_Area, base = 10) %>% 
  step_dummy(all_nominal_predictors())
simple_ames 

# Advantages
# These computations can be recycled across models since they are not tightly coupled to the modeling function.
# A recipe enables a broader set of data processing choices than formulas can offer.
# The syntax can be very compact. For example, all_nominal_predictors() 
# All data processing can be captured in a single R object instead of in scripts that are repeated, or even spread across different files.

lm_model <- 
  linear_reg() %>% 
  set_engine("lm")

lm_wflow <- 
  workflow() %>% 
  add_model(lm_model) %>% 
  add_recipe(simple_ames)
lm_wflow

lm_fit <- fit(lm_wflow, ames_train)
tidy(lm_fit) %>% print(n=Inf)

predict(lm_fit, ames_test %>% slice(1:5))


# Steps
## Define recipe, model.
## Build a workflow
## Fit workflow
## Predict


# Get the recipe after it has been estimated:
lm_fit %>% 
  extract_recipe(estimated = TRUE)

# To tidy the model fit: 
lm_fit %>% 
  extract_fit_parsnip() %>% 
  tidy() 


### There are many step function:
ggplot(ames, aes(y=Neighborhood)) + geom_bar()

# step_other: the bottom 1% of the neighborhoods will be lumped into a new level called “other.” 
# step_unknown: change missing values to a dedicated factor level
simple_ames <- 
  recipe(Sale_Price ~ Neighborhood + Gr_Liv_Area + Year_Built + Bldg_Type,
         data = ames_train) %>%
  step_log(Gr_Liv_Area, base = 10) %>% 
  step_unknown(Neighborhood, new_level = "unknown Neighborhood") %>%
  step_other(Neighborhood, threshold = 0.01) %>% # Criar uma classe 'outos' (Other), evitando classes sem ou quase sem observações
  step_dummy(all_nominal_predictors())

lm_wflow <- 
  workflow() %>% 
  add_model(lm_model) %>% 
  add_recipe(simple_ames)
lm_wflow

lm_fit <- fit(lm_wflow, ames_train)
lm_fit

predict(lm_fit, ames_test %>% slice(1:5))

## Include interaction terms:
ggplot(ames_train, aes(x = Gr_Liv_Area, y = 10^Sale_Price)) + 
  geom_point(alpha = .2) + 
  facet_wrap(~ Bldg_Type) + 
  geom_smooth(method = lm, formula = y ~ x, se = FALSE, color = "lightblue") + 
  scale_x_log10() + 
  scale_y_log10() + 
  labs(x = "Gross Living Area", y = "Sale Price (USD)")

## Base R: Formula for interaction
Sale_Price ~ Neighborhood + log10(Gr_Liv_Area) + Bldg_Type + 
  log10(Gr_Liv_Area):Bldg_Type
# or
Sale_Price ~ Neighborhood + log10(Gr_Liv_Area) * Bldg_Type 


## recipe()
# Recipes are more explicit and sequential, and they give you more control. 
# The order is important!
simple_ames <- 
  recipe(Sale_Price ~ Neighborhood + Gr_Liv_Area + Year_Built + Bldg_Type,
         data = ames_train) %>%
  step_log(Gr_Liv_Area, base = 10) %>% 
  step_other(Neighborhood, threshold = 0.01) %>% 
  step_dummy(all_nominal_predictors()) %>% 
  # Gr_Liv_Area is on the log scale from a previous step
  step_interact( ~ Gr_Liv_Area:starts_with("Bldg_Type_") ) # Considerando a mudança de nome da variável Bldg_Type, que se transformou em diversas variáveis Dummy. Por isso o " _ " 


lm_wflow <- 
  workflow() %>% 
  add_model(lm_model) %>% 
  add_recipe(simple_ames)
lm_wflow

lm_fit <- fit(lm_wflow, ames_train)
tidy(lm_fit) %>% print(n=33)

predict(lm_fit, ames_test %>% slice(1:5))


