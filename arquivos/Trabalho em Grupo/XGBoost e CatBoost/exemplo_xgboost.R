if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, readxl, janitor,xgboost,tidymodels,vip,rpart.plot,
               conflicted,tictoc,finetune,doParallel,DataExplorer,future)

tidymodels_prefer()

iris$Species = factor(iris$Species)

df = clean_names(iris)

# Modelagem
set.seed(150167636)
split = initial_validation_split(df, prop = c(0.6, 0.2), strata = species)
train = training(split)
val = validation(split)
test = testing(split)

model = boost_tree(mode = "classification",
                   trees = tune(),
                   tree_depth = tune(),
                   learn_rate = tune(),
                   loss_reduction = tune()
) %>%
  set_engine("xgboost")

recipe = recipe(species ~ sepal_length + sepal_width + petal_length + petal_width,
                data = df)

wf = workflow() %>%
  add_recipe(recipe) %>%
  add_model(model)

grid = wf %>%
  extract_parameter_set_dials() %>%
  grid_regular(levels = 3)

plan(multisession)
set.seed(150167636)
tic()
tune_res = tune_grid( 
  wf,
  resamples = vfold_cv(val, v = 5,strata = species),
  grid = grid,
  metrics = metric_set(accuracy, roc_auc, sens,spec)
)
toc()

best_params = tune_res %>%
  select_best(metric = "roc_auc")

best_params

tune_res %>%
  collect_metrics() %>%
  filter(.metric == "roc_auc") %>%
  select(mean, trees:loss_reduction) %>%
  pivot_longer(trees:loss_reduction,
               names_to = "hiperparâmetro",
               values_to = "valor") %>%
  ggplot(aes(valor, mean, color = hiperparâmetro)) +
  geom_point(show.legend = FALSE)+
  facet_wrap(~hiperparâmetro, scales = "free_x")

best_wf = finalize_workflow(wf, best_params)

final_fit <- best_wf %>%
  fit(data = train)

final_fit %>%
  extract_fit_parsnip() %>%
  vip(geom = "point")

predictions <- final_fit %>%
  predict(new_data = test)

results <- bind_cols(test, predictions)

results %>%
  conf_mat(species,.pred_class)

metrics <- metric_set(accuracy, specificity, sensitivity)

augment(final_fit, new_data = test) %>%
  metrics(truth = species, estimate = .pred_class) %>%
  select(.metric,.estimate)

#######

# Dados obtidos de:
# https://www.kaggle.com/datasets/isathyam31/adult-income-prediction-classification/data

# Adult Income Prediction Classification
# Forecasting Earnings Potential Based on Demographic and Employment Factors

# Variáveis

# workclass: The type of employment (e.g., Private, Self-emp-not-inc, Federal-gov, Local-gov)
# fnlwgt: The number of people the census believes the entry represents
# education: The highest level of education achieved
# education-num: The numeric representation of the previous column
# marital-status: The marital status of the individual
# occupation: The occupation of the individual
# relationship: The relationship of the individual to their household
# race: The race of the individual
# sex: The gender of the individual
# capital-gain: The capital gains of the individual
# capital-loss: The capital losses of the individual
# hours-per-week: The number of hours the individual works per week
# country: The native country of the individual
# salary: The income level of the individual, which is the target variable to predict.

# Objetivo: Prever se o salário é inferior a 50 mil dólares anuais (0) ou superior a 50 mil dólares anuais (1)

#######

df = read_csv("dados/data.csv")
df$salary = factor(df$salary)

df = clean_names(df)

# Modelagem
set.seed(150167636)
split = initial_validation_split(df, prop = c(0.6, 0.2), strata = salary)
train = training(split)
val = validation(split)
test = testing(split)

model = boost_tree(mode = "classification",
                   trees = tune(),
                   tree_depth = tune()#,
                   # learn_rate = tune()
) %>%
  set_engine("xgboost")

recipe = recipe(salary ~ age + workclass + education + marital_status + occupation + relationship + race + sex + capital_gain + capital_loss + hours_per_week + country,
                data = df) %>%
  step_dummy(all_nominal_predictors()) 

wf = workflow() %>%
  add_recipe(recipe) %>%
  add_model(model)

grid = wf %>%
  extract_parameter_set_dials() %>%
  grid_regular(levels = 3)

plan(multisession)
set.seed(150167636)
tic()
tune_res = tune_grid( 
  wf,
  resamples = vfold_cv(val, v = 5, strata = salary),
  grid = grid,
  metrics = metric_set(accuracy, roc_auc, sens,spec)
)
toc()

best_params = tune_res %>%
  select_best(metric = "roc_auc")

best_params

tune_res %>%
  collect_metrics() %>%
  filter(.metric == "roc_auc") %>%
  select(mean, trees:tree_depth) %>%
  pivot_longer(trees:tree_depth,
               names_to = "hiperparâmetro",
               values_to = "valor") %>%
  ggplot(aes(valor, mean, color = hiperparâmetro)) +
  geom_point(show.legend = FALSE)+
  facet_wrap(~hiperparâmetro, scales = "free_x")

best_wf = finalize_workflow(wf, best_params)

tic()
final_fit <- best_wf %>%
  fit(data = train)
toc()

final_fit %>%
  extract_fit_parsnip() %>%
  vip(geom = "point")

predictions <- final_fit %>%
  predict(new_data = test)

results <- bind_cols(test, predictions)

results %>%
  conf_mat(salary,.pred_class)

metrics <- metric_set(accuracy, specificity, sensitivity)

augment(final_fit, new_data = test) %>%
  metrics(truth = salary, estimate = .pred_class) %>%
  select(.metric,.estimate)
