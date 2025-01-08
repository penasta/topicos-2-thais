#################################################################
############ Aula 3: Tidymodels #################################
######## Profa. Thais - 11/2024  ################################
#################################################################


##### Judging Model Effectiveness: yardstick package
# empirical validation: using data that were not used to create the model
# as the substrate to measure effectiveness
# inference x prediction model
# tidymodels.org/find


# código da aula 02
pacman::p_load(tidymodels)
tidymodels_prefer()
data(ames) ; ames <- mutate(ames, Sale_Price = log10(Sale_Price))

set.seed(502)
ames_split <- initial_split(ames, prop = 0.80, strata = Sale_Price)
ames_train <- training(ames_split)
ames_test  <- testing(ames_split)

ames_rec <- 
  recipe(Sale_Price ~ Neighborhood + Gr_Liv_Area + Year_Built + Bldg_Type + 
           Latitude + Longitude, data = ames_train) %>%
  step_log(Gr_Liv_Area, base = 10) %>% 
  step_other(Neighborhood, threshold = 0.01) %>% 
  step_dummy(all_nominal_predictors()) %>% 
  step_interact( ~ Gr_Liv_Area:starts_with("Bldg_Type_") ) %>% 
  step_ns(Latitude, Longitude, deg_free = 20)

lm_model <- linear_reg() %>% set_engine("lm")

lm_wflow <- 
  workflow() %>% 
  add_model(lm_model) %>% 
  add_recipe(ames_rec)

lm_fit <- fit(lm_wflow, ames_train)
tidy(lm_fit) |> print(n=Inf)

## prediction
ames_test_res <- predict(lm_fit, new_data = ames_test %>% select(-Sale_Price))
ames_test_res

ames_test_res <- bind_cols(ames_test_res, ames_test %>% select(Sale_Price))
ames_test_res

ggplot(ames_test_res, aes(x = Sale_Price, y = .pred)) + 
  # Create a diagonal line:
  geom_abline(lty = 2) + 
  geom_point(alpha = 0.5) + 
  labs(y = "Predicted Sale Price (log10)", x = "Sale Price (log10)") +
  # Scale and size the x- and y-axis uniformly:
  coord_obs_pred()

## Metrics |---> ::Yardstick 
rmse(ames_test_res, truth = Sale_Price, estimate = .pred)

ames_metrics <- metric_set(rmse, rsq, mae)
ames_metrics(ames_test_res, truth = Sale_Price, estimate = .pred)


## Binary classification metrics
data(two_class_example) # from modeldata package
?two_class_example
tibble(two_class_example)

# A confusion matrix: 
conf_mat(two_class_example, truth = truth, estimate = predicted)

# Accuracy:
accuracy(two_class_example, truth, predicted)

# Matthews correlation coefficient:
mcc(two_class_example, truth, predicted)

# F1 metric:
f_meas(two_class_example, truth, predicted)

# Combining these three classification metrics together
classification_metrics <- metric_set(accuracy, mcc, f_meas)
classification_metrics(two_class_example, truth = truth, estimate = predicted)

## obs
# Default: the first level of the outcome factor is the event of interest.
head(two_class_example$predicted) # class1 is the positive/event of interest
f_meas(two_class_example, truth, predicted, event_level = "second") # change the event of interest


## using class probability metrics
two_class_curve <- roc_curve(two_class_example, truth, Class1)
two_class_curve
autoplot(two_class_curve)

roc_auc(two_class_example, truth, Class1)

prc <- pr_curve(two_class_example, truth, Class1)
autoplot(prc)


# Multiclass Classification Metrics
data(hpc_cv)
tibble(hpc_cv)
levels(hpc_cv$obs)

accuracy(hpc_cv, obs, pred)
mcc(hpc_cv, obs, pred)

# manual calculation
class_totals <- 
  count(hpc_cv, obs, name = "totals") %>% 
  mutate(class_wts = totals / sum(totals))
class_totals

cell_counts <- 
  hpc_cv %>% 
  group_by(obs, pred) %>% 
  count() %>% 
  ungroup()

# Compute the four sensitivities using 1-vs-all
one_versus_all <- 
  cell_counts %>% 
  filter(obs == pred) %>% 
  full_join(class_totals, by = "obs") %>% 
  mutate(sens = n / totals)
one_versus_all


# Three different estimates:
one_versus_all %>% 
  summarize(
    macro = mean(sens), 
    macro_wts = weighted.mean(sens, class_wts),
    micro = sum(n) / sum(totals)
  )

# yardstick
sensitivity(hpc_cv, obs, pred, estimator = "macro")
sensitivity(hpc_cv, obs, pred, estimator = "macro_weighted")
sensitivity(hpc_cv, obs, pred, estimator = "micro")

roc_auc(hpc_cv, obs, VF, F, M, L)
roc_curve(hpc_cv, obs, VF, F, M, L) %>%  autoplot()


# using CV folds
hpc_cv %>% 
  group_by(Resample) %>% 
  accuracy(obs, pred)

# Four 1-vs-all ROC curves for each fold
hpc_cv %>% 
  group_by(Resample) %>% 
  roc_curve(obs, VF, F, M, L) %>% 
  autoplot()

# ::parsnip
# ::recipe
# ::yardstick

