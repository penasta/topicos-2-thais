#################################################################
############ Aula 4: Tidymodels #################################
######## Profa. Thais - 11/2024  ################################
#################################################################

# Modeling packages
pacman::p_load(tidyverse,
               tidymodels,
               # Model interpretability packages:
               vip, # for variable importance
               rpart.plot # tree visualization
               ) 

##### Dataset: train + val + test
set.seed(123)
ames <- AmesHousing::make_ames()
split  <- initial_validation_split(ames, prop = c(0.6, 0.2), strata = "Sale_Price")
ames_train  <- training(split)
ames_val   <- validation(split)
ames_test   <- testing(split)

###### Training model 1
# Step 1: create decision tree model object
dt_mod <- decision_tree(mode = "regression") %>% set_engine("rpart")

# Step 2: create model recipe
# DT does not have pre-processing requirements
model_recipe <- recipe(
  Sale_Price ~ ., 
  data = ames_train
)

# Step 3: fit model workflow
dt_fit <- workflow() %>%
  add_recipe(model_recipe) %>%
  add_model(dt_mod) %>%
  fit(data = ames_train)

# Step 4: results
dt_fit

# Visualization
rpart.plot::rpart.plot(dt_fit$fit$fit$fit)
levels(ames$Overall_Qual)

# Predict
ames_val_res <- predict(dt_fit, new_data = ames_val %>% select(-Sale_Price))
ames_val_res <- bind_cols(ames_val_res, ames_val %>% select(Sale_Price))

ggplot(ames_val_res, aes(x = Sale_Price, y = .pred)) + 
  # Create a diagonal line:
  geom_abline(lty = 2) + 
  geom_point(alpha = 0.5) + 
  labs(y = "Predicted Sale Price (log10)", x = "Sale Price (log10)") +
  # Scale and size the x- and y-axis uniformly:
  coord_obs_pred()

# Metric
rmse(ames_val_res,  truth = Sale_Price, estimate = .pred)


###### Training model 2
# Step 1: create decision tree model object
dt_mod2 <- decision_tree(mode = "regression" ,min_n =100) %>% set_engine("rpart")

# Step 2: create model recipe - Same

# Step 3: fit model workflow
dt_fit2 <- workflow() %>%
  add_recipe(model_recipe) %>%
  add_model(dt_mod2) %>%
  fit(data = ames_train)

# Step 4: results
dt_fit2

# Visualization
rpart.plot::rpart.plot(dt_fit2$fit$fit$fit)

# Predict
ames_val_res2 <- predict(dt_fit2, new_data = ames_val %>% select(-Sale_Price))
ames_val_res2 <- bind_cols(ames_val_res2, ames_val %>% select(Sale_Price))

ggplot(ames_val_res2, aes(x = Sale_Price, y = .pred)) + 
  # Create a diagonal line:
  geom_abline(lty = 2) + 
  geom_point(alpha = 0.5) + 
  labs(y = "Predicted Sale Price (log10)", x = "Sale Price (log10)") +
  # Scale and size the x- and y-axis uniformly:
  coord_obs_pred()


###### Model comparison: Metric RMSE
rmse(ames_val_res,  truth = Sale_Price, estimate = .pred)
rmse(ames_val_res2,  truth = Sale_Price, estimate = .pred)


##### Model choice: dt_fit2
## Fit again train+val
ames_train_all <- rbind(ames_train, ames_val)

# Step 1: create decision tree model object - Same
dt_mod2 <- decision_tree(mode = "regression" ,min_n =100) %>% set_engine("rpart")
# Step 2: create model recipe
model_recipe <- recipe(
  Sale_Price ~ ., 
  data = ames_train_all
)
# Step 3: fit model workflow
dt_fit_final <- workflow() %>%
  add_recipe(model_recipe) %>%
  add_model(dt_mod2) %>%
  fit(data = ames_train_all)

# Visualization
rpart.plot::rpart.plot(dt_fit_final$fit$fit$fit)

# Predict
ames_test_res <- predict(dt_fit_final, new_data = ames_test %>% select(-Sale_Price))
ames_test_res <- bind_cols(ames_test_res, ames_test %>% select(Sale_Price))

ggplot(ames_test_res, aes(x = Sale_Price, y = .pred)) + 
  # Create a diagonal line:
  geom_abline(lty = 2) + 
  geom_point(alpha = 0.5) + 
  labs(y = "Predicted Sale Price (log10)", x = "Sale Price (log10)") +
  # Scale and size the x- and y-axis uniformly:
  coord_obs_pred()

# Metric
rmse(ames_test_res,  truth = Sale_Price, estimate = .pred)
rmse(ames_val_res2,  truth = Sale_Price, estimate = .pred)

# Variable Importance
dt_fit_final %>%
  extract_fit_parsnip() %>%
  vip(20)
