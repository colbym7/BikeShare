library(tidyverse)
library(tidymodels)
library(vroom)
library(patchwork)
library(rpart)

# Read in data and check variable types #
train <- vroom('C:\\Users\\cjmsp\\Desktop\\Stat348\\BikeShare\\train.csv')
test <- vroom('C:\\Users\\cjmsp\\Desktop\\Stat348\\BikeShare\\test.csv')

glimpse(train)

# Change variable types #
train <- train |>
  mutate(
  season = as.factor(season),
  holiday = as.factor(holiday),
  workingday = as.factor(workingday),
  weather = as.factor(weather),
  humidity = as.integer(humidity),
  casual = as.integer(casual),
  registered = as.integer(registered),
  count = as.integer(count)
)
test <- test %>%
  mutate(
    season = as.factor(season),
    holiday = as.factor(holiday),
    workingday = as.factor(workingday),
    weather = as.factor(weather),
    humidity = as.integer(humidity),
)

# EDA #
GGally::ggpairs(train)
skimr::skim(train)
ggplot(data = train, aes(x=humidity)) + # View Density of Humidity
  geom_density()

plot1 <- ggplot(data = train, aes(x = datetime, y = count, color = season)) + 
  geom_point() +
  labs(title = "Number of Bike Rentals over Time")
plot2 <- ggplot(data = train, aes(x = weather)) +
  geom_bar() +
  labs(title = "Number of Bike Rentals for Different Weather Conditions")
plot3 <- ggplot(data = train, aes(x = temp, y = count, color = weather)) +
  geom_point() +
  scale_color_manual(values = c("red", "blue", "green", "black")) +
  geom_smooth(method = lm, formula = y ~ x, se = FALSE) +
  labs(title = "Temperature vs Bike Rentals")
plot4 <- ggplot(data = train, aes(x = temp, y = count, color = workingday)) +
  geom_point() +
  scale_color_manual(values = c('red', 'blue')) +
  labs(title = "Temperature vs Bike Rentals")
(plot1 + plot2) / (plot3 + plot4)

# No missing data, pretty even amount of observations for each season #
# Weather: only one observation with category 4 weather, better weather <- more obs #
# Temperature in celcius #
# atemp (feels like temperature) generally appears to be around temp +4
# Humidity Generally pretty high. Q1: 47 Q3: 77 
# Frequent periodic gaps of no bike rentals across time



# Data Cleaning #
train <- train %>%
  select(., -casual, -registered) %>%
  mutate(count = log(count))

# Basic Linear Regression

my_linear_model <- linear_reg() %>% #Type of model
  set_engine("lm") %>% # Engine = What R function to use
  set_mode("regression") %>% # Regression just means quantitative response6
  fit(formula=count~ . , data=train)
bike_predictions <- predict(my_linear_model,
                            new_data=test) # Use fit to predict
bike_predictions <- exp(bike_predictions)

# Prepare predictions for Kaggle Submission
kaggle_submission <- bike_predictions %>%
  bind_cols(., test) %>% #Bind predictions with test data
  select(datetime, .pred) %>% #Just keep datetime and prediction variables
  rename(count=.pred) %>% #rename pred to count (for submission to Kaggle)
  mutate(count=pmax(0, count)) %>% #pointwise max of (0, prediction)
  mutate(datetime=as.character(format(datetime))) #needed for right format to Kaggle

## Write out the file
vroom_write(x=kaggle_submission, file="C:\\Users\\cjmsp\\Desktop\\Stat348\\BikeShare\\LinearPreds.csv", delim=",")



### Feature Engineering ###

my_recipe <- recipe(count ~ ., data = train) %>%
  step_mutate(weather = ifelse(weather == 4, 3, weather)) %>%
  step_time(datetime, features = 'hour') %>%
  step_date(datetime, features="dow") %>%
  step_poly(matches("hour"), degree = 4) %>%
  step_mutate(season = as.factor(season)) %>%
  step_dummy(season)
prepped_recipe <- prep(my_recipe)
baked_data <- bake(prepped_recipe, new_data = NULL)

print(baked_data, n = 5)

lin_model <- linear_reg() %>%
  set_engine("lm") %>%
  set_mode("regression")

## Combine into a Workflow and fit
bike_workflow <- workflow() %>%
  add_recipe(my_recipe) %>%
  add_model(lin_model) %>%
  fit(data=train)

## Run all the steps on test data
lin_preds <- predict(bike_workflow, new_data = test)
lin_preds <- exp(lin_preds)

kaggle_submission2 <- lin_preds %>%
  bind_cols(., test) %>% #Bind predictions with test data
  select(datetime, .pred) %>% #Just keep datetime and prediction variables
  rename(count=.pred) %>% #rename pred to count (for submission to Kaggle)
  mutate(count=pmax(0, count)) %>% #pointwise max of (0, prediction)
  mutate(datetime=as.character(format(datetime))) #needed for right format to Kaggle


## Write out the file
vroom_write(x=kaggle_submission2, file="C:\\Users\\cjmsp\\Desktop\\Stat348\\BikeShare\\LinearPreds2.csv", delim=",")



### Penalized Regression ###
my_recipe_preg <- recipe(count ~ ., data = train) %>%
  step_mutate(weather = ifelse(weather == 4, 3, weather)) %>%
  step_time(datetime, features = 'hour') %>%
  step_date(datetime, features="dow") %>%
  step_poly(matches("hour"), degree = 4) %>%
  step_mutate(season = as.factor(season)) %>%
  step_dummy(season) %>%
  step_rm(datetime) %>%
  step_dummy(all_nominal_predictors()) %>% #make dummy variables
  step_normalize(all_numeric_predictors()) # Make mean 0, sd=1


 ## Experiment with 5 different penalty, and mixture combinations
preg_model <- linear_reg(penalty= .01,mixture = 1) %>%
  set_engine("glmnet")
## Combine into a Workflow and fit
preg_wf <- workflow() %>%
  add_recipe(my_recipe_preg) %>%
  add_model(preg_model) %>%
  fit(data=train)

## 5 sets of predictions for tuning parameter specifications
preg_preds1 <- predict(preg_wf, new_data = test)
preg_preds1 <- exp(preg_preds1) # Penalty 2, mixture .5
preg_preds2 <- predict(preg_wf, new_data = test)
preg_preds2 <- exp(preg_preds2) # Penalty .1, mixture .5
preg_preds3 <- predict(preg_wf, new_data = test)
preg_preds3 <- exp(preg_preds3) # Penalty .01, mixture .5
preg_preds4 <- predict(preg_wf, new_data = test)
preg_preds4 <- exp(preg_preds4) # Penalty .01, mixture 0
preg_preds5 <- predict(preg_wf, new_data = test)
preg_preds5 <- exp(preg_preds5) # Penalty .01, mixture 1

# Prepare 5 sets of predictions for upload to kaggle
kaggle_submission3 <- preg_preds1 %>%
  bind_cols(., test) %>% #Bind predictions with test data
  select(datetime, .pred) %>% #Just keep datetime and prediction variables
  rename(count=.pred) %>% #rename pred to count (for submission to Kaggle)
  mutate(count=pmax(0, count)) %>% #pointwise max of (0, prediction)
  mutate(datetime=as.character(format(datetime))) #needed for right format to Kaggle
vroom_write(x=kaggle_submission3, file="C:\\Users\\cjmsp\\Desktop\\Stat348\\BikeShare\\Preg1.csv", delim=",")

kaggle_submission4 <- preg_preds2 %>%
  bind_cols(., test) %>% #Bind predictions with test data
  select(datetime, .pred) %>% #Just keep datetime and prediction variables
  rename(count=.pred) %>% #rename pred to count (for submission to Kaggle)
  mutate(count=pmax(0, count)) %>% #pointwise max of (0, prediction)
  mutate(datetime=as.character(format(datetime))) #needed for right format to Kaggle
vroom_write(x=kaggle_submission4, file="C:\\Users\\cjmsp\\Desktop\\Stat348\\BikeShare\\Preg2.csv", delim=",")

kaggle_submission5 <- preg_preds3 %>%
  bind_cols(., test) %>% #Bind predictions with test data
  select(datetime, .pred) %>% #Just keep datetime and prediction variables
  rename(count=.pred) %>% #rename pred to count (for submission to Kaggle)
  mutate(count=pmax(0, count)) %>% #pointwise max of (0, prediction)
  mutate(datetime=as.character(format(datetime))) #needed for right format to Kaggle
vroom_write(x=kaggle_submission5, file="C:\\Users\\cjmsp\\Desktop\\Stat348\\BikeShare\\Preg3.csv", delim=",")

kaggle_submission6 <- preg_preds4 %>%
  bind_cols(., test) %>% #Bind predictions with test data
  select(datetime, .pred) %>% #Just keep datetime and prediction variables
  rename(count=.pred) %>% #rename pred to count (for submission to Kaggle)
  mutate(count=pmax(0, count)) %>% #pointwise max of (0, prediction)
  mutate(datetime=as.character(format(datetime))) #needed for right format to Kaggle
vroom_write(x=kaggle_submission6, file="C:\\Users\\cjmsp\\Desktop\\Stat348\\BikeShare\\Preg4.csv", delim=",")

kaggle_submission7 <- preg_preds5 %>%
  bind_cols(., test) %>% #Bind predictions with test data
  select(datetime, .pred) %>% #Just keep datetime and prediction variables
  rename(count=.pred) %>% #rename pred to count (for submission to Kaggle)
  mutate(count=pmax(0, count)) %>% #pointwise max of (0, prediction)
  mutate(datetime=as.character(format(datetime))) #needed for right format to Kaggle
vroom_write(x=kaggle_submission7, file="C:\\Users\\cjmsp\\Desktop\\Stat348\\BikeShare\\Preg5.csv", delim=",")



### Tuning ###
preg_model <- linear_reg(penalty = tune(),
                         mixture = tune())%>%
  set_engine('glmnet')

preg_wf <- workflow() %>%
  add_recipe(my_recipe_preg) %>%
  add_model(preg_model)

grid_tuning_params <- grid_regular(penalty(),
                                   mixture(),
                                   levels = 12)
folds <- vfold_cv(train, v = 5)

CV_results <- preg_wf %>%
  tune_grid(resamples=folds,
            grid = grid_tuning_params,
            metrics = metric_set(rmse))

collect_metrics(CV_results) %>%
  filter(.metric=='rmse') %>%
  ggplot(data=., aes(x=penalty, y=mean, color=factor(mixture))) +
  geom_line()

bestTune <- CV_results %>%
  select_best(metric='rmse')

final_wf <- preg_wf %>%
  finalize_workflow(bestTune) %>%
  fit(data=train)

tuned_preds <- final_wf %>%
  predict(new_data = test)
tuned_preds <- exp(tuned_preds)

kaggle_submission8 <- tuned_preds %>%
  bind_cols(., test) %>% #Bind predictions with test data
  select(datetime, .pred) %>% #Just keep datetime and prediction variables
  rename(count=.pred) %>% #rename pred to count (for submission to Kaggle)
  mutate(count=pmax(0, count)) %>% #pointwise max of (0, prediction)
  mutate(datetime=as.character(format(datetime))) #needed for right format to Kaggle

vroom_write(x=kaggle_submission8, file="C:\\Users\\cjmsp\\Desktop\\Stat348\\BikeShare\\tunedpreds.csv", delim=",")


  ### Regression Trees ###
tree_mod <- decision_tree(tree_depth = tune(),
                          cost_complexity = tune(),
                          min_n = tune()) %>%
  set_engine('rpart') %>%
  set_mode('regression')

tree_wf <- workflow() %>%
  add_recipe(my_recipe) %>%
  add_model(tree_mod)

grid_tree_tuning_params <- grid_regular(tree_depth(),
                                   cost_complexity(),
                                   min_n(),
                                   levels = 10)
folds <- vfold_cv(train, v = 5)

CV_results_tree <- tree_wf %>%
  tune_grid(resamples=folds,
            grid = grid_tree_tuning_params,
            metrics = metric_set(rmse))


bestTune <- CV_results_tree %>%
  select_best(metric='rmse')

finaltree_wf <- tree_wf %>%
  finalize_workflow(bestTune) %>%
  fit(data=train)

tree_preds <- finaltree_wf %>%
  predict(new_data = test)
tree_preds <- exp(tree_preds)

kaggle_submission9 <- tree_preds %>%
  bind_cols(., test) %>% #Bind predictions with test data
  select(datetime, .pred) %>% #Just keep datetime and prediction variables
  rename(count=.pred) %>% #rename pred to count (for submission to Kaggle)
  mutate(count=pmax(0, count)) %>% #pointwise max of (0, prediction)
  mutate(datetime=as.character(format(datetime))) #needed for right format to Kaggle

vroom_write(x=kaggle_submission9, file="C:\\Users\\cjmsp\\Desktop\\Stat348\\BikeShare\\treepreds.csv", delim=",")


### Random Forests ###
rf_mod <- rand_forest(mtry = tune(),
                      min_n=tune(),
                      trees = 1000) %>%
  set_engine("ranger") %>%
  set_mode("regression")

forest_wf <- workflow() %>%
  add_recipe(my_recipe) %>%
  add_model(rf_mod)

grid_forest_tuning_params <- grid_regular(mtry(range=c(1,10)),
                                        min_n(),
                                        levels = 10)
folds <- vfold_cv(train, v = 5)

CV_results_forest <- forest_wf %>%
  tune_grid(resamples=folds,
            grid = grid_forest_tuning_params,
            metrics = metric_set(rmse))


bestTune_forest <- CV_results_forest %>%
  select_best(metric='rmse')

finalforest_wf <- forest_wf %>%
  finalize_workflow(bestTune_forest) %>%
  fit(data=train)

forest_preds <- finalforest_wf %>%
  predict(new_data = test)
forest_preds <- exp(forest_preds)

kaggle_submission10 <- forest_preds %>%
  bind_cols(., test) %>% #Bind predictions with test data
  select(datetime, .pred) %>% #Just keep datetime and prediction variables
  rename(count=.pred) %>% #rename pred to count (for submission to Kaggle)
  mutate(count=pmax(0, count)) %>% #pointwise max of (0, prediction)
  mutate(datetime=as.character(format(datetime))) #needed for right format to Kaggle

vroom_write(x=kaggle_submission10, file="C:\\Users\\cjmsp\\Desktop\\Stat348\\BikeShare\\forestpreds.csv", delim=",")
