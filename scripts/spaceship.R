library(discrim)
library(doParallel)
library(tidyverse)
library(tidymodels)
library(patchwork)



# Setup -------------------------------------------------------------------

theme_set(theme_minimal())

path_to_data <- "./data/"

clean_spaceship <- 
  function(spaceship_data) {
    spaceship_data |> 
      separate(Cabin, into = paste0("Cabin", c("Deck", "Num", "Side"))) |> 
      mutate(TravelGroup = str_extract(PassengerId, "^[0-9]*")) |> 
      mutate(GroupSize = n(), .by = TravelGroup) |> 
      mutate_if(is.logical, as.numeric) |> 
      mutate(across(c(HomePlanet, starts_with("Cabin"), Destination,
                      Transported, GroupSize, CryoSleep, VIP), factor)) |> 
      select(PassengerId, GroupSize, everything()) |> 
      select(-TravelGroup)
  }



# Data Import and Cleaning ------------------------------------------------

# import data
train_data <- 
  fs::dir_ls(path_to_data, glob = "*/train.csv") |> 
  read_csv()

test_data <- 
  fs::dir_ls(path_to_data, glob = "*/test.csv") |> 
  read_csv() |> 
  mutate(Transported = NA)

# clean data

train_data <- clean_spaceship(train_data)

test_data <- clean_spaceship(test_data)

glimpse(train_data)



# EDA and Feature Engineering ---------------------------------------------

# get overview of dataa
glimpse(train_data)
summary(train_data)

# explore `NAs`
(map_df(train_data, ~ sum(is.na(.))) |> unlist()) #/ nrow(train_data)
(map_df(test_data, ~ sum(is.na(.))) |> unlist()) #/ nrow(test_data)

# explore target column: Transported
train_data |> count(Transported)

# explore numeric columns - Age
# Feature engineering: normalize / discretize
summary(train_data$Age)

(
  train_data |> 
    ggplot(aes(x = Age, fill = Transported)) +
    geom_histogram() +
    facet_wrap(. ~ Transported, nrow = 2)
) + (
  train_data |> 
    ggplot(aes(x = Age, y = Transported, fill = Transported)) +
    geom_boxplot()
) + 
  plot_layout(guides = "collect")

# explore numeric columns - RoomService
# Feature engineering: log10, discretize
summary(train_data$RoomService)

(
  train_data |> 
    ggplot(aes(x = RoomService, fill = Transported)) +
    geom_histogram() +
    scale_x_log10() +
    facet_wrap(. ~ Transported, scales = "free_y")
) + (
  train_data |> 
    ggplot(aes(x = RoomService, y = Transported, fill = Transported)) +
    scale_x_log10() +
    geom_boxplot()
) + 
  plot_layout(guides = "collect")

# explore numeric columns - FoodCourt
# Feature engineering: log10, discretize
summary(train_data$FoodCourt)

(
  train_data |> 
    ggplot(aes(x = FoodCourt, fill = Transported)) +
    geom_histogram() +
    scale_x_log10() +
    facet_wrap(. ~ Transported, scales = "free_y")
) + (
  train_data |> 
    ggplot(aes(x = FoodCourt, y = Transported, fill = Transported)) +
    scale_x_log10() +
    geom_boxplot()
) + 
  plot_layout(guides = "collect")


# explore numeric columns - ShoppingMall
# Feature engineering: log10, discretize
summary(train_data$ShoppingMall)

(
  train_data |> 
    ggplot(aes(x = ShoppingMall, fill = Transported)) +
    geom_histogram() +
    scale_x_log10() +
    facet_wrap(. ~ Transported, scales = "free_y")
) + (
  train_data |> 
    ggplot(aes(x = ShoppingMall, y = Transported, fill = Transported)) +
    scale_x_log10() +
    geom_boxplot()
) + 
  plot_layout(guides = "collect")

# explore numeric columns: Spa
# Feature engineering: log10, discretize
summary(train_data$Spa)

(
  train_data |> 
    ggplot(aes(x = Spa, fill = Transported)) +
    geom_histogram() +
    scale_x_log10() +
    facet_wrap(. ~ Transported, scales = "free_y")
) + (
  train_data |> 
    ggplot(aes(x = Spa, y = Transported, fill = Transported)) +
    scale_x_log10() +
    geom_boxplot()
) + 
  plot_layout(guides = "collect")

# explore numeric columns: VRDeck
# Feature engineering: log10, discretize
summary(train_data$VRDeck)

(
  train_data |> 
    ggplot(aes(x = VRDeck, fill = Transported)) +
    geom_histogram() +
    scale_x_log10() +
    facet_wrap(. ~ Transported, scales = "free_y")
) + (
  train_data |> 
    ggplot(aes(x = VRDeck, y = Transported, fill = Transported)) +
    scale_x_log10() +
    geom_boxplot()
) + 
  plot_layout(guides = "collect")

# explore nominal columns: distribution of `CryoSleep`
# Feature engineering: dummy
train_data |> 
  ggplot(aes(x = factor(CryoSleep), fill = Transported)) +
  geom_bar(position = "fill")

# explore nominal columns: distribution of `VIP`
# Feature engineering: dummy
train_data |> 
  ggplot(aes(x = factor(VIP), fill = Transported)) +
  geom_bar(position = "fill")

# explore nominal columns: distribution of `GroupSize`
# Feature engineering: dummy
train_data |> 
  ggplot(aes(x = factor(GroupSize), fill = Transported)) +
  geom_bar(position = "fill")

# explore nominal columns: distribution of `HomePlanet`
# Feature engineering: dummy
train_data |> 
  ggplot(aes(x = factor(HomePlanet), fill = Transported)) +
  geom_bar(position = "fill")

# explore nominal columns: distribution of `CabinDeck`
# Feature engineering: dummy
train_data |> 
  ggplot(aes(x = factor(CabinDeck), fill = Transported)) +
  geom_bar(position = "fill")

# explore nominal columns: distribution of `CabinNum`
# Feature engineering: EXCLUDE!
train_data |> count(CabinNum) |> arrange(desc(n))

train_data |> 
  count(CabinNum, Transported) |> 
  mutate(TotalCabinNum = sum(n), .by = CabinNum) |> 
  mutate(CabinNum = if_else(TotalCabinNum / sum(n) >= 0.002, 
                            CabinNum, "Other")) |> 
  summarise(n = sum(n), .by = c(CabinNum, Transported)) |> 
  # filter(CabinNum != "Other") |> 
  ggplot(aes(x = factor(CabinNum), fill = Transported)) +
  geom_bar(position = "fill")

# explore nominal columns: distribution of `CabinSide`
# Feature engineering: dummy
train_data |> 
  ggplot(aes(x = factor(CabinSide), fill = Transported)) +
  geom_bar(position = "fill")

# explore nominal columns: distribution of `Destination`
# Feature engineering: dummy
train_data |> 
  ggplot(aes(x = factor(Destination), fill = Transported)) +
  geom_bar(position = "fill")



# Feature Engineering Recipes ---------------------------------------------
basic_preproc <-
  recipe(Transported ~ GroupSize + HomePlanet + CryoSleep + CabinDeck +
           CabinNum + CabinSide + Destination + Age + VIP + RoomService +
           FoodCourt + ShoppingMall + Spa + VRDeck,
         data = train_data) |> 
  step_unknown(all_nominal_predictors()) |> 
  step_impute_median(all_numeric_predictors()) |> 
  step_other(CabinNum, threshold = 0.0025) |> 
  step_dummy(all_nominal_predictors())

transform_preproc <- 
  recipe(Transported ~ GroupSize + HomePlanet + CryoSleep + CabinDeck +
           CabinSide + Destination + Age + VIP + RoomService +
           FoodCourt + ShoppingMall + Spa + VRDeck,
         data = train_data) |> 
  step_impute_median(all_numeric_predictors()) |> 
  step_impute_knn(all_nominal_predictors()) |> 
  step_normalize(Age) |> 
  step_log(RoomService, FoodCourt, ShoppingMall, Spa, VRDeck, offset = 1) |> 
  step_unknown(all_nominal_predictors()) |> 
  step_dummy(all_nominal_predictors())

pca_preproc <-
  recipe(Transported ~ GroupSize + HomePlanet + CryoSleep + CabinDeck +
           CabinNum + 
           CabinSide + Destination + Age + VIP + RoomService +
           FoodCourt + ShoppingMall + Spa + VRDeck,
         data = train_data) |> 
  step_impute_median(all_numeric_predictors()) |> 
  step_impute_knn(all_nominal_predictors()) |> 
  step_normalize(Age) |> 
  step_log(RoomService, FoodCourt, ShoppingMall, Spa, VRDeck, offset = 1) |> 
  step_other(CabinNum, threshold = 0.0015) |> 
  step_unknown(all_nominal_predictors()) |> 
  step_dummy(all_nominal_predictors()) |> 
  step_pca(all_predictors())

preproc <-
  list(
    "basic" = basic_preproc,
    "trans" = transform_preproc,
    "pca" = pca_preproc
  )



# Model Development -------------------------------------------------------

# Split data
set.seed(1234)
train_folds <- train_data |> vfold_cv(v = 10)

# Define models
# 1: Logistic Regression
logreg_spec <-
  logistic_reg(engine = "glmnet", penalty = tune(), mixture = tune()) |>
  set_mode("classification")

# 2: Decision Tree
dtree_spec <-
  decision_tree(cost_complexity = tune(),
                tree_depth = tune(),
                min_n = tune()) |>
  set_mode("classification")

# 3: XGBoost
btree_spec <- 
  boost_tree(trees = tune(), min_n = tune(), tree_depth = tune(),
             mtry = tune(), sample_size = tune(), loss_reduction = tune(),
             stop_iter = tune()) |> 
  set_mode("classification")

# 4: Random Forest
rf_spec <- 
  rand_forest(trees = tune(), min_n = tune(), mtry = tune()) |> 
  set_mode("classification")


# List all models
models <-
  list(
    "LogisticRegression" = logreg_spec,
    "DecisionTree" = dtree_spec,
    "BoostedTree" = btree_spec,
    "RandomForest" = rf_spec
  )


# Tune model's hyperparameters 
cl <- makePSOCKcluster(8)

registerDoParallel(cl)

results <-
  workflow_set(preproc = preproc, models = models, cross = TRUE) |> 
  workflow_map(resamples = train_folds, fn = "tune_grid", verbose = TRUE,
               grid = 20, seed = 1234)

stopCluster(cl)

gc()



# Make Final Predictions --------------------------------------------------

# find the model with the best hyperparameter
top_model_name <- 
  rank_results(results, rank_metric = "accuracy", select_best = TRUE) |> 
  pull(wflow_id) |> 
  _[1]

best_parameters <-
  results |>
  extract_workflow_set_result(top_model_name) |>
  select_best(metric = "accuracy")

# train with full data
final_model <-
  results |>
  extract_workflow(top_model_name) |>
  finalize_workflow(best_parameters) |>
  fit(train_data)

# make predictions
submission <-
  predict(final_model, test_data) |>
  bind_cols(select(test_data, PassengerId)) |>
  mutate(Transported = if_else(.pred_class == 1, "True", "False")) |>
  select(PassengerId, Transported)

write_csv(submission, paste0("./data/submission.csv"))
