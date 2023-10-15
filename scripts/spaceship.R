library(tidyverse)
library(tidymodels)
library(patchwork)
library(doParallel)

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


# Feature engineering recipes ---------------------------------------------
basic_preproc <-
  recipe(Transported ~ GroupSize + HomePlanet + CryoSleep + CabinDeck +
           CabinNum + CabinSide + Destination + Age + VIP + RoomService +
           FoodCourt + ShoppingMall + Spa + VRDeck,
         data = train_data) |> 
  step_unknown(all_nominal_predictors()) |> 
  step_impute_mean(all_numeric_predictors()) |> 
  step_other(CabinNum, threshold = 0.0025) |> 
  step_dummy(all_nominal_predictors())

transform_preproc <- 
  recipe(Transported ~ GroupSize + HomePlanet + CryoSleep + CabinDeck +
           CabinSide + Destination + Age + VIP + RoomService +
           FoodCourt + ShoppingMall + Spa + VRDeck,
         data = train_data) |> 
  step_normalize(Age) |> 
  step_log(RoomService, FoodCourt, ShoppingMall, Spa, VRDeck) |> 
  step_impute_linear(all_numeric_predictors()) |> 
  step_cut(all_numeric_predictors(), breaks = 10) |>
  step_impute_knn(all_nominal_predictors()) |> 
  step_unknown(all_nominal_predictors()) |> 
  step_dummy(all_nominal_predictors())

pca_preproc <-
  recipe(Transported ~ GroupSize + HomePlanet + CryoSleep + CabinDeck +
           CabinNum + 
           CabinSide + Destination + Age + VIP + RoomService +
           FoodCourt + ShoppingMall + Spa + VRDeck,
         data = train_data) |> 
  step_normalize(Age) |> 
  step_log(RoomService, FoodCourt, ShoppingMall, Spa, VRDeck) |> 
  step_impute_mean(all_numeric_predictors()) |> 
  step_cut(all_numeric_predictors(), breaks = 10) |>
  step_impute_knn(all_nominal_predictors()) |> 
  step_other(CabinNum, threshold = 0.001) |> 
  step_unknown(all_nominal_predictors()) |> 
  step_dummy(all_nominal_predictors()) |> 
  step_pca(all_predictors())
  
preproc <- list("pca" = pca_preproc)
  # list("basic" = basic_preproc,
  #      "transform" = transform_preproc,
  #      "pca" = pca_preproc)



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
  boost_tree(trees = tune(), min_n = tune(), tree_depth = tune()) |> 
  set_mode("classification")

# 4: Random Forest
rf_spec <- 
  rand_forest(trees = tune(), min_n = tune()) |> 
  set_mode("classification")

# 5: SVM
svm_spec <-
  svm_linear(engine = "kernlab", cost = tune(), margin = tune()) |> 
  set_mode("classification")

# List all models
models <-
  list(
    "LogisticRegression" = logreg_spec,
    "DecisionTree" = dtree_spec,
    # "BoostedTree" = btree_spec,
    # "RandomForest" = rf_spec,
    "SVM" = svm_spec
  )

# Tune model's hyperparameters 
results <-
  workflow_set(preproc = preproc, models = models, cross = TRUE) |> 
  workflow_map(resamples = train_folds, fn = "tune_grid", verbose = TRUE,
               grid = 2, seed = 1234)

top_model <- rank_results(results, rank_metric = "roc_auc")[1, 1] |> pull()


best_parameters <-
  results |>
  extract_workflow_set_result(top_model) |>
  select_best(metric = "roc_auc")

best_model_workflow <-
  results |> 
  extract_workflow(top_model) |> 
  finalize_workflow(best_parameters)

best_model <- fit(best_model_workflow, train_data)

predict(best_model, test_data)
