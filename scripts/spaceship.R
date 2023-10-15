library(tidyverse)
library(tidymodels)
library(doParallel)

path_to_data <- "./Classification--Spaceship/data/"


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
clean_spaceship <- 
  function(spaceship_data) {
    spaceship_data |> 
      separate(Cabin, into = paste0("Cabin", c("Deck", "Num", "Side"))) |> 
      mutate(TravelGroup = str_extract(PassengerId, "^[0-9]*")) |> 
      mutate(GroupSize = n(), .by = TravelGroup) |> 
      mutate(across(c(HomePlanet, starts_with("Cabin"), Destination,
                      Transported, GroupSize), factor)) |> 
      select(PassengerId, GroupSize, everything()) |> 
      select(-TravelGroup)
  }

train_data <- clean_spaceship(train_data)
test_data <- clean_spaceship(test_data)
glimpse(train_data)



# EDA and Feature Engineering ---------------------------------------------

# explore `NAs`
(map_df(train_data, ~ sum(is.na(.))) |> unlist()) / nrow(train_data)
(map_df(test_data, ~ sum(is.na(.))) |> unlist()) / nrow(test_data)

summary(train_data)
glimpse(train_data)
map(train_data |> select_if(is.numeric), function(x) unique(x) |> length())


# Feature Engineering
preproc <- 
  recipe(Transported ~ GroupSize + HomePlanet + CryoSleep + CabinDeck +
           CabinNum + CabinSide + Destination + Age + VIP + RoomService +
           FoodCourt + ShoppingMall + Spa +
           VRDeck,
         data = train_data) |> 
  step_impute_knn(all_factor()) |>
  step_dummy(all_factor())
  step_impute_mean(all_numeric_predictors()) |>
  step_other(CabinNum, threshold = 0.0025) |> 


# Model Development -------------------------------------------------------

# Setup
set.seed(1234)
train_folds <- vfold_cv(train_data, v = 5)



# Model 1: Logistic Regression
logistic_spec <-
  logistic_reg() |>
  set_engine("glm") |>
  set_mode("classification")

logistic_wf <- 
  workflow() |> 
  add_model(logistic_spec) |>
  add_recipe(preproc)

logistic_res <- fit_resamples(logistic_wf, resamples = train_folds)

# Model 2: Decision Tree
tree_spec <-
  decision_tree() |>
  set_engine("rpart") |>
  set_mode("classification")

tree_wf <- 
  workflow() |> 
  add_model(tree_spec) |> 
  add_recipe(preproc)

tree_res <- fit_resamples(tree_wf, resamples = train_folds)

# Model 3: KNN
knn_spec <-
  nearest_neighbor() |>
  set_engine("kknn") |>
  set_mode("classification")

knn_wf <- 
  workflow() |> 
  add_model(knn_spec) |> 
  add_recipe(preproc)

knn_res <- fit_resamples(knn_wf, resamples = train_folds)

# Model 4: XGBoost
btree_spec <- 
  boost_tree(trees = 15) |> 
  set_mode("classification")

btree_wf <-
  workflow() |> 
  add_model(btree_spec) |> 
  add_recipe(preproc)

btree_res <- fit_resamples(btree_wf, resamples = train_folds)

