# 2. Variable importance profiles
# rm(list=ls())
library(tidyverse)
library(skimr)
library(h2o)
library(janitor)
library(ggthemes)
library(data.tree)
library(DiagrammeR)

source("code/helper.R")

# init h2o cluster
h2o.init()
seed = 20210309

# read in data
data <- as_tibble(ISLR::Hitters) %>%
  drop_na(Salary) %>%
  mutate(log_salary = log(Salary), Salary = NULL)

data <- as.h2o(data)
response <- "log_salary"
predictors <- setdiff(names(data), "log_salary")

# rf1 ---------------------------------------------------------------------

rf1_params <- list(
  mtries = 2
)

start_time <- Sys.time()
rf1_grid = h2o.grid("randomForest", x = predictors, y = response, # model estimation
                   grid_id = "rf1",
                   training_frame = data,
                   seed = seed,
                   hyper_params = rf1_params)
end_time <- Sys.time()


# send notification
elapsed_time <- round(end_time - start_time, 2)
modelname <- '**rf1**'
text <- create_message(user, modelname, elapsed_time)
send_message()

rf1_model <- h2o.getModel(h2o.getGrid(rf1_grid@grid_id, "rmse")@model_ids[[1]])

ex2_rf1 <- h2o.varimp_plot(rf1_model)


# rf2 ---------------------------------------------------------------------

rf2_params <- list(
  mtries = 10
)

start_time <- Sys.time()
rf2_grid = h2o.grid("randomForest", x = predictors, y = response, # model estimation
                    grid_id = "rf2",
                    training_frame = data,
                    seed = seed,
                    hyper_params = rf2_params)
end_time <- Sys.time()


# send notification
elapsed_time <- round(end_time - start_time, 2)
modelname <- '**rf2**'
text <- create_message(user, modelname, elapsed_time)
send_message()

rf2_model <- h2o.getModel(h2o.getGrid(rf2_grid@grid_id, "rmse")@model_ids[[1]])

ex2_rf2 <- h2o.varimp_plot(rf2_model)

# When we can use more variables at each split, few variables dominate the splits, compared to when we limit the choice.
# This can cause our models to be more correlated with each other.



# gbm1 --------------------------------------------------------------------

gbm1_params <- list(
  learn_rate = 0.1,  # default: 0.1
  ntrees = 500,
  max_depth = 5,
  sample_rate = 0.1
)

start_time <- Sys.time()
gbm1_grid = h2o.grid("gbm", x = predictors, y = response, # model estimation
                    grid_id = "gbm1",
                    training_frame = data,
                    seed = seed,
                    hyper_params = gbm1_params)
end_time <- Sys.time()

# send notification
elapsed_time <- end_time - start_time
modelname <- '**gbm1**'
text <- create_message(user, modelname, elapsed_time)
send_message()

gbm1_gridperf = h2o.getGrid(grid_id = "gbm1", # get grid performance
                           sort_by = "rmse",
                           decreasing = TRUE)

gbm1_model <- h2o.getModel(h2o.getGrid(gbm1_grid@grid_id, "rmse")@model_ids[[1]])

ex2_gbm1 <- h2o.varimp_plot(gbm1_model)

# gbm2 --------------------------------------------------------------------

gbm2_params <- list(
  learn_rate = 0.1,  # default: 0.1
  ntrees = 500,
  max_depth = 5,
  sample_rate = 1
)

start_time <- Sys.time()
gbm2_grid = h2o.grid("gbm", x = predictors, y = response, # model estimation
                    grid_id = "gbm2",
                    training_frame = data,
                    seed = seed,
                    hyper_params = gbm2_params)
end_time <- Sys.time()

# send notification
elapsed_time <- end_time - start_time
modelname <- '**gbm2**'
text <- create_message(user, modelname, elapsed_time)
send_message()

gbm2_gridperf = h2o.getGrid(grid_id = "gbm2", # get grid performance
                            sort_by = "rmse",
                            decreasing = TRUE)

gbm2_model <- h2o.getModel(h2o.getGrid(gbm2_grid@grid_id, "rmse")@model_ids[[1]])

ex2_gbm2 <- h2o.varimp_plot(gbm2_model)



# shutdown ----------------------------------------------------------------

save_varimp(rf1_model, "out/ex2_rf1.png")
save_varimp(rf2_model, "out/ex2_rf2.png")
save_varimp(gbm1_model, "out/ex2_gbm1.png")
save_varimp(gbm2_model, "out/ex2_gbm2.png")

# shutdown cluster
# h2o.shutdown()
