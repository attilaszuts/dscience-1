# 3. Stacking
# rm(list=ls())
library(tidyverse)
library(skimr)
library(h2o)
library(janitor)
library(ggthemes)
library(data.tree)
library(DiagrammeR)
library(pROC)

source("code/helper.R")


# read data
data <- read_csv("data/KaggleV2-May-2016.csv")

# some data cleaning
data <- select(data, -one_of(c("PatientId", "AppointmentID", "Neighbourhood"))) %>%
  janitor::clean_names()

# for binary prediction, the target variable must be a factor + generate new variables
data <- mutate(
  data,
  no_show = factor(no_show, levels = c("Yes", "No")),
  handcap = ifelse(handcap > 0, 1, 0),
  across(c(gender, scholarship, hipertension, alcoholism, handcap), factor),
  hours_since_scheduled = as.numeric(appointment_day - scheduled_day)
)

# clean up a little bit
data <- filter(data, between(age, 0, 95), hours_since_scheduled >= 0) %>%
  select(-one_of(c("scheduled_day", "appointment_day", "sms_received")))

response <- "no_show"
predictors <- setdiff(names(data), response)

# init h2o ----------------------------------------------------------------

# init h2o cluster
h2o.init()
seed = 20210309

data_h2o <- as.h2o(data)

splitted_data <- h2o.splitFrame(data_h2o, ratios = c(0.5, 0.45), seed = seed)
data_train <- splitted_data[[1]]
data_valid <- splitted_data[[2]]
data_test <- splitted_data[[3]]



# benchmark ---------------------------------------------------------------

start_time <- Sys.time()
benchmark_params <- list(
  learn_rate = c(0.1, 0.3),  # default: 0.1
  ntrees = c(300),
  max_depth = c(5),
  sample_rate = c(0.2, 0.5)
)

start_time <- Sys.time()
benchmark_grid = h2o.grid("gbm", x = predictors, y = response, # model estimation
                    grid_id = "gbm_benchmark",
                    training_frame = data_train,
                    nfolds = 5, 
                    seed = seed,
                    hyper_params = benchmark_params)
end_time <- Sys.time()

# send notification
elapsed_time <- end_time - start_time
modelname <- '**gbm_benchmark**'
text <- create_message(user, modelname, elapsed_time)
send_message()

benchmark_gridperf = h2o.getGrid(grid_id = "gbm_benchmark", # get grid performance
                           sort_by = "auc",
                           decreasing = TRUE)

benchmark_gbm <- h2o.getModel(h2o.getGrid(benchmark_grid@grid_id, "auc")@model_ids[[1]])

benchmark_auc <- h2o.auc(h2o.performance(benchmark_gbm, data_valid))

# train base learners -----------------------------------------------------------

start_time <- Sys.time()
rf_model <- h2o.randomForest(
  x = predictors, y = response,
  training_frame = data_train,
  model_id = "rf",
  ntrees = 200,
  max_depth = 10,
  seed = seed,
  nfolds = 5,
  keep_cross_validation_predictions = TRUE
)

deeplearning_model <- h2o.deeplearning(
  x = predictors, y = response,
  training_frame = data_train,
  model_id = "deeplearning",
  hidden = c(32, 8),
  seed = seed,
  nfolds = 5,
  keep_cross_validation_predictions = TRUE
)

glm_model <- h2o.glm(
  x = predictors, y = response,
  training_frame = data_train,
  model_id = "lasso",
  family = "binomial",
  alpha = 1,
  lambda_search = TRUE,
  seed = seed,
  nfolds = 5,
  keep_cross_validation_predictions = TRUE  # this is necessary to perform later stacking
)
end_time <- Sys.time()

# send notification
elapsed_time <- end_time - start_time
modelname <- '**base learners**'
text <- create_message(user, modelname, elapsed_time)
send_message()

######### Performance comparison
my_models <- list(glm_model, rf_model, deeplearning_model)

base_learners <- c("glm", "rf", "deeplearning")
aucs <- c()
i <- 0

for (model in my_models) {
  i <- i + 1
  aucs <- c(aucs,h2o.auc(h2o.performance(model)))
}

all_performance <- map_df(my_models, getPerformanceMetrics, xval = TRUE)
plotROC(all_performance)

h2o.model_correlation_heatmap(my_models, data_valid)


# train ensemble ----------------------------------------------------------

start_time <- Sys.time()
ensemble_model <- h2o.stackedEnsemble(
  x = predictors, y = response,
  training_frame = data_train,
  base_models = my_models,
  keep_levelone_frame = TRUE
)
end_time <- Sys.time()

# send notification
elapsed_time <- end_time - start_time
modelname <- '**ensemble model**'
text <- create_message(user, modelname, elapsed_time)
send_message()

ensemble_model
ensemble_model@model$metalearner_model@model$coefficients_table
str(ensemble_model@model$metalearner_model@model$coefficients_table)

ensemble_auc <- h2o.auc(h2o.performance(ensemble_model))

ensemble_performance <- getPerformanceMetrics(ensemble_model, data_valid, xval = T) %>% 
  mutate(model = "ensemble_model")
plotROC(ensemble_performance)

plotROC(bind_rows(all_performance, ensemble_performance))


# evaluate on test set ----------------------------------------------------

ensemble_test_auc <- h2o.auc(h2o.performance(ensemble_model, data_test))

ensemble_performance <- getPerformanceMetrics(ensemble_model, newdata = data_test)
gbm_performances <- map_df(benchmark_grid@model_ids, ~{
  getPerformanceMetrics(h2o.getModel(.), newdata = data_test)
})

ensemble_test <- ggplot(gbm_performances, aes(fpr, tpr, group = model)) +
  geom_path(alpha = 0.2) +
  geom_path(color = "firebrick", data = ensemble_performance) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed") +
  coord_fixed() +
  labs(x = "False Positive Rate", y = "True Positive Rate") + 
  theme_bw()

# shutdown ----------------------------------------------------------------

writeLines(as.character(round(benchmark_auc, 4)), "out/ex3_benchmark_auc.txt")

base_learners_df <- data.frame(base_learner = base_learners, auc = round(aucs, 4))
write_csv(base_learners_df, "out/ex3_base_learners_auc.csv")

# save base learners ROC
base_learners_roc <- plotROC(all_performance) + 
  theme_bw() + 
  theme(legend.position = "bottom")
ggsave("out/ex3_base_learners_roc.png")

# save model correlation heatmap
png("out/ex3_model_correlation_heatmap.png")
h2o.model_correlation_heatmap(my_models, data_valid)
dev.off()


#evaluate ensemble
writeLines(as.character(round(ensemble_auc, 4)), "out/ex3_ensemble_auc.txt")

ensemble_roc <- plotROC(bind_rows(all_performance, ensemble_performance)) + 
  theme_bw() + 
  theme(legend.position = "bottom")

ggsave("out/ex3_ensemble_roc.png")

# best model on test

writeLines(as.character(round(ensemble_test_auc, 4)), "out/ex3_ensemble_auc_test.txt")

ggsave("out/ex3_ensemble_test_roc.png", ensemble_test)

h2o.shutdown()
