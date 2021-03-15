# 1. Tree Ensemble methods
rm(list=ls())
start_time <- Sys.time()
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
data <- as_tibble(ISLR::OJ) %>% clean_names()
skim(data)
h20_data <- as.h2o(data)

# feature engineering
response = "purchase"
predictors = c("store7", "weekof_purchase", "price_mm", "price_ch", "disc_mm", "disc_ch",
               "special_ch", "special_mm", "loyal_ch", "list_price_diff", "store")
# ------------------------------------------------------
##################  filter variables  ##################
# ------------------------------------------------------



# train-test split
splitted_data <- h2o.splitFrame(h20_data, ratios = 0.75, seed = seed)
data_train <- splitted_data[[1]]
data_test <- splitted_data[[2]]

# 
# Decision tree -----------------------------------------------------------


dtree_params = list(max_depth = seq(2, 10)) # tree depth parameter search

dtree_grid = h2o.grid("gbm", x = predictors, y = response, # model estimation
                    grid_id = "gbm_grid_1tree",
                    training_frame = data_train,
                    nfolds = 5,
                    ntrees = 1, min_rows = 1, 
                    sample_rate = 1, col_sample_rate = 1,
                    learn_rate = .01, seed = seed,
                    hyper_params = dtree_params)

dtree_gridperf = h2o.getGrid(grid_id = "gbm_grid_1tree", # get grid performance
                           sort_by = "auc",
                           decreasing = TRUE)

best_tree <- h2o.getModel(h2o.getGrid(dtree_grid@grid_id, "auc")@model_ids[[1]])

h2o.auc(h2o.performance(best_tree))


# visualise output
ggplot(as.data.frame(sapply(dtree_gridperf@summary_table, as.numeric))) + # plot performance
  geom_point(aes(max_depth, auc)) +
  geom_line(aes(max_depth, auc, group=1)) +
  labs(x="max depth", y="AUC", title="Grid Search for Single Tree Models") +
  theme_pander(base_family = 'Palatino', base_size = 12)
# bset tree is max_depth = 5, retrain model with this parameter
oj_1tree = 
  h2o.gbm(x = predictors, y = response, 
          training_frame = data_train, 
          ntrees = 1, min_rows = 1, 
          sample_rate = 1, col_sample_rate = 1,
          max_depth = 5,
          # use early stopping once the validation AUC doesn't improve 
          # by at least 0.01% for 5 consecutive scoring events
          stopping_rounds = 3, stopping_tolerance = 0.01, 
          stopping_metric = "AUC", 
          seed = seed)

# get the tree model object
ojH2oTree = h2o.getModelTree(model = oj_1tree, tree_number = 1)



## configure plot options
ojDT = createDataTree(ojH2oTree)
GetEdgeLabel <- function(node) {return (node$edgeLabel)}
GetNodeShape <- function(node) {switch(node$type, 
                                       split = "diamond", leaf = "oval")}
GetFontName <- function(node) {switch(node$type, 
                                      split = 'Palatino-bold', 
                                      leaf = 'Palatino')}
SetEdgeStyle(ojDT, fontname = 'Palatino-italic', 
             label = GetEdgeLabel, labelfloat = TRUE,
             fontsize = "26", fontcolor='royalblue4')
SetNodeStyle(ojDT, fontname = GetFontName, shape = GetNodeShape, 
             fontsize = "26", fontcolor='royalblue4',
             height="0.75", width="1")
SetGraphStyle(ojDT, rankdir = "LR", dpi=70.)
plot(ojDT, output = "graph") # show plot

h2o.auc(h2o.performance(dtree_grid))
# create tree based ensemble models
end_time <- Sys.time()
elapsed_time <- end_time - start_time
text <- paste(user, 'your models training is about to begin! So far the script ran for', elapsed_time, units(elapsed_time) , sep = " ")
send_message()

# gbm ---------------------------------------------------------------------


gbm_params <- list(
  learn_rate = c(0.01, 0.05, 0.1, 0.3),  # default: 0.1
  ntrees = c(10, 50, 100, 300),
  max_depth = c(2, 5),
  sample_rate = c(0.2, 0.5, 0.8, 1)
)

start_time <- Sys.time()
gbm_grid = h2o.grid("gbm", x = predictors, y = response, # model estimation
                      grid_id = "gbm",
                      training_frame = data_train,
                      nfolds = 5, 
                      seed = seed,
                      hyper_params = gbm_params)
end_time <- Sys.time()

# send notification
elapsed_time <- end_time - start_time
modelname <- '**gbm**'
text <- create_message(user, modelname, elapsed_time)
send_message()

gbm_gridperf = h2o.getGrid(grid_id = "gbm", # get grid performance
                             sort_by = "auc",
                             decreasing = TRUE)

best_gbm <- h2o.getModel(h2o.getGrid(gbm_grid@grid_id, "auc")@model_ids[[1]])
# h2o.auc(h2o.performance(best_gbm))
# h2o.auc(h2o.performance(best_gbm, data_test))
# random forest -----------------------------------------------------------


rf_params <- list(
  ntrees = c(10, 50, 100, 300),
  mtries = c(2, 4, 6, 8),
  sample_rate = c(0.2, 0.632, 1),
  max_depth = c(10, 20)
)

start_time <- Sys.time()
rf_grid = h2o.grid("randomForest", x = predictors, y = response, # model estimation
                      grid_id = "rf",
                      training_frame = data_train,
                      nfolds = 5,
                      seed = seed,
                      hyper_params = rf_params)
end_time <- Sys.time()


# send notification
elapsed_time <- round(end_time - start_time, 2)
modelname <- '**rf**'
text <- create_message(user, modelname, elapsed_time)
send_message()

rf_gridperf = h2o.getGrid(grid_id = "rf", # get grid performance
                             sort_by = "auc",
                             decreasing = TRUE)

best_rf <- h2o.getModel(h2o.getGrid(rf_grid@grid_id, "auc")@model_ids[[1]])

# XGB ---------------------------------------------------------------------


xgb_params <- list(
  learn_rate = c(0.1, 0.3),  # same as "eta", default: 0.3
  ntrees = c(50, 100, 300),
  max_depth = c(2, 5),
  gamma = c(0, 1, 2),  # regularization parameter, default: 0, same as 'mean split improvement' -> how much improvement we'd like to see with an additional cut
  sample_rate = c(0.5, 1)
)

start_time <- Sys.time()
xgb_grid = h2o.grid("xgboost", x = predictors, y = response, # model estimation
                   grid_id = "xgb",
                   training_frame = data_train,
                   nfolds = 5,
                   seed = seed,
                   hyper_params = xgb_params)
end_time <- Sys.time()


# send notification
elapsed_time <- end_time - start_time
modelname <- '**xgb**'
text <- create_message(user, modelname, elapsed_time)
send_message()

xgb_gridperf = h2o.getGrid(grid_id = "xgb", # get grid performance
                          sort_by = "auc",
                          decreasing = TRUE)

best_xgb <- h2o.getModel(h2o.getGrid(xgb_grid@grid_id, "auc")@model_ids[[1]])

# collect baseline and best models ----------------------------------------

best_models <- list(dtree_gridperf, gbm_gridperf, rf_gridperf, xgb_gridperf)
aucs <- c()
models <- c("dtree", "gbm", "rf", "xgb")
ind <- 0
for (model in best_models) {
  ind <- ind + 1
  aucs <- c(aucs, model@summary_table[-order("auc")][1, c("auc")])
}
aucs_df <- data.frame(models = models, auc = round(as.numeric(aucs), 4))

best_gbm <- h2o.getModel(h2o.getGrid(gbm_grid@grid_id, "auc")@model_ids[[1]])
gbm_test_auc <- round(h2o.auc(h2o.performance(best_gbm, data_test)), 4)

plot(h2o.performance(best_gbm, xval = TRUE), type = "roc")


# variable importance plots -----------------------------------------------

h2o.varimp_plot(best_tree)
h2o.varimp_plot(best_gbm)
h2o.varimp_plot(best_rf)
h2o.varimp_plot(best_xgb)

# shutdown ----------------------------------------------------------------

# png("out/ex1_dtree.png")
# plot(ojDT)
# dev.off()

# save CV performance comparison table
write_csv(aucs_df, "out/ex1_cv_auc.csv")

# save best model results
writeLines(as.character(gbm_test_auc), "out/ex1_gbm_test_auc.txt")

# save best model ROC
png("out/ex1_gbm_test_roc.png")
plot(h2o.performance(best_gbm, xval = TRUE), type = "roc")
dev.off()


models <- list(best_gbm, best_rf, best_xgb)
paths <- c("out/ex1_gbm_varimp.png", "out/ex1_rf_varimp.png", "out/ex1_xgb_varimp.png")
i <- 0

for (model in models) {
  i <- i + 1
  save_varimp(model, paths[i])
}

# shutdown cluster
h2o.shutdown()
