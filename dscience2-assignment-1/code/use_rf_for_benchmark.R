library(rattle)
library(tidyverse)
library(skimr)
library(h2o)
library(janitor)
library(ggthemes)
library(data.tree)
library(DiagrammeR)
library(data.tree)
library(caret)
library(rpart)

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



# train-test split
splitted_data <- h2o.splitFrame(h20_data, ratios = 0.75, seed = seed)
data_train <- splitted_data[[1]]
data_test <- splitted_data[[2]]


oj_1tree = 
  h2o.randomForest(x = predictors, y = response, 
          training_frame = data_train, 
          ntrees = 1, min_rows = 1, 
          max_depth = 5,
          mtries = 1,
          # use early stopping once the validation AUC doesn't improve 
          # by at least 0.01% for 5 consecutive scoring events
          stopping_rounds = 3, stopping_tolerance = 0.01, 
          stopping_metric = "AUC", 
          seed = seed)

# get the tree model object
ojH2oTree = h2o.getModelTree(model = oj_1tree, tree_number = 1)



## configure plot options
ojDT = createDataTree(ojH2oTree)

# h2oTree = ojH2oTree

# plot functions
createDataTree <- function(h2oTree) {
  #
  h2oTreeRoot = h2oTree@root_node
  dataTree = Node$new(h2oTreeRoot@split_feature)
  dataTree$type = 'split'
  # 
  addChildren(dataTree, h2oTreeRoot)
  return(dataTree)
}

# # 1st split
# dtree = dataTree
# node = h2oTreeRoot
# 
# # 2nd split
# dtree = dtreeLeft
# node = left_node
# 
# # 3rd split
# dtree = dtreeLeft
# node = left_node
# 
# # 4th split
# dtree = dtreeLeft
# node = left_node

addChildren <- function(dtree, node) {
  
  if(class(node)[1] != 'H2OSplitNode') return(TRUE)
  
  feature = node@split_feature
  id = node@id
  na_direction = node@na_direction
  
  if(is.na(node@threshold)) {
    leftEdgeLabel = printValues(node@left_levels, 
                                na_direction=='LEFT', 4)
    rightEdgeLabel = printValues(node@right_levels, 
                                 na_direction=='RIGHT', 4)
  }else {
    leftEdgeLabel = paste("<", node@threshold, 
                          ifelse(na_direction=='LEFT',',NA',''))
    rightEdgeLabel = paste(">=", node@threshold, 
                           ifelse(na_direction=='RIGHT',',NA',''))
  }
  
  left_node = node@left_child
  right_node = node@right_child
  
  if(class(left_node)[[1]] == 'H2OLeafNode'){
    leftLabel = paste("prediction:", left_node@prediction)
  }else{
    leftLabel = left_node@split_feature
  }
  
  if(class(right_node)[[1]] == 'H2OLeafNode'){
    rightLabel = paste("prediction:", right_node@prediction)
  }else{
    rightLabel = right_node@split_feature
  }
  
  if(leftLabel == rightLabel) {
    leftLabel = paste(leftLabel, "(L)")
    rightLabel = paste(rightLabel, "(R)")
  }
  
  dtreeLeft = dtree$AddChild(leftLabel)
  dtreeLeft$edgeLabel = leftEdgeLabel
  dtreeLeft$type = ifelse(class(left_node)[1] == 'H2OSplitNode', 'split', 'leaf')
  
  dtreeRight = dtree$AddChild(rightLabel)
  dtreeRight$edgeLabel = rightEdgeLabel
  dtreeRight$type = ifelse(class(right_node)[1] == 'H2OSplitNode', 'split', 'leaf')
  
  addChildren(dtreeLeft, left_node)
  addChildren(dtreeRight, right_node)
  
  return(FALSE)
}

printValues <- function(values, is_na_direction, n=4) {
  l = length(values)
  if(l == 0)
    value_string = ifelse(is_na_direction, "NA", "")
  else
    value_string = paste0(paste0(values[1:min(n,l)], collapse = ', '),
                          ifelse(l > n, ",...", ""),
                          ifelse(is_na_direction, ", NA", ""))
  return(value_string)
}

# customise plot

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

ex1_dtree_rf <- plot(ojDT, output = "graph") # show plot


