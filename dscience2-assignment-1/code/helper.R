library(data.tree)

# https://www.h2o.ai/blog/finally-you-can-plot-h2o-decision-trees-in-r/

# visualise 
createDataTree <- function(h2oTree) {
  h2oTreeRoot = h2oTree@root_node
  dataTree = Node$new(h2oTreeRoot@split_feature)
  dataTree$type = 'split'
  addChildren(dataTree, h2oTreeRoot)
  return(dataTree)
}

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
  
  if(class(left_node)[[1]] == 'H2OLeafNode')
    leftLabel = paste("prediction:", left_node@prediction)
  else
    leftLabel = left_node@split_feature
  
  if(class(right_node)[[1]] == 'H2OLeafNode')
    rightLabel = paste("prediction:", right_node@prediction)
  else
    rightLabel = right_node@split_feature
  
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

# discord notification
# define webhook function
create_message <- function(user, model, elapsed_time) {
  text <- paste0('hello ', user, ' your ', modelname, ' has finished training! It took: ', round(elapsed_time, 2), ' ', units(elapsed_time))
  return(text)
}
send_message <- function(webhookurl = url, my_text = text) {
  library(jsonlite)
  library(rvest)
  
  headers = c(
    `Content-type` = 'application/json'
  )
  tryCatch({
    data= toJSON(list("content"= my_text), auto_unbox = T)
    res <- httr::POST(url = webhookurl, httr::add_headers(.headers=headers), body = data)
  },
  warning = function(w) {
    print('warning!')
  }, error = function(e) {
    print('error!')
  }, finally = {
  }
  )
}


# read in credentials
creds <- read.delim('/mnt/DATA/Projects/BA-20-21/archive/danalysis-3/data/creds.txt', sep = ',') # change the path to your creds.txt file
url <- as.character(creds[creds$key == 'webhookurl', 2])
user <- as.character(creds[creds$key == 'user', 2])

# text <- paste0('hello ', user, ' your model has finished training!')


getPerformanceMetrics <- function(model, newdata = NULL, xval = FALSE) {
  h2o.performance(model, newdata = newdata, xval = xval)@metrics$thresholds_and_metric_scores %>%
    as_tibble() %>%
    mutate(model = model@model_id)
}

plotROC <- function(performance_df) {
  ggplot(performance_df, aes(fpr, tpr, color = model)) +
    geom_path() +
    geom_abline(slope = 1, intercept = 0, linetype = "dashed") +
    coord_fixed() +
    labs(x = "False Positive Rate", y = "True Positive Rate")
}


save_varimp <- function(model, png_path) {
  png(png_path)
  h2o.varimp_plot(model)
  dev.off()
  
}

