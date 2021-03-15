# scatterplot function
scatter_fun <- function(pred, df = data, save = T) {
  
  # create x-axis label
  pred_name <- gsub("data$", "", deparse(substitute(pred)), fixed = T)
  
  # create plot
  plot <- df %>% 
    ggplot(aes(pred, logTotalValue)) + 
    geom_point() + 
    geom_smooth(method = "loess") +
    labs(x = pred_name,
         y = "total value in logarithms")
  
  # print plot
  print(plot)
  # if (save == T) {
  #   # save plot
  #   ggsave(paste0("assginment-1/out/scatter_logTotalValue-", pred_name, ".png"))
  # }
  return(plot)
}


# boxplot function
box_fun <- function(pred) {
  
  # create x-axis label
  pred_name <- gsub("data$", "", deparse(substitute(pred)), fixed = T)
  
  # create plot
  plot <- data %>% 
    ggplot(aes(reorder(pred, -logTotalValue, median), logTotalValue)) + 
    geom_boxplot() + 
    labs(x = pred_name,
         y = "total value in logarithms")
  
  # print plot
  print(plot)
  
  # save plot
  # tryCatch(
  #   {ggsave(paste0("/assginment-1/out/box_logTotalValue-", pred_name, ".png"), plot)},
  #   error = function(e) {
  #     print('there was an error')
  #     png(paste0("/assginment-1/out/box_logTotalValue-", pred_name, ".png"))
  #     dev.off()
  #   }
  # )
  return(plot)
}

# one-hot-encode factor variables
one_hot <- function(dt, cols="auto", dropCols=TRUE, dropUnusedLevels=FALSE){
  # One-Hot-Encode unordered factors in a data.table
  # If cols = "auto", each unordered factor column in dt will be encoded. (Or specifcy a vector of column names to encode)
  # If dropCols=TRUE, the original factor columns are dropped
  # If dropUnusedLevels = TRUE, unused factor levels are dropped
  
  # Automatically get the unordered factor columns
  if(cols[1] == "auto") cols <- colnames(dt)[which(sapply(dt, function(x) is.factor(x) & !is.ordered(x)))]
  
  # Build tempDT containing and ID column and 'cols' columns
  tempDT <- dt[, cols, with=FALSE]
  tempDT[, ID := .I]
  setcolorder(tempDT, unique(c("ID", colnames(tempDT))))
  for(col in cols) set(tempDT, j=col, value=factor(paste(col, tempDT[[col]], sep="_"), levels=paste(col, levels(tempDT[[col]]), sep="_")))
  
  # One-hot-encode
  if(dropUnusedLevels == TRUE){
    newCols <- dcast(melt(tempDT, id = 'ID', value.factor = T), ID ~ value, drop = T, fun = length)
  } else{
    newCols <- dcast(melt(tempDT, id = 'ID', value.factor = T), ID ~ value, drop = F, fun = length)
  }
  
  # Combine binarized columns with the original dataset
  result <- cbind(dt, newCols[, !"ID"])
  
  # If dropCols = TRUE, remove the original factor columns
  if(dropCols == TRUE){
    result <- result[, !cols, with=FALSE]
  }
  
  return(result)
}
