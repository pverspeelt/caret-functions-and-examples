# Caret's summary functions are located in aaa.R


# based on https://en.wikipedia.org/wiki/Precision_and_recall 
# recall == sensitivity
# F1 = 2 * precision * sensitivity / (precision + sensitivity) 
# F1 = 2 * TP / (2 * TP + FP + FN)

F1Summary <- function (data, lev = NULL, model = NULL) {
  if (length(levels(data$obs)) > 2) 
    stop(paste("Your outcome has", length(levels(data$obs)), 
               "levels. The F1Summary() function isn't appropriate."))
  
  score <- as.vector(table(data[, "obs"], data[, "pred"]))
  names(score) <- c("TP", "FN", "FP", "TN")
  out <- 2 * score["TP"] / (2*score["TP"] + score["FP"] + score["FN"]) 
  names(out) <- c("F1")
  out
}


# mean absolute error from the Metrics package
maeSummary <- function (data, lev = NULL, model = NULL) {
  out <- Metrics::mae(data$obs, data$pred)
  names(out) <- "MAE"
  out
}
