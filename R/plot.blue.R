
#' @title plot.blue 
#' @description A function to plot temperature data in blue from teh raw data
#' @param data A data frame containing the temperature data
#' @param ... Additional arguments to be passed to the plot function
#' @return A plot of the temperature data over time
#' @export

plot.blue  <- function(data, ...) {

  if(is.list(data)){
    for(i in 1:length(data)){
      mfrow <- c(1, length(data))
      plot(temperature ~ datetime, data[[i]], xlab = "Time and Date", ylab = "Temperature", main = unique(data[[i]]$name), ...)
    }
  } else {  
    plot(temperature ~ datetime, data, type = "l", xlab = "Time and Date", ylab = "Temperature", main = unique(data$name), ...)
  }
}