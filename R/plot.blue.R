
#' @title plot.blue 
#' @description A function to plot temperature data in blue from teh raw data
#' @param data A data frame or list of dataframes containing the temperature data
#' @param rows The number of rows to use in the plot
#' @param ... Additional arguments to be passed to the plot function
#' @return A plot of the temperature data over time
#' @export

plot.blue  <- function(data, rows = 1, ...) {

  if(is.list(data)){
    par(mfrow = c(rows, length(data)))
    for(i in 1:length(data)){
      plot(temperature ~ datetime, data[[i]], type = "l", xlab = "Time and Date", ylab = "Temperature", main = unique(data[[i]]$name), ...)
    }
  } else {  
    plot(temperature ~ datetime, data, type = "l", xlab = "Time and Date", ylab = "Temperature", main = unique(data$name), ...)
  }
}