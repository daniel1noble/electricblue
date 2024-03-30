
#' @title read_blue 
#' @description Read an electric blue temperature logging file or directory of files
#' @param path A character string giving the path to the file or directory to read
#' @param ... Additional arguments passed to \code{\link{readr::read_csv}}
#' @return A dataframe with columns \code{datetime}, \code{temperature}, and \code{unit}
#' @export
#' @examples
#' \dontrun{
#' data <- read_blue("./raw/04BC 0000 1C02 10-20240330 125652.csv")
#' }

read_blue <- function(path, ...) {
  # Read the file
  	 data <- readr::read_csv(path, col_names = c("datetime", "temperature"), ...)

# Get some important information
	tz <- with(data, data[which(datetime == "time zone"), "temperature"])

# Return the data
   		
	temps <- as.numeric(data[(which(data$temperature =="temp")+1):nrow(data),"temperature"])
	times <- lubridate::ymd_hms((data[(which(data$temperature =="temp")+1):nrow(data),"datetime"]), tz = tz)
}