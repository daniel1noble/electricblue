
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

# Get version
		vers_logger <- as.numeric(with(data, data[which(datetime == "EnvLogger version"), "temperature"])) # logger version
		vers_software <- as.numeric(with(data, data[which(datetime == "EnvLogger Viewer version"), "temperature"])) # logger version

# Get some important information which will be kep as an attribute
	    tz <- with(data, data[which(datetime == "time zone"), "temperature"]) # Time zone
	  unit <- with(data, data[which(datetime == "temperature"), "temperature"]) # Unit
	serial <- with(data, data[which(datetime == "serial number"), "temperature"]) # Serial number
	   res <- as.numeric(with(data, data[which(datetime == "sampling resolution"), "temperature"])) # Resolution
	lat <- as.numeric(with(data, data[which(datetime == "lat"), "temperature"])) # Latitude
	long <- as.numeric(with(data, data[which(datetime == "long"), "temperature"])) # Longitude

# Return the data
   		
	temps <- as.numeric(data[(which(data$temperature =="temp")+1):nrow(data),"temperature"])
	times <- lubridate::ymd_hms((data[(which(data$temperature =="temp")+1):nrow(data),"datetime"]), tz = tz)
}