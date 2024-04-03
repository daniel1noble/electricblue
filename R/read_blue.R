
#' @title read_blue 
#' @description Read an electric blue temperature logging file or directory of files
#' @param dir A character string giving the path to the directory to read
#' @param ... Additional arguments passed to \code{\link{readr::read_csv}}
#' @return A dataframe with columns \code{datetime}, \code{temperature}, and \code{unit}
#' @export
#' @examples
#' \dontrun{
#' data <- read_blue("./raw")
#' plot(data)
#' }

read_blue <- function(dir, ...) {
  
  if( (substring(dir, nchar(dir)) == "/") == FALSE){
		dir <- paste0(dir, "/")
	}
  
  # Get files
	files <- list.files(dir) 

  # Get full path
	path <- paste0(dir, "/", files)
  
  if(length(path) == 0){
	stop("No files found in directory")
  }

  if(length(path) == 1){
  # Read the file
  	    data <- readr::read_csv(path, col_names = c("datetime", "temperature"), show_col_types = FALSE, ...)

  # Get versions. Store these as attributes. These will be used if versions change file structure
	    vers <- get_versions(data)

  # Get some important information which will be kep as an attribute
	metadata <- get_metadata(data)

  # Create the dataframe
	   data2 <- build_data(data, metadata, vers)

  # Set class
	class(data2) <- c("blue", class(data2))

  } else {

	# Read the files
  	data <- lapply(path, function (x) readr::read_csv(x, col_names = c("datetime", "temperature"), show_col_types = FALSE, ...))

	# Get versions. Store these as attributes. These will be used if versions change file structure
	    vers <- lapply(data, function(x) get_versions(x))

  # Get some important information which will be kep as an attribute
	metadata <- lapply(data, function(x) get_metadata(x))

  # Create the dataframe
	   data2 <- mapply(function(x, y, z) build_data(x, y, z), x = data, y = metadata, z = vers, SIMPLIFY = FALSE)

  # Set class
		data2 <- lapply(data2, function(x) list_class(x))

  }
  
  # Return the processed data
	return(data2)
}

####----------------- Helper functions -----------------####

#' @title get_versions
#' @description Get the software and logger versions from the data
#' @param data A dataframe with columns \code{datetime} and \code{temperature}
#' @return A list with elements \code{vers_logger} and \code{vers_software}
#' @export
get_versions <- function(data) {
  	# logger version
		  vers_logger <- as.character(with(data, data[which(datetime == "EnvLogger version"), "temperature"]))
	# Software version
		vers_software <- as.numeric(with(data, data[which(datetime == "EnvLogger Viewer version"), "temperature"])) 
	return(list(vers_logger = vers_logger, vers_software = vers_software))
}

#' @title get_metadata
#' @description Get the metadata from the electric blue file
#' @param data A dataframe with columns \code{datetime} and \code{temperature}
#' @return A list with elements \code{serial}, \code{name}, \code{lat}, \code{long}, \code{unit}, \code{resolution}, and \code{tz}
#' @export
get_metadata <- function(data){
	tz <- as.character(stringr::str_extract_all(with(data, data[which(datetime == "time zone"), "temperature"]),"[A-Z]+", simplify = TRUE)) # Time zone
  unit <- as.character(with(data, data[which(datetime == "temperature"), "temperature"])) # Unit
serial <- as.character(with(data, data[which(datetime == "serial number"), "temperature"])) # Serial number
   res <- as.numeric(with(data, data[which(datetime == "sampling resolution"), "temperature"])) # Resolution
   lat <- as.numeric(with(data, data[which(datetime == "lat"), "temperature"])) # Latitude
  long <- as.numeric(with(data, data[which(datetime == "long"), "temperature"])) # Longitude
  name <- as.character(with(data, data[which(datetime == "custom name"), "temperature"])) # Name

	return(list(serial = serial, name = name, lat = lat, long = long, unit = unit,  resolution = res, tz = tz))
}

#' @title build_data
#' @description Build the data frame
#' @param data A dataframe with columns \code{datetime} and \code{temperature}
#' @param metadata A list with elements \code{serial}, \code{name}, \code{lat}, \code{long}, \code{unit}, \code{resolution}, and \code{tz}
#' @param vers A list with elements \code{vers_logger} and \code{vers_software}
#' @return A dataframe with columns \code{datetime}, \code{temperature}, and \code{unit}
#' @export
build_data <- function(data, metadata, vers){
	temps <- as.numeric(data[(which(data$temperature =="temp")+1):nrow(data),"temperature"]$temperature)
	times <- lubridate::ymd_hms((data[(which(data$temperature =="temp")+1):nrow(data),"datetime"]$datetime), tz = metadata[["tz"]])

	data <- data.frame(serial = metadata[["serial"]],
						 name = metadata[["name"]], 
						  lat = metadata[["lat"]], 
						 long = metadata[["long"]],
					 datetime = times, 
				  temperature = temps,
					     unit = metadata[["unit"]],
				   resolution = metadata[["resolution"]])
	
	attr(data, "soft_version") <- vers[["vers_software"]]
	 attr(data, "log_version") <- vers[["vers_logger"]]
	 
	return(data)
}

#' @title list_class
#' @description Set the class of the data
#' @param x A dataframe with columns \code{datetime}, \code{temperature}, and \code{unit}
#' @return A dataframe with class \code{blue} for eache element of the list
#' @export

list_class <- function(x){
	class(x) <- c("blue", class(x))
  return(x)
}