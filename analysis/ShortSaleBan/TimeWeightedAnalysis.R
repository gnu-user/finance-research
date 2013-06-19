library(stringr)
library(xts)
library(data.table)

# A function that gets the actual date of the file from the filename
# The argument is the name of the file
date_from_name <- function(filename)
{
  # Mapping of month names to numeric value
  date_num <- c("01","02","03","04","05","06","07","08", "09", "10", "11","12")
  names(date_num) <- c("jan","feb","mar","apr","may","jun","jul","aug","sep", "oct","nov","dec")
  
  # Extract the date from filename
  regexp <- "_([0-9]{2})([a-z]{3})([0-9]{4})"
  match <- str_match(file, regexp)
  
  return(paste(match[4], date_num[match[3]], match[2], sep="-"))
}


# A function that can be mapped to each time in order to create a proper POSIXct datetime
# The first argument is the date as a string (e.g. 2008-12-25)
# The second argument is the time in milliseconds from midnight format
convert_time <- function(date, time)
{
  hour = (time %/% 3600000) %% 24
  min = (time %/% 60000) %% 60
  sec = (time %/% 1000) %% 60
  
  # Get the time and final datetime, which is converted to POSIXct
  time_str = paste(hour,min,sec,sep=":")
  datetime = paste(date, time_str)
  return(as.POSIXct(strptime(datetime, "%Y-%m-%d %H:%M:%S")))
}


# Gather some basic summary statistics on the data, such as the min,mean,max, and sd for each symbol
# The argument data is the data.table to generate the summary statistics for
summary_stats <- function(data)
{
  # List of columns and summary statistics to calculate on the data
  return(data[,list(
      min_NBB=min(NBB), mean_NBB=mean(NBB), med_NBB=median(NBB), max_NBB=max(NBB), sd_NBB=sd(NBB),
      min_NBO=min(NBO), mean_NBO=mean(NBO), med_NBO=median(NBO), max_NBO=max(NBO), sd_NBO=sd(NBO),
      min_NQBB=min(NQBB), mean_NQBB=mean(NQBB), med_NQBB=median(NQBB), max_NQBB=max(NQBB), sd_NQBB=sd(NQBB),
      min_NQBO=min(NQBO), mean_NQBO=mean(NQBO), med_NQBO=median(NQBO), max_NQBO=max(NQBO), sd_NQBO=sd(NQBO),
      min_NRQHS=min(NRQHS), mean_NRQHS=mean(NRQHS), med_NRQHS=median(NRQHS), max_NRQHS=max(NRQHS), sd_NRQHS=sd(NRQHS),
      min_NRPI5=min(NRPI5), mean_NRPI5=mean(NRPI5), med_NRPI5=median(NRPI5), max_NRPI5=max(NRPI5), sd_NRPI5=sd(NRPI5),
      min_NQRQHS=min(NQRQHS), mean_NQRQHS=mean(NQRQHS), med_NQRQHS=median(NQRQHS), max_NQRQHS=max(NQRQHS), sd_NQRQHS=sd(NQRQHS),
      min_NQRPI5=min(NQRPI5), mean_NQRPI5=mean(NQRPI5), med_NQRPI5=median(NQRPI5), max_NQRPI5=max(NQRPI5), sd_NQRPI5=sd(NQRPI5)      
  ), by="time,symbol"])
}


# Directory containing the files to parse and analyze
file_dir <- "/run/media/jon/TOSHIBA/RESEARCH_DATA/short_sale_taq/separate"

# File to write the summary statistics for daily results to as CSV
time_weight_results_file <- "/run/media/jon/TOSHIBA/RESEARCH_DATA/short_sale_taq/results.csv"

# Get a list of the files in the directory
filenames <- list.files(file_dir, pattern="*.csv", full.names=TRUE)

# Read in each file and gather summary statistics, store final combined results for further analysis
for (file in filenames)
{  
  # Load the individual file
  taq <- fread(file)
  
  # Get the date of the file from the filename
  file_date <- date_from_name(file)
  
  # Set the time entry as the file date
  taq[, time := NULL]
  taq[, time := as.POSIXct(strptime(file_date, "%Y-%m-%d"))]

  # Convert data to the proper types
  taq[, symbol := as.factor(symbol)]
  
  # set the time and symbol as keys
  setkey(taq, time, symbol)
  gc()
  
  # Calculate basic summary statistics for each symbol
  results <- summary_stats(taq)
  
  # Add the results for the current day to the daily list of results
  if (exists("time_weight_results"))
  {
      time_weight_results <- rbind(time_weight_results,results)
  }
  else
  {
      time_weight_results <- results
  }
  
  # Update the key indexes as files may not be loaded in correct date order
  setkey(time_weight_results, time, symbol)
  
  # Free up memory
  rm(taq,results)
  gc()
}

# Write the results to CSV file
write.csv(time_weight_results, time_weight_results_file, row.names = FALSE)