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


# Gather some basic summary statistics on the data, such as the min,mean,max, and sd
# For each type of trade (NN, HN, ...) and based on whether it is a shortsale or not
# The argument data is the data.table to generate the summary statistics for
summary_stats <- function(data)
{
  # List of columns and summary statistics to calculate on the data
  return(data[,list(
                  n=NROW(price), 
                  min_shares=min(shares), mean_shares=mean(shares), max_shares=max(shares), sd_shares=sd(shares), sum_shares=sum(shares), 
                  min_price=min(price), mean_price=mean(price), max_price=max(price), sd_price=sd(price),
                  min_vol=min(volume), mean_vol=mean(volume), max_vol=max(volume), sd_vol=sd(volume), sum_vol=sum(volume), 
                  min_NBB=min(NBB), mean_NBB=mean(NBB), max_NBB=max(NBB), sd_NBB=sd(NBB),
                  min_NBO=min(NBO), mean_NBO=mean(NBO), max_NBO=max(NBO), sd_NBO=sd(NBO),
                  min_NBBSIZ=min(NBBSIZ), mean_NBBSIZ=mean(NBBSIZ), max_NBBSIZ=max(NBBSIZ), sd_NBBSIZ=sd(NBBSIZ),
                  min_NBOSIZ=min(NBOSIZ), mean_NBOSIZ=mean(NBOSIZ), max_NBOSIZ=max(NBOSIZ), sd_NBOSIZ=sd(NBOSIZ),
                  min_NQBB=min(NQBB), mean_NQBB=mean(NQBB), max_NQBB=max(NQBB), sd_NQBB=sd(NQBB),
                  min_NQBO=min(NQBO), mean_NQBO=mean(NQBO), max_NQBO=max(NQBO), sd_NQBO=sd(NQBO),
                  min_NQBBSIZ=min(NQBBSIZ), mean_NQBBSIZ=mean(NQBBSIZ), max_NQBBSIZ=max(NQBBSIZ), sd_NQBBSIZ=sd(NQBBSIZ),
                  min_NQBOSIZ=min(NQBOSIZ), mean_NQBOSIZ=mean(NQBOSIZ), max_NQBOSIZ=max(NQBOSIZ), sd_NQBOSIZ=sd(NQBOSIZ)
                ),
              by="time,symbol,buysell,type,ShortSale"])
}


# Directory containing the files to parse and analyze
file_dir <- "/run/media/jon/TOSHIBA/RESEARCH_DATA/short_sale_taq/separate"

# File to write the results to as CSV
results_file <- "/run/media/jon/TOSHIBA/RESEARCH_DATA/short_sale_taq/results.csv"

# Overrides for parsing file columns
column_types <- c("symbol"="character", "LinkIndicator"="factor")

# Get a list of the files in the directory
filenames <- list.files(file_dir, pattern="*.csv", full.names=TRUE)

# Read in each file and gather summary statistics, store final combined results for further analysis
for (file in filenames)
{
  # Load the individual file
  taq <- read.csv(file, colClasses=column_types)

  # Get the date of the file from the filename
  file_date <- date_from_name(file)

  # Set the time entry as just the date, if finer granularity needed, use convert_time()
  #taq <- taq[, time := convert_time(file_date,time)]
  taq$time <- as.POSIXct(strptime(file_date, "%Y-%m-%d"))
  
  # Create a data.table
  taq <- data.table(taq)
  
  # Clean the data read from CSV, remove unncessary columns, convert data to the proper types
  taq <- taq[, date := NULL]
  taq <- taq[, shares := as.integer(shares)]
  taq <- taq[, NBBSIZ := as.integer(NBBSIZ)]
  taq <- taq[, NBOSIZ := as.integer(NBOSIZ)]
  taq <- taq[, NQBBSIZ := as.integer(NQBBSIZ)]
  taq <- taq[, NQBOSIZ := as.integer(NQBOSIZ)]
  taq <- taq[, ShortSale := as.logical(ShortSale)]
  
  # Add a row for the volume and calculate it
  taq <- taq[, volume := shares * price]
  
  # set the time, symbol, type, and ShortSale as keys
  setkey(taq, time, symbol, buysell, type, ShortSale)
  
  # Calculate basic summary statistics for all possible combinations of Buy/Sell, type, and shortsale indicator
  results <- summary_stats(taq)
  
  # Add the results for the current day to the final list of results
  if (exists("final_results"))
  {
    final_results <- rbind(final_results,results)
  }
  else
  {
    final_results <- results
  }
  
  # Update the key indexes as files may not be loaded in correct date order
  setkey(final_results, time, symbol, buysell, type, ShortSale)
  
  # Free up memory
  rm(taq,results)
  gc()
}

# Write the results to CSV file
write.csv(final_results, results_file, row.names = FALSE)