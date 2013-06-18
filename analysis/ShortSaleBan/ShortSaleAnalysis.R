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
                  min_shares=min(shares), mean_shares=mean(shares), med_shares=as.integer(median(shares)), max_shares=max(shares), sd_shares=sd(shares), sum_shares=sum(shares), 
                  min_price=min(price), mean_price=mean(price), med_price=median(price), max_price=max(price), sd_price=sd(price),
                  min_vol=min(volume), mean_vol=mean(volume), med_vol=median(volume), max_vol=max(volume), sd_vol=sd(volume), sum_vol=sum(volume), 
                  min_RES_NTL=min(RES_NTL), mean_RES_NTL=mean(RES_NTL), med_RES_NTL=median(RES_NTL), max_RES_NTL=max(RES_NTL), sd_RES_NTL=sd(RES_NTL),
                  min_RES_NSDQ=min(RES_NSDQ), mean_RES_NSDQ=mean(RES_NSDQ), med_RES_NSDQ=median(RES_NSDQ), max_RES_NSDQ=max(RES_NSDQ), sd_RES_NSDQ=sd(RES_NSDQ),
                  rel_range=((max(price) - min(price)) / (sum(volume) / sum(shares)))
                ),
              by="time,symbol,type,ShortSale"])
}


# Directory containing the files to parse and analyze
file_dir <- "/run/media/jon/TOSHIBA/RESEARCH_DATA/short_sale_taq/separate"

# File to write the summary statistics for daily results to as CSV
daily_results_file <- "/run/media/jon/TOSHIBA/RESEARCH_DATA/short_sale_taq/results.csv"

# Get a list of the files in the directory
filenames <- list.files(file_dir, pattern="*.csv", full.names=TRUE)

# Read in each file and gather summary statistics, store final combined results for further analysis
for (file in filenames)
{
  # Load the individual file
  taq <- fread(file)
  
  # Get the date of the file from the filename
  file_date <- date_from_name(file)

  # Set the time entry as just the date, if finer granularity needed, use convert_time()
  #taq[, time := convert_time(file_date,time)]
  taq[, time := as.POSIXct(strptime(file_date, "%Y-%m-%d"))]
  taq[, time := as.POSIXct(time, origin = "1970-01-01", tz="GMT")]

  # Clean the data read from CSV, remove unncessary columns, date, shorttype and linkindicator
  taq[, date := NULL]
  taq[, buysell := NULL]
  taq[, ShortType := NULL]
  taq[, LinkIndicator := NULL]
  
  # Convert data to the proper types
  taq[, symbol := as.factor(symbol)]
  taq[, type := as.factor(type)]
  taq[, shares := as.integer(shares)]
  taq[, NBBSIZ := as.integer(NBBSIZ)]
  taq[, NBOSIZ := as.integer(NBOSIZ)]
  taq[, NQBBSIZ := as.integer(NQBBSIZ)]
  taq[, NQBOSIZ := as.integer(NQBOSIZ)]
  taq[, ShortSale := as.logical(ShortSale)]

  # Add a row for the volume and calculate it
  taq[, volume := shares * price]
  
  # Calculate the RES using the National and NSDQ BB/BO
  # |Pt - Qt| / Qt , where Qt = (At + Bt) / 2
  taq[, RES_NTL := abs(price - ((NBO + NBB) / 2)) / ((NBO + NBB) / 2)]
  taq[, RES_NSDQ := abs(price - ((NQBO + NQBB) / 2)) / ((NQBO + NQBB) / 2)]
  
  # Set any Inf results for RES to 0
  taq[!is.finite(RES_NTL), RES_NTL := 0]
  taq[!is.finite(RES_NSDQ), RES_NSDQ := 0]
  
  # set the time, symbol, type, and ShortSale as keys
  setkey(taq, time, symbol, type, ShortSale)
    
  # Calculate basic summary statistics for all possible combinations of type, and shortsale indicator
  results <- summary_stats(taq)
  
  # Add the results for the current day to the daily list of results
  if (exists("daily_results"))
  {
    daily_results <- rbind(daily_results,results)
  }
  else
  {
    daily_results <- results
  }
  
  # Update the key indexes as files may not be loaded in correct date order
  setkey(daily_results, time, symbol, type, ShortSale)
  
  # Free up memory
  rm(taq,results)
  gc()
}

# Write the results to CSV file
write.csv(daily_results, daily_results_file, row.names = FALSE)