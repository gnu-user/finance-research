###############################################################################
#
# Performs analysis of the TAQ as well as time-weighted data to produce daily
# summary statistics for each symbol, type, and whether it is a ShortSale.
# These summary statistics are then aggregated to produce more general
# descriptive statistics that describe all stocks based on criteria.
# 
# Copyright (C) 2013, Jonathan Gillett
# All rights reserved.
#
#
# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <http://www.gnu.org/licenses/>.
#
###############################################################################
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
  match <- str_match(filename, regexp)
  
  return(paste(match[4], date_num[match[3]], match[2], sep="-"))
}


# A function that gets the postfix date from the filename
# The argument is the name of the file
filename_date <- function(filename)
{
  # Extract the date from filename
  regexp <- "_([0-9]{2}[a-z]{3}[0-9]{4})"
  match <- str_match(filename, regexp)
  
  return(match[2])
}


# A function that gets the name of the time weighted file based on the taq filename
# The argument is the name of the taq file
weighted_file_name <- function(filename)
{
  regexp <- "_([0-9]{2}[a-z]{3}[0-9]{4}\\.csv)"
  match <- str_match(filename, regexp)
  
  return(paste("taq_time_", match[2], sep=""))
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
                  min_NRES=min(NRES), mean_NRES=mean(NRES), med_NRES=median(NRES), max_NRES=max(NRES), sd_NRES=sd(NRES),
                  min_NQRES=min(NQRES), mean_NQRES=mean(NQRES), med_NQRES=median(NQRES), max_NQRES=max(NQRES), sd_NQRES=sd(NQRES),
                  min_NRPI5=min(NRPI5), mean_NRPI5=mean(NRPI5), med_NRPI5=median(NRPI5), max_NRPI5=max(NRPI5), sd_NRPI5=sd(NRPI5),
                  min_NQRPI5=min(NQRPI5), mean_NQRPI5=mean(NQRPI5), med_NQRPI5=median(NQRPI5), max_NQRPI5=max(NQRPI5), sd_NQRPI5=sd(NQRPI5),
                  rel_range=((max(price) - min(price)) / (sum(volume) / sum(shares)))
                ),
              by="time,symbol,buysell,type,ShortSale"])
}


# Gather some basic summary statistics on the time weighted data, such as the min,mean,max, and sd 
# for each symbol. The argument data is the data.table to generate the summary statistics for
summary_stats_weighted <- function(data)
{
  # List of columns and summary statistics to calculate on the data
  return(data[,list(
    min_NBB=min(NBB), mean_NBB=mean(NBB), med_NBB=median(NBB), max_NBB=max(NBB), sd_NBB=sd(NBB),
    min_NBO=min(NBO), mean_NBO=mean(NBO), med_NBO=median(NBO), max_NBO=max(NBO), sd_NBO=sd(NBO),
    min_NQBB=min(NQBB), mean_NQBB=mean(NQBB), med_NQBB=median(NQBB), max_NQBB=max(NQBB), sd_NQBB=sd(NQBB),
    min_NQBO=min(NQBO), mean_NQBO=mean(NQBO), med_NQBO=median(NQBO), max_NQBO=max(NQBO), sd_NQBO=sd(NQBO),
    min_NRQHS=min(NRQHS), mean_NRQHS=mean(NRQHS), med_NRQHS=median(NRQHS), max_NRQHS=max(NRQHS), sd_NRQHS=sd(NRQHS),
    min_NQRQHS=min(NQRQHS), mean_NQRQHS=mean(NQRQHS), med_NQRQHS=median(NQRQHS), max_NQRQHS=max(NQRQHS), sd_NQRQHS=sd(NQRQHS)     
  ), by="time,symbol"])
}


# Write the simplified summary results containing the means and medians rather than all of the
# other statistical calculations such as std. deviation
write_simplified_summary <- function(trade_output_file, time_output_file)
{
  # Write the trade weighted results without all of the unnecessary columns
  out_daily <- copy(daily_results)
  out_daily[, AddDate := as.POSIXct(AddDate, origin = "1970-01-01")]
  out_daily[, AddDate := as.POSIXct(strptime(AddDate, "%Y-%m-%d"))]
  out_daily[, ExpDate := as.POSIXct(ExpDate, origin = "1970-01-01")]
  out_daily[, ExpDate := as.POSIXct(strptime(ExpDate, "%Y-%m-%d"))]
  
  
  write.csv(out_daily[,
                      list(time, symbol, type, ShortSale, 
                           n, mean_shares, med_shares, sum_shares,
                           mean_price, med_price,
                           mean_vol, med_vol, sum_vol,
                           mean_NRES, med_NRES, mean_NQRES, med_NQRES,
                           mean_NRPI5, med_NRPI5, mean_NQRPI5, med_NQRPI5,
                           rel_range, market_cap, 
                           banned, AddDate, ExpDate)], 
            trade_output_file, 
            row.names = FALSE)
  rm(out_daily)
  
  
  
  # Write the time weighted results without all of the unnecessary columns
  write.csv(time_weight_results[,
                                list(time, symbol,
                                     mean_NBB, med_NBB, mean_NBO, med_NBO,
                                     mean_NQBB, med_NQBB, mean_NQBO, med_NQBO,
                                     mean_NRQHS, med_NRQHS, mean_NQRQHS, med_NQRQHS)],
            time_output_file, 
            row.names = FALSE)
}




# A flag specifying whether or not to calculate the results using trimming
TRIM_RESULTS <- FALSE

# Directory containing the files to parse and analyze
taq_dir <- "/run/media/jon/TOSHIBA/RESEARCH_DATA/Finance/short_sale_taq/test_taq/"

# Directory to save the preliminary taq calculations, which are used to calculate the final aggregate results
taq_results_dir <- "/run/media/jon/TOSHIBA/RESEARCH_DATA/Finance/short_sale_taq/test_results/"

# Directory containing the time weighted files
time_dir <- "/run/media/jon/TOSHIBA/RESEARCH_DATA/Finance/short_sale_taq/test_time/"

# File containing the market capitalization data
market_cap_file <- "/run/media/jon/TOSHIBA/RESEARCH_DATA/Finance/short_sale_taq/market_cap.csv"

# File to write the summary statistics for daily results to as CSV
daily_results_file <- "/run/media/jon/TOSHIBA/RESEARCH_DATA/Finance/short_sale_taq/daily_results_trim.csv"

# File to write the summary statistics for time weighted results to as CSV
time_weight_results_file <- "/run/media/jon/TOSHIBA/RESEARCH_DATA/Finance/short_sale_taq/time_weight_results_trim.csv"




# Get a list of the files in the directory
filenames <- list.files(taq_dir, pattern="*.csv", full.names=TRUE)

# Read in each file and gather summary statistics, store final combined results for further analysis
for (file in filenames)
{
  # Load the individual file
  taq <- fread(file)
  
  # Set the filename for the output to save the preliminary taq results
  taq_output_file <- paste("taq_results_", filename_date(file), ".csv", sep="")
  
  
  # Get the date of the file from the filename
  file_date <- date_from_name(file)
  cur_date <- as.POSIXct(strptime(file_date, "%Y-%m-%d"))
  
  
  # Set the time boundaries for trimming data before the first 5 mins and the last 5 min
  left_bound <- as.POSIXct(strptime(paste(file_date, "09:35:00"), "%Y-%m-%d %H:%M:%S"))
  right_bound <- as.POSIXct(strptime(paste(file_date, "15:55:00"), "%Y-%m-%d %H:%M:%S"))
  
  
  # Add a column with the time as seconds to midnight before converting to a datetime value
  taq[, sec_midnight := (time %/% 1000)]
  
  # Set the time entry for each trade as a datetime
  taq[, time := convert_time(file_date, time)]
  #taq[, time := as.POSIXct(strptime(file_date, "%Y-%m-%d"))]
  #taq[, time := as.POSIXct(time, origin = "1970-01-01", tz="GMT")]
  
  
  # Clean the data read from CSV, remove unncessary columns: date, buysell, shorttype and linkindicator
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

  
  # Reorder the colums so that seconds since midnight is after the date (SAS date format)
  setcolorder(taq, c("time", "symbol", "shares", "buysell", "price", "type", "date", "sec_midnight", "NBB", "NBO",
                     "NBBSIZ", "NBOSIZ", "NQBB", "NQBO", "NQBBSIZ", "NQBOSIZ", "ShortSale", "ShortSize"))
  
  
  # Add a row for the volume and calculate it
  taq[, volume := shares * price]
  setkey(taq, time, symbol)
  gc()
  
  
  # Set the trade direction, used to set RES/RPI5 as +/- if the trade is a buy or sell
  trade_direction <- data.table(type=c("B", "S"), v1=c(1, -1))
  setkey(trade_direction, type)
  
  
  
  # Load the corresponding time weighted file
  time_file <- paste(time_dir, weighted_file_name(file), sep="")
  time_weighted <- fread(time_file)
  gc()
  
  # Clean the data, convert data to proper types
  time_weighted[, time := as.POSIXct(strptime(paste(file_date, time), "%Y-%m-%d %H:%M:%S"))]
  gc()
  time_weighted[, symbol := as.factor(symbol)]
  
  # set the time and symbol as keys
  setkey(time_weighted, time, symbol)
  gc()
  
  
  
  # Add the midpoint and midpoint in 5 minute columns from the time weighted data
  taq <- merge(taq, time_weighted[, list(time, symbol, NMID, NMID5, NQMID, NQMID5)])
  gc()
  
  
  
  # Calculate the RES using the National and NSDQ BB/BO
  # |Pt - Qt| / Qt , where Qt = (At + Bt) / 2
  taq[, NRES := ((price - NMID) / NMID) * trade_direction[buysell][,v1]]
  taq[, NQRES := ((price - NQMID) / NQMID) * trade_direction[buysell][,v1]]
  
  
   
  # Calculate the RPI5 using the National and NSDQ BB/BO and midpoint in 5 minutes
  # RPI5 = (Q_5min - Q) / Q, where Qt = (At + Bt) / 2
  taq[, NRPI5 := ((NMID5 - NMID) / NMID) * trade_direction[buysell][,v1]]
  gc()
  
  taq[, NQRPI5 := ((NQMID5 - NQMID) / NQMID) * trade_direction[buysell][,v1]]
  gc()
  
  
  # Set any Inf results for RES or RPI5 to 0
  taq[!is.finite(NRES), NRES := 0]
  taq[!is.finite(NQRES), NQRES := 0]
  taq[!is.finite(NRPI5), NRPI5 := 0]
  taq[!is.finite(NQRPI5), NQRPI5 := 0]
  
  
  # set the time, symbol, type, and ShortSale as keys
  setkey(taq, time, symbol, buysell, type, ShortSale)
  gc()


  
  
  # Store the current calculations for the TAQ data to a file
  write.csv(taq, paste(taq_results_dir, taq_output_file, sep="/"), row.names = FALSE)
  
  
  
  # Calculate basic summary statistics, aggregate the results for the current day 
  # based on symbol, type, and shortsale indicator
  if (TRIM_RESULTS)
  {
    taq <- taq[time >= left_bound & time <= right_bound]
    gc()
  }
  
  taq[, time := cur_date]
  setkey(taq, time, symbol, buysell, type, ShortSale)
  results <- summary_stats(taq)
  gc()
  
  # Add the results for the current day to the daily list of results
  if (exists("daily_results"))
  {
    daily_results <- rbind(daily_results, results)
  }
  else
  {
    daily_results <- results
  }
  
  # Update the key indexes as files may not be loaded in correct date order
  setkey(daily_results, time, symbol, buysell, type, ShortSale)
  
  # Free up memory
  rm(taq, results)
  gc()
  
  

  # Calculate basic time weighted summary statistics for each symbol
  if (TRIM_RESULTS)
  {
    time_weighted <- time_weighted[time >= left_bound & time <= right_bound]
    gc()
  }
  
  time_weighted[, time := cur_date]
  setkey(time_weighted, time, symbol)
  gc()
  
  results <- summary_stats_weighted(time_weighted)
  gc()
  
  # Add the results for the current day to the daily list of results
  if (exists("time_weight_results"))
  {
    time_weight_results <- rbind(time_weight_results, results)
  }
  else
  {
    time_weight_results <- results
  }
  
  # Update the key indexes as files may not be loaded in correct date order
  setkey(time_weight_results, time, symbol)
  
  # Free up memory
  rm(time_weighted, results)
  gc()
}



# Calculate the market capitalization for each stock and add it to the daily results
market_cap <- fread(market_cap_file)

# Convert the date entry
market_cap[, time := as.POSIXct(strptime(time, "%Y%m%d"))]
market_cap[, symbol := as.factor(symbol)]

# Clean the data read from CSV, remove unncessary columns
market_cap[, permno := NULL]

# Ignore any erroneous entries missing symbols, prc, or shrout
market_cap <- market_cap[symbol != "" | !is.na(prc) | !is.na(shrout)]

# Calculate market cap for each stock
market_cap[, market_cap := prc * shrout]
setkey(market_cap, time, symbol)

# Calculate the market cap for each day for each symbol
for (entry in daily_results[, unique(symbol)])
{
  dates <- daily_results[symbol == entry, unique(time)]
  
  if (exists("results"))
  {
    results <- rbind(results, 
                     market_cap[symbol == entry & time %in% dates, list(market_cap), by="time,symbol"])
  }
  else
  {
    results <- market_cap[symbol == entry & time %in% dates, list(market_cap), by="time,symbol"]
  }
}

# NOTE any symbols not in the market results will be removed from daily_results after merge!!!
setkey(results, time, symbol)
daily_results <- merge(daily_results, 
                       results[, list(market_cap), by="time,symbol"])

# Free up memory
rm(market_cap, results)
gc()


# Write the results to CSV file
write.csv(daily_results, daily_results_file, row.names = FALSE)
write.csv(time_weight_results, time_weight_results_file, row.names = FALSE)


