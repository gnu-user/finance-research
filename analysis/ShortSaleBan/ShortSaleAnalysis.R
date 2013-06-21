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
              by="time,symbol,type,ShortSale"])
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


# Directory containing the files to parse and analyze
taq_dir <- "/run/media/jon/TOSHIBA/RESEARCH_DATA/short_sale_taq/test_taq/"

# Directory containing the time weighted files
time_dir <- "/run/media/jon/TOSHIBA/RESEARCH_DATA/short_sale_taq/test_time/"

# File to write the summary statistics for daily results to as CSV
daily_results_file <- "/run/media/jon/TOSHIBA/RESEARCH_DATA/short_sale_taq/daily_results_test.csv"

# File to write the summary statistics for time weighted results to as CSV
time_weight_results_file <- "/run/media/jon/TOSHIBA/RESEARCH_DATA/short_sale_taq/time_weight_results_test.csv"


# Get a list of the files in the directory
filenames <- list.files(taq_dir, pattern="*.csv", full.names=TRUE)

# Read in each file and gather summary statistics, store final combined results for further analysis
for (file in filenames)
{
  # Load the individual file
  taq <- fread(file)
  
  # Get the date of the file from the filename
  file_date <- date_from_name(file)
  cur_date <- as.POSIXct(strptime(file_date, "%Y-%m-%d"))
  
  # Set the time entry for each trade
  taq[, time := convert_time(file_date, time)]
  #taq[, time := as.POSIXct(strptime(file_date, "%Y-%m-%d"))]
  #taq[, time := as.POSIXct(time, origin = "1970-01-01", tz="GMT")]

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
  taq[, NRES := abs(price - ((NBO + NBB) / 2)) / ((NBO + NBB) / 2)]
  taq[, NQRES := abs(price - ((NQBO + NQBB) / 2)) / ((NQBO + NQBB) / 2)]
  
  # set the time, symbol, type, and ShortSale as keys
  setkey(taq, time, symbol, type, ShortSale)
  gc()
  
  
  # Load the corresponding time weighted file
  time_file <- paste(time_dir, weighted_file_name(file), sep="")
  time_weighted <- fread(time_file)
  gc()
  
  # Clean the data, convert data to proper types
  time_weighted[, NMID := NULL]
  time_weighted[, NQMID := NULL]
  time_weighted[, time := as.POSIXct(strptime(paste(file_date, time), "%Y-%m-%d %H:%M:%S"))]
  gc()
  time_weighted[, symbol := as.factor(symbol)]

  # set the time and symbol as keys
  setkey(time_weighted, time, symbol)
  gc()
  

  # Calculate the RPI5 using the National and NSDQ BB/BO and 
  # midpoint in 5 minutes from the time-weighted file
  # RPI5 = (Q_5min - Q) / Q, where Qt = (At + Bt) / 2
  for (ticker in taq[, list(symbol=unique(symbol))]$symbol)
  {
    # Get the trade times
    times <- taq[symbol == ticker, time]
    
    mid5 <- time_weighted[J(times, ticker)]$NMID5
    taq[symbol == ticker, NRPI5 := (mid5 - ((NBO + NBB) / 2)) / ((NBO + NBB) / 2)]
    gc()
    
    mid5 <- time_weighted[J(times, ticker)]$NQMID5
    taq[symbol == ticker, NQRPI5 := (mid5 - ((NQBO + NQBB) / 2)) / ((NQBO + NQBB) / 2)]
    gc()
  }
  
  # Set any Inf results for RES or RPI5 to 0
  taq[!is.finite(NRES), NRES := 0]
  taq[!is.finite(NQRES), NQRES := 0]
  taq[!is.finite(NRPI5), NRPI5 := 0]
  taq[!is.finite(NQRPI5), NQRPI5 := 0]
  
  

  # Calculate basic summary statistics, aggregate the results for the current day 
  # based on symbol, type, and shortsale indicator
  taq[, time := cur_date]
  setkey(taq, time, symbol, type, ShortSale)
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
  setkey(daily_results, time, symbol, type, ShortSale)
  
  # Free up memory
  rm(taq, results)
  gc()
  
  

  # Calculate basic time weighted summary statistics for each symbol
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

# Write the results to CSV file
write.csv(daily_results, daily_results_file, row.names = FALSE)
write.csv(time_weight_results, time_weight_results_file, row.names = FALSE)