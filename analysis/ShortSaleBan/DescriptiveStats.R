###############################################################################
#
# Generates descriptive statistics for the entire dataset based on the summary
# statistics for each day.
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

# Converts a string representation of a date into a POSIXct object, to greatly
# simplify using string dates in comparisons
as_date <- function(date)
{
  return(as.POSIXct(strptime(date, "%Y-%m-%d")))
}


# Get a list of the banned symbols that having missing days during the preban period and the
# number of active trading days that the symbol is missing from
miss_days_preban <- function(banned_symbols, symbols_list)
{
  missing_preban <- data.table()
  
  for (entry in banned_symbols)
  {
    start_date <- symbols_list[symbol == entry, time][1]
    ban_date <- symbols_list[symbol == entry, AddDate][1]
    ban_days <- NROW(symbols_list[symbol == entry & time >= start_date & time < ban_date])
    
    # List of all trading days during the ban period
    all_ban_days <- NROW(symbols_list[time >= start_date & time < ban_date, list(time=unique(time))])
    
    if (ban_days < all_ban_days)
    {
      # Add the symbol to the list of symbols that do not trade during the ban period
      missing_days <- all_ban_days - ban_days
      
      if(NROW(missing_preban) > 0)
      {
        missing_preban <- rbind(missing_preban, data.table(symbol=entry, missing_days=missing_days))
      }
      else
      {
        missing_preban <- data.table(symbol=entry, missing_days=missing_days)
      }
    }
  }
  
  return(missing_preban)
}



# Get a list of the banned symbols that having missing days at all during the time the symbol
# was first listed and the last date the sybmol was listed
miss_days_all <- function(banned_symbols, symbols_list)
{
  missing_all <- data.table()
  
  for (entry in banned_symbols)
  {
    start_date <- symbols_list[symbol == entry, time][1]
    end_date <- last(symbols_list[symbol == entry, time])
    symbol_days <- NROW(symbols_list[symbol == entry & time %between% c(start_date,end_date)])
    
    # List of all trading days during the ban period
    all_days <- NROW(symbol_means[time %between% c(start_date,end_date), list(time=unique(time))])
    
    if (symbol_days < all_days)
    {
      # Add the symbol to the list of symbols that do not trade during the ban period
      missing_days <- all_days - symbol_days
      
      if(NROW(missing_all) > 0)
      {
        missing_all <- rbind(missing_all, data.table(symbol=entry, missing_days=missing_days))
      }
      else
      {
        missing_all <- data.table(symbol=entry, missing_days=missing_days)
      }
    }
  }
  
  return(missing_all)
}



# Read in the daily summary and time-weighted results
daily_results_file <- "/run/media/jon/TOSHIBA/RESEARCH_DATA/short_sale_taq/daily_results_test.csv"
time_weight_results_file <- "/run/media/jon/TOSHIBA/RESEARCH_DATA/short_sale_taq/time_weight_results_test.csv"

daily_results <- fread(daily_results_file)
time_weight_results <- fread(time_weight_results_file)

# Convert data to the proper types
daily_results[, time := as_date(time)]
daily_results[, symbol := as.factor(symbol)]
daily_results[, type := as.factor(type)]
daily_results[, ShortSale := as.logical(ShortSale)]

time_weight_results[, time := as_date(time)]
time_weight_results[, symbol := as.factor(symbol)]

# Set the keys
setkey(daily_results, time, symbol, type, ShortSale)
setkey(time_weight_results, time, symbol)


# Generate some descriptive statistics for the banned stocks
banned_stocks 



