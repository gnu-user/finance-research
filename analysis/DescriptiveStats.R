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
    preban_days <- NROW(symbols_list[symbol == entry & time >= start_date & time < ban_date])
    
    # List of all trading days during the ban period
    all_preban_days <- NROW(symbols_list[time >= start_date & time < ban_date, list(time=unique(time))])
    
    if (preban_days < all_preban_days)
    {
      # Add the symbol to the list of symbols that do not trade during the ban period
      missing_days <- all_preban_days - preban_days
      
      if(NROW(missing_preban) > 0)
      {
        missing_preban <- rbind(missing_preban, data.table(symbol=entry, days=missing_days))
      }
      else
      {
        missing_preban <- data.table(symbol=entry, days=missing_days)
      }
    }
  }
  
  return(missing_preban)
}


# Get a list of the banned symbols that having missing days during the ban period and the
# number of active trading days that the symbol is missing from
miss_days_ban <- function(banned_symbols, symbols_list)
{
  missing_ban <- data.table()
  
  for (entry in banned_symbols)
  {
    ban_date <- symbols_list[symbol == entry, AddDate][1]
    exp_date <- symbols_list[symbol == entry, ExpDate][1]
    ban_days <- NROW(symbols_list[symbol == entry & time >= ban_date & time < exp_date])
    
    # List of all trading days during the ban period
    all_ban_days <- NROW(symbols_list[time >= ban_date & time < exp_date, list(time=unique(time))])
    
    if (ban_days < all_ban_days)
    {
      # Add the symbol to the list of symbols that do not trade during the ban period
      missing_days <- all_ban_days - ban_days
      
      if(NROW(missing_ban) > 0)
      {
        missing_ban <- rbind(missing_ban, data.table(symbol=entry, days=missing_days))
      }
      else
      {
        missing_ban <- data.table(symbol=entry, days=missing_days)
      }
    }
  }
  
  return(missing_ban)
}


# Get a list of the banned symbols that having missing days during the post-ban period and the
# number of active trading days that the symbol is missing from
miss_days_postban <- function(banned_symbols, symbols_list)
{
  missing_postban <- data.table()
  
  for (entry in banned_symbols)
  {
    exp_date <- symbols_list[symbol == entry, ExpDate][1]
    end_date <- last(symbols_list[symbol == entry, time])
    postban_days <- NROW(symbols_list[symbol == entry & time >= exp_date & time <= end_date])
    
    # List of all trading days during the ban period
    all_postban_days <- NROW(symbols_list[time >= exp_date & time <= end_date, list(time=unique(time))])
    
    if (postban_days < all_postban_days)
    {
      # Add the symbol to the list of symbols that do not trade during the ban period
      missing_days <- all_postban_days - postban_days
      
      if(NROW(missing_postban) > 0)
      {
        missing_postban <- rbind(missing_postban, data.table(symbol=entry, days=missing_days))
      }
      else
      {
        missing_postban <- data.table(symbol=entry, days=missing_days)
      }
    }
  }
  
  return(missing_postban)
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
        missing_all <- rbind(missing_all, data.table(symbol=entry, days=missing_days))
      }
      else
      {
        missing_all <- data.table(symbol=entry, days=missing_days)
      }
    }
  }
  
  return(missing_all)
}


# Calculate the descriptive statistics for banned stocks and their correpsonding matches that were
# added to the original ban list. The descriptive stats include the means and medians for each period 
# (pre-ban, ban, post-ban) and for each type of trade, and based on whether the trade is a shortsale
desc_original_ban <- function(data, matches)
{
  # Get the list of banned symbols and their corresponding matches that were added to original banlist
  ban_date <- as_date("2008-09-19")
  exp_date <- as_date("2008-10-09")
  #banned <- matches[AddDate == ban_date, symbol]
  #matched <- matches[symbol %in% banned, match]

  # Types of HFT and Non-HFT demanding/supplying liquidity
  types <- data.table(type=c("HFT_D","HFT_S", "NHFT_D", "NHFT_S"), 
                      v1=c("HH", "HH", "NN", "NN"), 
                      v2=c("HN", "NH", "NH", "HN"))
  setkey(types, type)
  
  # ShortSale status 
  short_status <- c(TRUE,FALSE)
  
  
  # Display the results for the pre-ban period for banned symbols
  for (entry in types[, type])
  {
    for (status in short_status)
    {
      # Calculate the daily number of trades and trading volume based on the average of each stock each day
      daily_trades <- data[symbol %in% banned & time < ban_date & type %in% types[entry][,c(v1,v2)] & ShortSale == status,
                           list(trades=sum(n)), by="time,symbol"]
      
      daily_volume <- data[symbol %in% banned & time < ban_date & type %in% types[entry][,c(v1,v2)] & ShortSale == status,
                           list(volume=sum(sum_vol)), by="time,symbol"]
      
      daily_shares <- data[symbol %in% banned & time < ban_date & type %in% types[entry][,c(v1,v2)] & ShortSale == status,
                           list(shares=sum(sum_shares)), by="time,symbol"]
      
      result <- data[symbol %in% banned & time < ban_date & type %in% types[entry][,c(v1,v2)] & ShortSale == status, 
                     list(
                       group="original banned", period="pre-ban", type=entry, shortsale=status,
                       mean_NRES=mean(mean_NRES), med_NRES=median(med_NRES), mean_NQRES=mean(mean_NQRES), med_NQRES=median(med_NQRES),
                       mean_NRPI5=mean(mean_NRPI5), med_NRPI5=median(med_NRPI5), mean_NQRPI5=mean(mean_NQRPI5), med_NQRPI5=median(med_NQRPI5),
                       mean_daily_trades=mean(daily_trades[, trades]), med_daily_trades=median(daily_trades[, trades]),
                       mean_daily_volume=mean(daily_volume[, volume]), med_daily_volume=median(daily_volume[, volume]),
                       mean_rel_range=mean(rel_range), med_rel_range=median(rel_range),
                       mean_daily_shares=mean(daily_shares[, shares]), med_daily_shares=median(daily_shares[, shares]))]
      
      if (exists("results"))
      {
        results <- rbind(results, result)
      }
      else
      {
        results <- result
      }
    }
  }
  
  # Display the results for the ban period for banned symbols
  for (entry in types[, type])
  {
    for (status in short_status)
    {
      # Calculate the daily number of trades and trading volume based on the average of each stock each day
      daily_trades <- data[symbol %in% banned & time >= ban_date & time < exp_date & type %in% types[entry][,c(v1,v2)] & ShortSale == status,
                           list(trades=sum(n)), by="time,symbol"]
      
      daily_volume <- data[symbol %in% banned & time >= ban_date & time < exp_date & type %in% types[entry][,c(v1,v2)] & ShortSale == status,
                           list(volume=sum(sum_vol)), by="time,symbol"]
      
      daily_shares <- data[symbol %in% banned & time >= ban_date & time < exp_date & type %in% types[entry][,c(v1,v2)] & ShortSale == status,
                           list(shares=sum(sum_shares)), by="time,symbol"]
      
      result <- data[symbol %in% banned & time >= ban_date & time < exp_date & type %in% types[entry][,c(v1,v2)] & ShortSale == status, 
                     list(
                       group="original banned", period="ban", type=entry, shortsale=status,
                       mean_NRES=mean(mean_NRES), med_NRES=median(med_NRES), mean_NQRES=mean(mean_NQRES), med_NQRES=median(med_NQRES),
                       mean_NRPI5=mean(mean_NRPI5), med_NRPI5=median(med_NRPI5), mean_NQRPI5=mean(mean_NQRPI5), med_NQRPI5=median(med_NQRPI5),
                       mean_daily_trades=mean(daily_trades[, trades]), med_daily_trades=median(daily_trades[, trades]),
                       mean_daily_volume=mean(daily_volume[, volume]), med_daily_volume=median(daily_volume[, volume]),
                       mean_rel_range=mean(rel_range), med_rel_range=median(rel_range),
                       mean_daily_shares=mean(daily_shares[, shares]), med_daily_shares=median(daily_shares[, shares]))]

        results <- rbind(results, result)
    }
  }
  
  # Display the results for the post-ban period for banned symbols
  for (entry in types[, type])
  {
    for (status in short_status)
    {
      # Calculate the daily number of trades and trading volume based on the average of each stock each day
      daily_trades <- data[symbol %in% banned & time >= exp_date & type %in% types[entry][,c(v1,v2)] & ShortSale == status,
                           list(trades=sum(n)), by="time,symbol"]
      
      daily_volume <- data[symbol %in% banned & time >= exp_date & type %in% types[entry][,c(v1,v2)] & ShortSale == status,
                           list(volume=sum(sum_vol)), by="time,symbol"]
      
      daily_shares <- data[symbol %in% banned & time >= exp_date & type %in% types[entry][,c(v1,v2)] & ShortSale == status,
                           list(shares=sum(sum_shares)), by="time,symbol"]
      
      result <- data[symbol %in% banned & time >= exp_date & type %in% types[entry][,c(v1,v2)] & ShortSale == status, 
                     list(
                       group="original banned", period="post-ban", type=entry, shortsale=status,
                       mean_NRES=mean(mean_NRES), med_NRES=median(med_NRES), mean_NQRES=mean(mean_NQRES), med_NQRES=median(med_NQRES),
                       mean_NRPI5=mean(mean_NRPI5), med_NRPI5=median(med_NRPI5), mean_NQRPI5=mean(mean_NQRPI5), med_NQRPI5=median(med_NQRPI5),
                       mean_daily_trades=mean(daily_trades[, trades]), med_daily_trades=median(daily_trades[, trades]),
                       mean_daily_volume=mean(daily_volume[, volume]), med_daily_volume=median(daily_volume[, volume]),
                       mean_rel_range=mean(rel_range), med_rel_range=median(rel_range),
                       mean_daily_shares=mean(daily_shares[, shares]), med_daily_shares=median(daily_shares[, shares]))]
      
      results <- rbind(results, result)
    }
  }
  
  return(results)
}

# Read in the daily summary and time-weighted results
daily_results_file <- "/run/media/jon/TOSHIBA/RESEARCH_DATA/short_sale_taq/daily_results.csv"
time_weight_results_file <- "/run/media/jon/TOSHIBA/RESEARCH_DATA/short_sale_taq/time_weight_results.csv"

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



# Generate some descriptive statistics for the banned and unbanned symbols
# Get a list of all symbols and dates they were present
symbols_list = final_results[, list(time=unique(time)), by="symbol"]
setkey(symbols_list, symbol, time)

# Set the banned date added and expiry date
for (ticker in ban_list$Ticker)
{
  symbols_list[ticker, banned := TRUE]
  symbols_list[ticker, AddDate := as_date(ban_list[Ticker == ticker, AddDate])]
  symbols_list[ticker, ExpDate := as_date(ban_list[Ticker == ticker, ExpDate])]
}
setkey(symbols_list, symbol, time, banned, AddDate, ExpDate)


# Generate descriptive statistics on the total number of stocks, and stocks added/removed
#stocks_added_preban <- 
#stocks_desc 



# Generate some descriptive statistics for the banned stocks where days are missing
missing_days_preban <- miss_days_preban(banned, symbols_list)
missing_days_ban <- miss_days_ban(banned, symbols_list)
missing_days_postban <- miss_days_postban(banned, symbols_list)

missing_days_desc <- data.table(period=c("pre-ban", "ban", "post-ban"), 
                           n=c(NROW(missing_days_preban), NROW(missing_days_ban), NROW(missing_days_postban)),
                           min_days=c(min(missing_days_preban[,days]), min(missing_days_ban[,days]), min(missing_days_postban[,days])),
                           mean_days=c(mean(missing_days_preban[,days]), mean(missing_days_ban[,days]), mean(missing_days_postban[,days])),
                           max_days=c(max(missing_days_preban[,days]), max(missing_days_ban[,days]), max(missing_days_postban[,days])))
    



# TODO generate descriptive statistics for matched stocks
# num stocks banned
# num stocks matched
# num stocks unmatched and removed due to the mean_diff > Q3 (outliers, very poor match)


# Calculate the descriptive statistics for the means and medians for each period (pre-ban, ban, post-ban)
# and for each type of trade, and based on whether the trade is a shortsale

# Calculate 



periods <- hash(keys=c("pre-ban", "ban", "post-ban")



