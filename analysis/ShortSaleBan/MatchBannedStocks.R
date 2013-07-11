###############################################################################
#
# Matches banned stocks to the best match possible given the context of trying
# to match all other stocks as best as possible. The matching is done using
# a stochastic approach to ensure that the best match possible is found for
# each banned stock.
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
library(quantmod)

# Converts a string representation of a date into a POSIXct object, to greatly
# simplify using string dates in comparisons
as_date <- function(date)
{
  return(as.POSIXct(strptime(date, "%Y-%m-%d")))
}


# Gather summary statistics for the matched symbol, such as the min, max, median, quartiles, sd, and skew
# The argument is the data containining the numeric and % price and volume difference for the symbol
summary_stats_matched <- function(ban_symbol, best_match, sum_differences, data)
{
  results <- data.table(ban_symbol = ban_symbol, match = best_match, sum_differences[1,list(sum_price, sum_vol, mean)])
  
  # Summary statistics for numeric price difference
  summary_stats <- summary(data[symbol == best_match, num_price])
  results[, min_num_price := summary_stats["Min."]]
  results[, q1_num_price := summary_stats["1st Qu."]]
  results[, med_num_price := summary_stats["Median"]]
  results[, mean_num_price := summary_stats["Mean"]]
  results[, q3_num_price := summary_stats["3rd Qu."]]
  results[, max_num_price := summary_stats["Max."]]
  results[, sd_num_price := sd(data[symbol == best_match, num_price])]
  results[, skew_num_price := skew(data[symbol == best_match, num_price])]
  results[, kurt_num_price := kurtosi(data[symbol == best_match, num_price])]
  
  # Summary statistics for % price difference
  summary_stats <- summary(data[symbol == best_match, per_price])
  results[, min_per_price := summary_stats["Min."]]
  results[, q1_per_price := summary_stats["1st Qu."]]
  results[, med_per_price := summary_stats["Median"]]
  results[, mean_per_price := summary_stats["Mean"]]
  results[, q3_per_price := summary_stats["3rd Qu."]]
  results[, max_per_price := summary_stats["Max."]]
  results[, sd_per_price := sd(data[symbol == best_match, per_price])]
  results[, skew_per_price := skew(data[symbol == best_match, per_price])]
  results[, kurt_per_price := kurtosi(data[symbol == best_match, per_price])]
  
  # Summary statistics for numeric volume difference
  summary_stats <- summary(data[symbol == best_match, num_vol])
  results[, min_num_vol := summary_stats["Min."]]
  results[, q1_num_vol := summary_stats["1st Qu."]]
  results[, med_num_vol := summary_stats["Median"]]
  results[, mean_num_vol := summary_stats["Mean"]]
  results[, q3_num_vol := summary_stats["3rd Qu."]]
  results[, max_num_vol := summary_stats["Max."]]
  results[, sd_num_vol := sd(data[symbol == best_match, num_vol])]
  results[, skew_num_vol := skew(data[symbol == best_match, num_vol])]
  results[, kurt_num_vol := kurtosi(data[symbol == best_match, num_vol])]
  
  # Summary statistics for the % volume difference
  summary_stats <- summary(data[symbol == best_match, per_vol])
  results[, min_per_vol := summary_stats["Min."]]
  results[, q1_per_vol := summary_stats["1st Qu."]]
  results[, med_per_vol := summary_stats["Median"]]
  results[, mean_per_vol := summary_stats["Mean"]]
  results[, q3_per_vol := summary_stats["3rd Qu."]]
  results[, max_per_vol := summary_stats["Max."]]
  results[, sd_per_vol := sd(data[symbol == best_match, per_vol])]
  results[, skew_per_vol := skew(data[symbol == best_match, per_vol])]
  results[, kurt_per_vol := kurtosi(data[symbol == best_match, per_vol])]
  
  return(results)
}

# Read the ban list
ban_file <- "/run/media/jon/TOSHIBA/RESEARCH_DATA/short_sale_taq/ban_list.csv"
ban_list <- read.csv(ban_file)

# Set the dates as POSIXct dates
ban_list$AddDate <- as.POSIXct(strptime(ban_list$AddDate, "%Y-%m-%d"))
ban_list$ExpDate <- as.POSIXct(strptime(ban_list$ExpDate, "%Y-%m-%d"))

# Create a data.table and set the keys
ban_list <- data.table(ban_list)
setkey(ban_list, Ticker, Exchange)


# Add columns to daily results for identifying which stocks were banned, and the start and end period
daily_results[, banned := FALSE]

# Mark each banned stock as TRUE, with the add and expiry date
setkey(daily_results, symbol)
for (ticker in ban_list$Ticker)
{
  daily_results[ticker, banned := TRUE]
  daily_results[ticker, AddDate := ban_list[Ticker == ticker, AddDate]]
  daily_results[ticker, ExpDate := ban_list[Ticker == ticker, ExpDate]]
}

# Update the key indexes to include banned
setkey(daily_results, time, symbol, type, ShortSale, banned)


# Create a new data table containing the data that symbols will be matched on:
# The mean price, mean volume, and market cap for each symbol for all types
symbol_match_data <- daily_results[, list(matched=FALSE, mean_price=mean(mean_price), mean_vol=mean(mean_vol), market_cap=mean(market_cap)), 
                                   by="time,symbol,banned,AddDate"]
setkey(symbol_match_data,time,banned,matched,symbol)

# Get the list of banned symbols
banned <- symbol_match_data[banned == TRUE, list(symbol=unique(symbol))]$symbol

# Set N, the number or random permutations of the banned symbols list to generate, recommended 2 * number banned symbols
#N <- 2 * NROW(banned)
N <- 150

# For each random permutation of the banned stocks keep a record of the best match for each banned symbol
for (i in 1:N)
{
  permutation <- sample(banned)

  # Find a matching un-banned stock based on pre-ban price, volume, and market cap for each symbol of the permutation
  for (ban_symbol in permutation)
  {
    # The date the symbol was first listed
    start_date <- symbol_match_data[symbol == ban_symbol, time][1]
    
    # The date the symbol was banned
    ban_date <- symbol_match_data[symbol == ban_symbol, AddDate][1]
    
    # The number of days the symbol was banned (based on the days it was listed)
    days_banned <- NROW(symbol_match_data[symbol == ban_symbol & time >= start_date & time < ban_date])
    
    # Get the list of dates the symbol was listed as banned
    dates <- symbol_match_data[symbol == ban_symbol & time >= start_date & time < ban_date, time]  
    
    # Get a list of candidate stocks which occurr at the same time
    temp <- symbol_match_data[J(dates,FALSE,FALSE),list(n=NROW(time)), nomatch=0, by="symbol"]
    setkey(temp, symbol, n)
    candidates <- temp[n == days_banned]$symbol

    # Store the candidates in the data table that will contain the differences
    total_differences <- symbol_match_data[time %in% dates & symbol %in% candidates, list(time, symbol, mean_price, mean_vol, market_cap)]
    setkey(total_differences,symbol)
    
    # Get a vector of the mean price and volume and market cap for the banned symbol for each day
    ban_prices <- symbol_match_data[J(dates,TRUE,FALSE,ban_symbol)]$mean_price  
    ban_vols <- symbol_match_data[J(dates,TRUE,FALSE,ban_symbol)]$mean_vol
    ban_caps <- symbol_match_data[J(dates,TRUE,FALSE,ban_symbol)]$market_cap
    
    # Calculate the differences between the banned stock and each candidate stock
    total_differences[, num_price := (mean_price - ban_prices), by="symbol"]
    total_differences[, per_price := Delt(ban_prices, mean_price, type='arithmetic'), by="symbol"]
    total_differences[, num_vol := (mean_vol - ban_vols), by="symbol"]
    total_differences[, per_vol := Delt(ban_vols, mean_vol, type='arithmetic'), by="symbol"]
    total_differences[, num_cap := (market_cap - ban_caps), by="symbol"]
    total_differences[, per_cap := Delt(ban_caps, market_cap, type='arithmetic'), by="symbol"]
    
    # Calculate the sum of absolute % differences
    sum_differences <- total_differences[, list(sum_price=sum(abs(per_price)), sum_vol=sum(abs(per_vol)), sum_cap=sum(abs(per_cap))), by="symbol"]
    
    # Calculate the mean of the % price, volume, and market cap sums
    sum_differences[, sum_mean := rowMeans(.SD), by="symbol"]
    
    # The best match is the symbol with the lowest average min price, volume, and market cap
    setkey(sum_differences, sum_mean)
    best_match <- sum_differences[1,symbol]
    
    results <- data.table(symbol = ban_symbol, 
                          match = best_match, 
                          frequency = 1, 
                          sum_price = sum_differences[1,sum_price],
                          sum_vol = sum_differences[1,sum_vol],
                          sum_cap = sum_differences[1,sum_cap],
                          sum_mean = sum_differences[1,sum_mean])
    
    # Add the results to the matched symbol histogram, update the frequency of the match
    if (exists("matched_hist"))
    {
      if (NROW(matched_hist[J(ban_symbol,best_match), nomatch=0]) > 0)
      {
        matched_hist[J(ban_symbol,best_match), frequency := (frequency + 1)]
      }
      else
      {
        matched_hist <- rbind(matched_hist, results)
      }
    }
    else
    {
      # Create the initial matched histogram, set the keys
      matched_hist <- results
    }
    
    # Mark the best matched symbol used as matched
    symbol_match_data[symbol == best_match, matched := TRUE]
    setkey(symbol_match_data,time,banned,matched,symbol)
    setkey(matched_hist, symbol, match)
    
    # Clean up temporary data.tables
    #rm(dates, candidates, total_differences, sum_differences, results)
  }
    
  # Reset the list of matched symbols for the next permutation of the banned symbols
  symbol_match_data[,matched := FALSE]
  setkey(symbol_match_data,time,banned,matched,symbol)
}



# For each matched symbol resolve any conflicts and produce the final_matched table
for (entry in matched_hist[, list(symbol=unique(symbol))]$symbol)
{
  # For each symbol use the match with the highest frequency out of all other symbols matched to that match
  for (candidate in matched_hist[entry][order(-frequency)]$match)
  {
    # If the candidate symbol has a frequency equal to N then it was matched everytime
    if (matched_hist[J(entry,candidate)]$frequency == N)
    {
      final_match <- candidate
      break
    }
    # If the candidate symbol is only matched to the current entry, it is the best match
    if (NROW(matched_hist[match == candidate]) == 1)
    {
      final_match <- candidate
      break
    }
    # If the candidate symbol has the highest frequency out of all other symbols also matched to it, it is the best match
    # TODO ADD SUPPORT FOR HANDLING THE CASE WHEN TWO OR MORE SYMBOLS HAVE SAME FREQUENCY
    if (matched_hist[match == candidate][order(-frequency), symbol][1] == entry)
    {
      final_match <- candidate
      break
    }
  }
  
  if (exists("final_match"))
  {
    # Get the results for the final match for the current symbol
    results <- matched_hist[J(entry,final_match)]
  }
  else
  {
    # There was no match at all, mark the entry as NA, and try and match it manually
    # This should not happen, try increasing the size of N and re-running the script
    results <- data.table(symbol=entry, match=NA, frequency=NA, sum_price=NA, sum_vol=NA, sum_cap=NA, sum_mean=NA)
  }
  
  # Add the final matched results to list of all matched symbols
  if (exists("final_matched"))
  {
    final_matched <- rbind(final_matched, results)
  }
  else
  {
    final_matched <- results
  }
  
  rm(final_match)
  gc()
}


# Match any remaining stocks that have been unmatched, these are usually statistical outliers
# from the matching process. However, if there are a relatively large number of stocks that 
# are not matched try increasing N (number of permutations)
for (entry in final_matched[is.na(match), symbol])
{
  # For each unmatched symbol used the best match that hasn't already been matched
  for (candidate in matched_hist[entry][order(-frequency)]$match)
  {
    if (NROW(final_matched[match == entry]) == 0)
    {
      final_match <- candidate
      break
    }
  }
  
  if (exists("final_match"))
  {
    # Get the results for the final match for the current symbol
    results <- matched_hist[J(entry,final_match)]
  }
  else
  {
    # There was no match at all, mark the entry as NA, and try and match it manually
    # This should not happen, try increasing the size of N and re-running the script
    results <- data.table(symbol=entry, match=NA, frequency=NA, sum_price=NA, sum_vol=NA, sum_cap=NA, sum_mean=NA)
  }
  
  # Update the unmatched entry to have a match
  final_matched[symbol == entry, match := results[, match]]
  final_matched[symbol == entry, frequency := results[, frequency]]
  final_matched[symbol == entry, sum_price := results[, sum_price]]
  final_matched[symbol == entry, sum_vol := results[, sum_vol]]
  final_matched[symbol == entry, sum_cap := results[, sum_cap]]
  final_matched[symbol == entry, sum_mean := results[, sum_mean]]
  rm(final_match)
  gc()
}

setkey(final_matched, symbol, match)

# Add the AddDate and ExpDate
# TODO: this should be done prior
for (ticker in ban_list$Ticker)
{
  final_matched[ticker, AddDate := ban_list[Ticker == ticker, AddDate]]
  final_matched[ticker, ExpDate := ban_list[Ticker == ticker, ExpDate]]
}


