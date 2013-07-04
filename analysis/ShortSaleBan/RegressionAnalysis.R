###############################################################################
#
# Perform regression analysis on the daily market attributes for a banned stock
# and its corresponding matching unbanned stock to measure market quality.
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

# Create a dataset from the aggregate daily and time-weighted data to perform regression 
# analysis on for stocks added to the original banlist. 
# Daily_data, is the daily summary statistics, time_weight_data, which contains the time-weighted 
# RQS data and matches which contains the matches.
gen_dataset_orig <- function(daily_data, time_weight_data, matches)
{
  # Get the list of banned symbols
  ban_date <- as_date("2008-09-19")
  exp_date <- as_date("2008-10-09")
  banned <- matches[AddDate == ban_date, symbol]
  
  # Types of HFT and Non-HFT demanding/supplying liquidity
  types <- data.table(type=c("HFT_D","HFT_S", "NHFT_D", "NHFT_S"), 
                      v1=c("HH", "HH", "NN", "NN"), 
                      v2=c("HN", "NH", "NH", "HN"))
  setkey(types, type)
  
  # ShortSale status 
  short_status <- c(TRUE,FALSE)
  
  
  # For each banned symbol get the match and calculate the differences of the variables
  for (ban_symbol in banned)
  {
    for (entry in types[, type])
    {
      for (status in short_status)
      {
        matched <- matches[ban_symbol]$match
        
        daily_volume <- daily_data[symbol == ban_symbol & type %in% types[entry][,c(v1,v2)] & ShortSale == status,
                             list(type=entry, shortsale=status, ban_sum_vol=sum(sum_vol)), by="time,symbol"]
        
        daily_volume <- merge(daily_volume, 
                              daily_data[symbol == matched & type %in% types[entry][,c(v1,v2)] & ShortSale == status,
                                         list(match_sum_vol=sum(sum_vol)), by="time"],
                              by="time")
        
        
        # Calculate the dependent variable, RQS
        daily_RQS <- time_weight_data[symbol == ban_symbol, 
                                      list(NRQS_ban=(2 * mean_NRQHS), NQRQS_ban=(2 * mean_NQRQHS)), by="time"]
        daily_RQS <- merge(daily_RQS,
                           time_weight_data[symbol == matched, 
                                            list(NRQS_match=(2 * mean_NRQHS), NQRQS_match=(2 * mean_NQRQHS)), by="time"])
        
        
        # Calculate the difference for the current symbol, store results
        result <- daily_volume[, list(sum_vol=(ban_sum_vol - match_sum_vol)), by="time,symbol,type,shortsale"]
        
        result <- merge(result,
                        daily_RQS[, list(NRQS=(NRQS_ban - NRQS_match), 
                                         NQRQS=(NQRQS_ban - NQRQS_match)), by="time"],
                        by="time")
        
        if (exists("results"))
        {
          results <- rbind(results, result)
        }
        else
        {
          results <- result
        }
        setkey(results, time, symbol, type, shortsale)
      }
    }
  }
  
  # Mark all entries during the ban period with 1
  results[, ban_dummy := 0]
  results[time >= ban_date & time < exp_date, ban_dummy := 1]
  
  return(results)
}



# Create regression analysis datasets for the mean daily volume, and RQS
dataset_original <- gen_dataset_orig(daily_results, time_weight_results, final_matched)


# Perform regression analysis for the orginal banned stocks with RQS as the dependent variable
NRQS_model=lm(NRQS~HFT_D_sum_vol+HFT_S_sum_vol+ban_dummy, dataset_original)
NQRQS_model=lm(NQRQS~HFT_D_sum_vol+HFT_S_sum_vol+ban_dummy, dataset_original)

# NRQS Regression Analysis for Original Banned Stocks
summary(NRQS_model)

#NQRQS Regression Analysis for Oringal Banned Stocks")
summary(NQRQS_model)
