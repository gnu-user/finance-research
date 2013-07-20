###############################################################################
#
# Perform analysis of the data using correlations, covariances, and various
# plots to better understand the data.
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


# Create a dataset for the HFT and NHFT daily total dollar volume for all stocks.
# Daily_data, is the daily summary statistics, time_weight_data, which contains the time-weighted 
gen_vol_dataset <- function(daily_data)
{
  # Get the list of all symbols in the dataset
  symbols <- unique(daily_data[, symbol])
  
  # Types of HFT and Non-HFT demanding/supplying liquidity
  types <- data.table(type=c("HFT_D","HFT_S", "NHFT_D", "NHFT_S"), 
                      v1=c("HH", "HH", "NN", "NN"), 
                      v2=c("HN", "NH", "NH", "HN"))
  setkey(types, type)
  
  # ShortSale status 
  short_status <- c(TRUE,FALSE)
  
  
  # For each symbol get the HFT and NHFT total daily dollar volume
  for (cur_symbol in symbols)
  {
    for (entry in types[, type])
    {
      daily_volume <- daily_data[symbol == cur_symbol & type %in% types[entry][,c(v1,v2)],
                                 list(type=entry, sum_vol=sum(sum_vol)), by="time,symbol"]
      
      daily_volume[is.na(sum_vol), sum_vol := 0]
      
      
      if (exists("results"))
      {
        results <- rbind(results, daily_volume)
      }
      else
      {
        results <- daily_volume
      }
      setkey(results, time, symbol, type)
    }
  }
  
  return(results)
}


# Create a table for correlation analysis with HFT and NHFT daily total dollar volume for all stocks.
gen_vol_table <- function(dataset)
{  
  # The initial table of each date and symbol
  results <- dataset[, list(symbol=unique(symbol)), by=time]
  setkey(results, time, symbol)
  
  # Set the volume for hft and non-hft for supply, demand, and all
  results <- merge(results, 
                   dataset[type == "HFT_D", list(hft_d=sum_vol), by="time,symbol"], all.x=TRUE)
  results <- merge(results, 
                   dataset[type == "HFT_S", list(hft_s=sum_vol), by="time,symbol"], all.x=TRUE)
  
  # Set any entries that are NA to 0
  results[is.na(hft_d), hft_d := 0]
  results[is.na(hft_s), hft_s := 0]
  
  # Calculate the combined volume for HFT supply and demand
  results[, hft_a := (hft_d + hft_s)]
  
  
  results <- merge(results, 
                   dataset[type == "NHFT_D", list(nhft_d=sum_vol), by="time,symbol"], all.x=TRUE)
  results <- merge(results, 
                   dataset[type == "NHFT_S", list(nhft_s=sum_vol), by="time,symbol"], all.x=TRUE)
  
  # Set any entries that are NA to 0
  results[is.na(nhft_d), nhft_d := 0]
  results[is.na(nhft_s), nhft_s := 0]
  
  # Calculate the combined volume for NHFT supply and demand
  results[, nhft_a := (nhft_d + nhft_s)]
  
  return(results)
}


# Create a data table with daily market quality measures for all stocks
gen_measures_table <- function(daily_data, time_weight_data)
{
  # Get the list of all symbols in the dataset
  symbols <- unique(daily_data[, symbol])
  
  # For each banned symbol get the match and calculate the differences of the variables
  for (cur_symbol in symbols)
  {
    # The total daily trading volume
    daily_volume <- daily_data[symbol == cur_symbol,
                               list(sum_vol=sum(sum_vol)), by="time,symbol"]
    
    daily_volume[is.na(sum_vol), sum_vol := 0]
    
    
    # Relative range
    relative_range <- daily_data[symbol == cur_symbol,
                                 list(rel_range=mean(rel_range)), by="time,symbol"]

    relative_range[is.na(rel_range), rel_range := 0]

    
    # Market cap
    market_cap <- daily_data[symbol == cur_symbol,
                             list(market_cap=mean(market_cap)), by="time,symbol"]
    
    market_cap[is.na(market_cap), market_cap := 0]
    
    
    # Calculate the daily weighted average price (WAP) by volume (VWAP)
    # TODO this should be part of the daily aggregate results rather than having to derive it from rel_range
    daily_VWAP <- daily_data[symbol == cur_symbol,
                             list(vwap=(mean(rel_range) / (mean(max_price) - mean(min_price)))^-1), by="time,symbol"]
    
    daily_VWAP[is.na(vwap) | is.nan(vwap), vwap := 0]
    
    
    # Calculate the dependent variable, RQS
    daily_RQS <- time_weight_data[symbol == cur_symbol, 
                                  list(NRQS=(2 * mean_NRQHS), NQRQS=(2 * mean_NQRQHS)), by="time,symbol"]

    daily_RQS[is.na(NRQS), NRQS := 0]
    daily_RQS[is.na(NQRQS), NQRQS := 0]
    
    
    # Calculate the dependent variable, RES
    daily_RES <- daily_data[symbol == cur_symbol,
                            list(NRES=mean(mean_NRES), NQRES=mean(mean_NQRES)), by="time,symbol"]
    
    daily_RES[is.na(NRES), NRES := 0]
    daily_RES[is.na(NQRES), NQRES := 0]
    
    
    # Calculate the depndent variable, RPI5
    daily_RPI5 <- daily_data[symbol == cur_symbol,
                             list(NRPI5=mean(mean_NRPI5), NQRPI5=mean(mean_NQRPI5)), by="time,symbol"]
      
    daily_RPI5[is.na(NRPI5), NRPI5 := 0]
    daily_RPI5[is.na(NQRPI5), NQRPI5 := 0]
    
    
    # Calculate the difference for the current symbol, store results
    result <- daily_volume[, list(sum_vol), by="time,symbol"]
    
    result <- merge(result,
                    relative_range[, list(rel_range), by="time"],
                    by="time", all.x=TRUE)
    
    result <- merge(result,
                    market_cap[, list(market_cap), by="time"],
                    by="time", all.x=TRUE)
    
    result <- merge(result,
                    daily_VWAP[, list(vwap), by="time"],
                    by="time", all.x=TRUE)
    
    result <- merge(result,
                    daily_RQS[, list(NRQS, NQRQS), by="time"],
                    by="time", all.x=TRUE)
    
    result <- merge(result,
                    daily_RES[, list(NRES, NQRES), by="time"],
                    by="time", all.x=TRUE)
    
    result <- merge(result,
                    daily_RPI5[, list(NRPI5, NQRPI5), by="time"],
                    by="time", all.x=TRUE)
    
    
    if (exists("results"))
    {
      results <- rbind(results, result)
    }
    else
    {
      results <- result
    }
    setkey(results, time, symbol)
  }
  
  return(results)
}




# Create the HFT and NHFT volume table
vol_dataset <- gen_vol_dataset(daily_results)
vol_table <- gen_vol_table(vol_dataset)


# Create the  market measures table
market_measures <- gen_measures_table(daily_results, time_weight_results)


# Combine the volume and market measures table and run the correlation
cor_table <- merge(vol_table,
                   market_measures[, .SD, by="time,symbol"])

cor_matrix <- cor(cor_table[, list(hft_d, hft_s, hft_a, sum_vol, rel_range, market_cap, vwap, NRQS, NQRQS, NRES, NQRES, NRPI5, NQRPI5)])
write.csv(cor_matrix, "/home/jon/Source/RESEARCH/finance-research/analysis/ShortSaleBan/results/correlations.csv")
