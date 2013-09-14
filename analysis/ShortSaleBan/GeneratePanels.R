###############################################################################
#
# Generates the panel data, which can be used to perform regressions or for
# further analysis.
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


# Create a dataset from the aggregate daily and time-weighted data to perform regression 
# analysis on all stocks, stocks that are banned will be identified by their corresponding match
# this dataset contains the original values rather than the difference between banned and matched
gen_dataset_hft_all <- function(daily_data, time_weight_data, matches)
{
  banned <- matches[, symbol]
  symbols <- unique(daily_data[, symbol])
  
  # Types of HFT and Non-HFT demanding/supplying liquidity
  types <- data.table(type=c("HFT_D","HFT_S", "NHFT_D", "NHFT_S"), 
                      v1=c("HH", "HH", "NN", "NN"), 
                      v2=c("HN", "NH", "NH", "HN"),
                      v3=c("B", "S", "B", "S"))
  setkey(types, type)
  
  # ShortSale status, NA means HFT/NHFT for both non-shortsale and shortsale
  short_status <- c(TRUE,FALSE, NA)
  
  
  # For each banned symbol get the match and calculate the differences of the variables
  for (cur_symbol in symbols)
  {
    for (entry in types[, type])
    {
      for (status in short_status)
      { 
        if (is.na(status))
        {
          daily_volume <- daily_data[symbol == cur_symbol & type %in% types[entry][,c(v1,v2)],
                                     list(type=entry, shortsale=status, sum_vol=sum(sum_vol)), by="time,symbol"]
        }
        else if (status)
        {
          daily_volume <- daily_data[symbol == cur_symbol & buysell == types[entry][,v3] & type %in% types[entry][,c(v1,v2)] & ShortSale == status,
                                     list(type=entry, shortsale=status, sum_vol=sum(sum_vol)), by="time,symbol"]
        }
        else
        {
          daily_volume <- daily_data[symbol == cur_symbol & type %in% types[entry][,c(v1,v2)] & ShortSale == status,
                                     list(type=entry, shortsale=status, sum_vol=sum(sum_vol)), by="time,symbol"]          
        }
        
        
        if (exists("results"))
        {
          results <- rbind(results, daily_volume)
        }
        else
        {
          results <- daily_volume
        }
        setkey(results, time, symbol, type, shortsale)
      }
    }
  }
  
  
  # For each symbol mark all entries during the ban period with a 1
  results[, ssb := 0]
  for (cur_symbol in banned)
  {
    ban_date <- matches[symbol == cur_symbol, AddDate]
    exp_date <- matches[symbol == cur_symbol, ExpDate]
    
    results[symbol == cur_symbol & time >= ban_date & time < exp_date, ssb := 1]
  }
  
  return(results)
}


# Create a table for regression analysis with HFT daily volume as independent
gen_HFT_table_all <- function(dataset)
{  
  # The initial table of each date and symbol
  results <- dataset[, list(symbol=unique(symbol)), by=time]
  setkey(results, time, symbol)
  
  
  # Set the volume for hft, shortsale and non-shortsale data
  results <- merge(results, 
                   dataset[type == "HFT_D" & is.na(shortsale), list(hft_d=sum_vol), by="time,symbol"], all.x=TRUE)
  results <- merge(results, 
                   dataset[type == "HFT_S" & is.na(shortsale), list(hft_s=sum_vol), by="time,symbol"], all.x=TRUE)
  
  
  # Calculate the combined volume for supply and demand
  results[!is.na(hft_d) & !is.na(hft_s), hft_a := (hft_d + hft_s)]
  results[!is.na(hft_d) & is.na(hft_s), hft_a := hft_d]
  results[is.na(hft_d) & !is.na(hft_s), hft_a := hft_s]
  
  
  results <- merge(results, 
                   dataset[type == "HFT_D" & shortsale == FALSE, list(hft_d_long=sum_vol), by="time,symbol"], all.x=TRUE)
  results <- merge(results, 
                   dataset[type == "HFT_S" & shortsale == FALSE, list(hft_s_long=sum_vol), by="time,symbol"], all.x=TRUE)
  
  # Calculate the combined volume for supply and demand
  results[!is.na(hft_d_long) & !is.na(hft_s_long), hft_a_long := (hft_d_long + hft_s_long)]
  results[!is.na(hft_d_long) & is.na(hft_s_long), hft_a_long := hft_d_long]
  results[is.na(hft_d_long) & !is.na(hft_s_long), hft_a_long := hft_s_long]
  
  
  results <- merge(results, 
                   dataset[type == "HFT_D" & shortsale == TRUE, list(hft_d_short=sum_vol), by="time,symbol"], all.x=TRUE)
  results <- merge(results, 
                   dataset[type == "HFT_S" & shortsale == TRUE, list(hft_s_short=sum_vol), by="time,symbol"], all.x=TRUE)
  
  # Calculate the combined volume for supply and demand
  results[!is.na(hft_d_short) & !is.na(hft_s_short), hft_a_short := (hft_d_short + hft_s_short)]
  results[!is.na(hft_d_short) & is.na(hft_s_short), hft_a_short := hft_d_short]
  results[is.na(hft_d_short) & !is.na(hft_s_short), hft_a_short := hft_s_short]
  
  
  
  
  # Set the volume for non-hft, shortsale and non-shortsale data
  results <- merge(results, 
                   dataset[type == "NHFT_D" & is.na(shortsale), list(nhft_d=sum_vol), by="time,symbol"], all.x=TRUE)
  results <- merge(results, 
                   dataset[type == "NHFT_S" & is.na(shortsale), list(nhft_s=sum_vol), by="time,symbol"], all.x=TRUE)
  
  # Calculate the combined volume for supply and demand
  results[!is.na(nhft_d) & !is.na(nhft_s), nhft_a := (nhft_d + nhft_s)]
  results[!is.na(nhft_d) & is.na(nhft_s), nhft_a := nhft_d]
  results[is.na(nhft_d) & !is.na(nhft_s), nhft_a := nhft_s]
  
  
  results <- merge(results, 
                   dataset[type == "NHFT_D" & shortsale == FALSE, list(nhft_d_long=sum_vol), by="time,symbol"], all.x=TRUE)
  results <- merge(results, 
                   dataset[type == "NHFT_S" & shortsale == FALSE, list(nhft_s_long=sum_vol), by="time,symbol"], all.x=TRUE)
  
  # Calculate the combined volume for supply and demand
  results[!is.na(nhft_d_long) & !is.na(nhft_s_long), nhft_a_long := (nhft_d_long + nhft_s_long)]
  results[!is.na(nhft_d_long) & is.na(nhft_s_long), nhft_a_long := nhft_d_long]
  results[is.na(nhft_d_long) & !is.na(nhft_s_long), nhft_a_long := nhft_s_long]
  
  
  results <- merge(results, 
                   dataset[type == "NHFT_D" & shortsale == TRUE, list(nhft_d_short=sum_vol), by="time,symbol"], all.x=TRUE)
  
  results <- merge(results, 
                   dataset[type == "NHFT_S" & shortsale == TRUE, list(nhft_s_short=sum_vol), by="time,symbol"], all.x=TRUE)
  
  
  # Calculate the combined volume for supply and demand
  results[!is.na(nhft_d_short) & !is.na(nhft_s_short), nhft_a_short := (nhft_d_short + nhft_s_short)]
  results[!is.na(nhft_d_short) & is.na(nhft_s_short), nhft_a_short := nhft_d_short]
  results[is.na(nhft_d_short) & !is.na(nhft_s_short), nhft_a_short := nhft_s_short]
  
  
  
  
  # Add additional columns for the natural log of the HFT data, where it is computed as follows:
  # if value positive: ln(value), if value negative: -1 * ln(abs(value)), if value 0: 0
  results[hft_d > 0, hft_d_ln := log(hft_d)]
  results[hft_d == 0, hft_d_ln := 0]
  results[hft_d < 0, hft_d_ln := (-1 * log(abs(hft_d)))]
  
  results[hft_s > 0, hft_s_ln := log(hft_s)]
  results[hft_s == 0, hft_s_ln := 0]
  results[hft_s < 0, hft_s_ln := (-1 * log(abs(hft_s)))]
  
  results[hft_a > 0, hft_a_ln := log(hft_a)]
  results[hft_a == 0, hft_a_ln := 0]
  results[hft_a < 0, hft_a_ln := (-1 * log(abs(hft_a)))]
  
  
  # Add additional columns for the natural log of the NHFT data, where it is computed as follows:
  # if value positive: ln(value), if value negative: -1 * ln(abs(value)), if value 0: 0
  results[nhft_d > 0, nhft_d_ln := log(nhft_d)]
  results[nhft_d == 0, nhft_d_ln := 0]
  results[nhft_d < 0, nhft_d_ln := (-1 * log(abs(nhft_d)))]
  
  results[nhft_s > 0, nhft_s_ln := log(nhft_s)]
  results[nhft_s == 0, nhft_s_ln := 0]
  results[nhft_s < 0, nhft_s_ln := (-1 * log(abs(nhft_s)))]
  
  results[nhft_a > 0, nhft_a_ln := log(nhft_a)]
  results[nhft_a == 0, nhft_a_ln := 0]
  results[nhft_a < 0, nhft_a_ln := (-1 * log(abs(nhft_a)))]
  
  
  
  
  # Add additional columns for the natural log of the HFT LONG data, where it is computed as follows:
  # if value positive: ln(value), if value negative: -1 * ln(abs(value)), if value 0: 0
  results[hft_d_long > 0, hft_d_long_ln := log(hft_d_long)]
  results[hft_d_long == 0, hft_d_long_ln := 0]
  results[hft_d_long < 0, hft_d_long_ln := (-1 * log(abs(hft_d_long)))]
  
  results[hft_s_long > 0, hft_s_long_ln := log(hft_s_long)]
  results[hft_s_long == 0, hft_s_long_ln := 0]
  results[hft_s_long < 0, hft_s_long_ln := (-1 * log(abs(hft_s_long)))]
  
  results[hft_a_long > 0, hft_a_long_ln := log(hft_a_long)]
  results[hft_a_long == 0, hft_a_long_ln := 0]
  results[hft_a_long < 0, hft_a_long_ln := (-1 * log(abs(hft_a_long)))]
  
  
  # Add additional columns for the natural log of the NHFT data, where it is computed as follows:
  # if value positive: ln(value), if value negative: -1 * ln(abs(value)), if value 0: 0
  results[nhft_d_long > 0, nhft_d_long_ln := log(nhft_d_long)]
  results[nhft_d_long == 0, nhft_d_long_ln := 0]
  results[nhft_d_long < 0, nhft_d_long_ln := (-1 * log(abs(nhft_d_long)))]
  
  results[nhft_s_long > 0, nhft_s_long_ln := log(nhft_s_long)]
  results[nhft_s_long == 0, nhft_s_long_ln := 0]
  results[nhft_s_long < 0, nhft_s_long_ln := (-1 * log(abs(nhft_s_long)))]
  
  results[nhft_a_long > 0, nhft_a_long_ln := log(nhft_a_long)]
  results[nhft_a_long == 0, nhft_a_long_ln := 0]
  results[nhft_a_long < 0, nhft_a_long_ln := (-1 * log(abs(nhft_a_long)))]
  
  
  
  
  # Add additional columns for the natural log of the HFT SHORT data, where it is computed as follows:
  # if value positive: ln(value), if value negative: -1 * ln(abs(value)), if value 0: 0
  results[hft_d_short > 0, hft_d_short_ln := log(hft_d_short)]
  results[hft_d_short == 0, hft_d_short_ln := 0]
  results[hft_d_short < 0, hft_d_short_ln := (-1 * log(abs(hft_d_short)))]
  
  results[hft_s_short > 0, hft_s_short_ln := log(hft_s_short)]
  results[hft_s_short == 0, hft_s_short_ln := 0]
  results[hft_s_short < 0, hft_s_short_ln := (-1 * log(abs(hft_s_short)))]
  
  results[hft_a_short > 0, hft_a_short_ln := log(hft_a_short)]
  results[hft_a_short == 0, hft_a_short_ln := 0]
  results[hft_a_short < 0, hft_a_short_ln := (-1 * log(abs(hft_a_short)))]
  
  
  # Add additional columns for the natural log of the NHFT data, where it is computed as follows:
  # if value positive: ln(value), if value negative: -1 * ln(abs(value)), if value 0: 0
  results[nhft_d_short > 0, nhft_d_short_ln := log(nhft_d_short)]
  results[nhft_d_short == 0, nhft_d_short_ln := 0]
  results[nhft_d_short < 0, nhft_d_short_ln := (-1 * log(abs(nhft_d_short)))]
  
  results[nhft_s_short > 0, nhft_s_short_ln := log(nhft_s_short)]
  results[nhft_s_short == 0, nhft_s_short_ln := 0]
  results[nhft_s_short < 0, nhft_s_short_ln := (-1 * log(abs(nhft_s_short)))]
  
  results[nhft_a_short > 0, nhft_a_short_ln := log(nhft_a_short)]
  results[nhft_a_short == 0, nhft_a_short_ln := 0]
  results[nhft_a_short < 0, nhft_a_short_ln := (-1 * log(abs(nhft_a_short)))]
  
  
  
  
  return(results)
}


# Create a data table for performing the main set of regressions for all symbols, this dataset 
# contains the original values rather than the difference between banned and matched
gen_regression_table_all <- function(daily_data, time_weight_data, matches)
{
  banned <- matches[, symbol]
  symbols <- unique(daily_data[, symbol])
  
  # Aggregate the market cap so there is only one entry for each symbol for each day
  # as the market cap is not affected by the type of trade
  agr_market_cap <- daily_data[, list(market_cap=mean(market_cap)), by="time,symbol"]
  
  # Calculate the quartile bounds
  quartiles <- quantile(agr_market_cap[, market_cap], names=FALSE)
  Q0 <- quartiles[1]
  Q1 <- quartiles[2]
  Q2 <- quartiles[3]
  Q3 <- quartiles[4]
  Q4 <- quartiles[5]
  
  
  # For each symbol calculate the various market quality measures
  for (cur_symbol in symbols)
  {
    matched <- matches[cur_symbol]$match
    
    # Determine the quartile of the current symbol based on market cap
    cur_cap <- mean(daily_data[symbol == cur_symbol, 
                               list(market_cap=mean(market_cap)), by="time"]$market_cap)
    
    if (cur_cap >= Q0 && cur_cap <= Q1)
    {
      quartile <- "Q1"
    }
    else if (cur_cap > Q1 && cur_cap <= Q2)
    {
      quartile <- "Q2"
    }
    else if (cur_cap > Q2 && cur_cap <= Q3)
    {
      quartile <- "Q3"
    }
    else if (cur_cap > Q3 && cur_cap <= Q4)
    {
      quartile <- "Q4"
    }
    else
    {
      quartile <- NA
    }
    
    
    # The daily trading volume
    daily_volume <- daily_data[symbol == cur_symbol,
                               list(match=matched, Q=quartile, sum_vol=sum(sum_vol)), by="time,symbol"]
    
    
    # Relative range
    relative_range <- daily_data[symbol == cur_symbol,
                                 list(rel_range=mean(rel_range)), by="time,symbol"]
    
    
    # Market cap
    market_cap <- daily_data[symbol == cur_symbol,
                             list(market_cap=mean(market_cap)), by="time,symbol"]
    
    
    # Calculate the daily weighted average price (WAP) by volume (VWAP)
    # TODO this should be part of the daily aggregate results rather than having to derive it from rel_range
    daily_VWAP <- daily_data[symbol == cur_symbol,
                             list(vwap=(mean(rel_range) / (mean(max_price) - mean(min_price)))^-1), by="time,symbol"]
    
    
    # Calculate the dependent variable, RQS
    daily_RQS <- time_weight_data[symbol == cur_symbol, 
                                  list(NRQS=(2 * mean_NRQHS), NQRQS=(2 * mean_NQRQHS)), by="time,symbol"]
    
    # Calculate the dependent variable, RES
    daily_RES <- daily_data[symbol == cur_symbol,
                            list(NRES=mean(mean_NRES), NQRES=mean(mean_NQRES)), by="time,symbol"]
    
    
    # Calculate the depndent variable, RPI5
    daily_RPI5 <- daily_data[symbol == cur_symbol,
                             list(NRPI5=mean(mean_NRPI5), NQRPI5=mean(mean_NQRPI5)), by="time,symbol"]
    
    
    # Calculate the difference for the current symbol, store results
    result <- daily_volume[, list(sum_vol), by="time,symbol,match,Q"]
    
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
  
  
  # For each symbol mark all entries during the ban period with a 1
  results[, ssb := 0]
  for (cur_symbol in banned)
  {
    ban_date <- matches[symbol == cur_symbol, AddDate]
    exp_date <- matches[symbol == cur_symbol, ExpDate]
    
    results[symbol == cur_symbol & time >= ban_date & time < exp_date, ssb := 1]
  }
  
  return(results)
}




# The file containing the banned symbols matches
final_matched_file <- "/home/jon/Source/RESEARCH/finance-research/analysis/ShortSaleBan/data/final_matched.csv"

# The file containing the daily aggregate results
daily_results_file <- "/home/jon/Source/RESEARCH/finance-research/analysis/ShortSaleBan/data/daily_results.csv"

# The file containing the time weighted results
time_weight_results_file <- "/home/jon/Source/RESEARCH/finance-research/analysis/ShortSaleBan/data/time_weight_results.csv"

# The directory to store the panel data
data_dir <- "/home/jon/Source/RESEARCH/finance-research/analysis/ShortSaleBan/panel_data/"




# Load the banned symbol matches
final_matched <- fread(final_matched_file)

# Convert columns to the appropriate type
final_matched[, AddDate := as.POSIXct(AddDate, origin = "1970-01-01")]
final_matched[, ExpDate := as.POSIXct(ExpDate, origin = "1970-01-01")]

setkey(final_matched, symbol, match)


# Load the daily aggregate results
daily_results <- fread(daily_results_file)

# Convert columns to the appropriate type
daily_results[, time := as.POSIXct(strptime(time, "%Y-%m-%d"))]
daily_results[, symbol := as.factor(symbol)]
daily_results[, type := as.factor(type)]
daily_results[, ShortSale := as.logical(ShortSale)]

setkey(daily_results, time, symbol, buysell, type, ShortSale)


# Load the time weight results
time_weight_results <- fread(time_weight_results_file)

# Convert columns to the appropriate type
time_weight_results[, time := as.POSIXct(strptime(time, "%Y-%m-%d"))]
time_weight_results[, symbol := as.factor(symbol)]

setkey(time_weight_results, time, symbol)




# Create regression datasets for the mean daily volume, and RQS
dataset_all_nodiff <- gen_dataset_hft_all(daily_results, time_weight_results, final_matched)
HFT_table_all <- gen_HFT_table_all(dataset_all_nodiff)


# Create the dataset for all symbols with the original values rather than the difference
regression_table_all <- gen_regression_table_all(daily_results, time_weight_results, final_matched)
setkey(regression_table_all, time, symbol, Q)


# Combine the regression table and the HFT table into a single panel for all symbols
setkey(HFT_table_all, time, symbol)
regression_table_all <-  merge(regression_table_all, HFT_table_all)




# Write the panel data for all symbols to file
write.csv(regression_table_all, paste(data_dir, "regression_all_panel.csv", sep="/"), row.names = FALSE)

