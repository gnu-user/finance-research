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

# Wrapper for the built-in lm() method to display the actual formula used in the
# case where the formula passed to lm() is represented as a string in a variable
lm <- function(...)
{
  mf <- match.call()
  mf[[1]] <- quote(stats::lm)
  env <- parent.frame()
  mf$formula <- eval(mf$formula, env)
  eval(mf, env)
}


# Create a dataset from the aggregate daily and time-weighted data to perform regression 
# analysis on stocks added to the original banlist. 
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
                              by="time", all.x=TRUE)
        daily_volume[is.na(ban_sum_vol), ban_sum_vol := 0]
        daily_volume[is.na(match_sum_vol), match_sum_vol := 0]
        
        
        # Calculate the dependent variable, RQS
        daily_RQS <- time_weight_data[symbol == ban_symbol, 
                                      list(NRQS_ban=(2 * mean_NRQHS), NQRQS_ban=(2 * mean_NQRQHS)), by="time,symbol"]
        daily_RQS <- merge(daily_RQS,
                           time_weight_data[symbol == matched, 
                                            list(NRQS_match=(2 * mean_NRQHS), NQRQS_match=(2 * mean_NQRQHS)), by="time"], 
                           by="time", all.x=TRUE)
        daily_RQS[is.na(NRQS_ban), NRQS_ban := 0]
        daily_RQS[is.na(NQRQS_ban), NQRQS_ban := 0]
        daily_RQS[is.na(NRQS_match), NRQS_match := 0]
        daily_RQS[is.na(NQRQS_match), NQRQS_match := 0]
        
        
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
  results[, ssb := 0]
  results[time >= ban_date & time < exp_date, ssb := 1]
  
  return(results)
}


# Create a data table similar to what was used for the shackling shortsellers paper
gen_shackling_table <- function(daily_data, time_weight_data, matches)
{
  # Get the list of banned symbols
  ban_date <- as_date("2008-09-19")
  exp_date <- as_date("2008-10-09")
  banned <- matches[AddDate == ban_date, symbol]
  
  # For each banned symbol get the match and calculate the differences of the variables
  for (ban_symbol in banned)
  {
    matched <- matches[ban_symbol]$match
    
    # The daily trading volume
    daily_volume <- daily_data[symbol == ban_symbol,
                               list(ban_sum_vol=sum(sum_vol)), by="time,symbol"]
    
    daily_volume <- merge(daily_volume, 
                          daily_data[symbol == matched,
                                     list(match_sum_vol=sum(sum_vol)), by="time"],
                          by="time", all.x=TRUE)
    daily_volume[is.na(ban_sum_vol), ban_sum_vol := 0]
    daily_volume[is.na(match_sum_vol), match_sum_vol := 0]
    
    
    # Relative range
    relative_range <- daily_data[symbol == ban_symbol,
                                 list(ban_rel_range=mean(rel_range)), by="time,symbol"]
    
    relative_range <- merge(relative_range, 
                            daily_data[symbol == matched,
                                       list(match_rel_range=mean(rel_range)), by="time"],
                            by="time", all.x=TRUE)
    relative_range[is.na(ban_rel_range), ban_rel_range := 0]
    relative_range[is.na(match_rel_range), match_rel_range := 0]
    
    
    # Market cap
    market_cap <- daily_data[symbol == ban_symbol,
                             list(ban_market_cap=mean(market_cap)), by="time,symbol"]
    
    market_cap <- merge(market_cap, 
                        daily_data[symbol == matched,
                                   list(match_market_cap=mean(market_cap)), by="time"],
                            by="time", all.x=TRUE)
    market_cap[is.na(ban_market_cap), ban_market_cap := 0]
    market_cap[is.na(match_market_cap), match_market_cap := 0]
    
    
    # Calculate the daily weighted average price (WAP) by volume (VWAP)
    # TODO this should be part of the daily aggregate results rather than having to derive it from rel_range
    daily_VWAP <- daily_data[symbol == ban_symbol,
                             list(ban_vwap=(mean(rel_range) / (mean(max_price) - mean(min_price)))^-1), by="time,symbol"]
    
    daily_VWAP <- merge(daily_VWAP, 
                        daily_data[symbol == matched,
                                   list(match_vwap=(mean(rel_range) / (mean(max_price) - mean(min_price)))^-1), by="time"],
                        by="time", all.x=TRUE)
    daily_VWAP[is.na(ban_vwap) | is.nan(ban_vwap), ban_vwap := 0]
    daily_VWAP[is.na(match_vwap) | is.nan(match_vwap), match_vwap := 0]
    
    
    # Calculate the dependent variable, RQS
    daily_RQS <- time_weight_data[symbol == ban_symbol, 
                                  list(NRQS_ban=(2 * mean_NRQHS), NQRQS_ban=(2 * mean_NQRQHS)), by="time,symbol"]
    daily_RQS <- merge(daily_RQS,
                       time_weight_data[symbol == matched, 
                                        list(NRQS_match=(2 * mean_NRQHS), NQRQS_match=(2 * mean_NQRQHS)), by="time"], by="time", all.x=TRUE)
    daily_RQS[is.na(NRQS_ban), NRQS_ban := 0]
    daily_RQS[is.na(NQRQS_ban), NQRQS_ban := 0]
    daily_RQS[is.na(NRQS_match), NRQS_match := 0]
    daily_RQS[is.na(NQRQS_match), NQRQS_match := 0]
    
    
    # Calculate the dependent variable, RES
    daily_RES <- daily_data[symbol == ban_symbol,
                            list(NRES_ban=mean(mean_NRES), NQRES_ban=mean(mean_NQRES)), by="time,symbol"]
    
    daily_RES <- merge(daily_RES, 
                       daily_data[symbol == matched,
                                  list(NRES_match=mean(mean_NRES), NQRES_match=mean(mean_NQRES)), by="time"],
                       by="time", all.x=TRUE)
    daily_RES[is.na(NRES_ban), NRES_ban := 0]
    daily_RES[is.na(NQRES_ban), NQRES_ban := 0]
    daily_RES[is.na(NRES_match), NRES_match := 0]
    daily_RES[is.na(NQRES_match), NQRES_match := 0]
    
    
    # Calculate the depndent variable, RPI5
    daily_RPI5 <- daily_data[symbol == ban_symbol,
                             list(NRPI5_ban=mean(mean_NRPI5), NQRPI5_ban=mean(mean_NQRPI5)), by="time,symbol"]
    
    daily_RPI5 <- merge(daily_RPI5, 
                        daily_data[symbol == matched,
                                   list(NRPI5_match=mean(mean_NRPI5), NQRPI5_match=mean(mean_NQRPI5)), by="time"],
                        by="time", all.x=TRUE)    
    daily_RPI5[is.na(NRPI5_ban), NRPI5_ban := 0]
    daily_RPI5[is.na(NQRPI5_ban), NQRPI5_ban := 0]
    daily_RPI5[is.na(NRPI5_match), NRPI5_match := 0]
    daily_RPI5[is.na(NQRPI5_match), NQRPI5_match := 0]
    
    
    # Calculate the difference for the current symbol, store results
    result <- daily_volume[, list(sum_vol=(ban_sum_vol - match_sum_vol)), by="time,symbol"]
    
    result <- merge(result,
                    relative_range[, list(rel_range=(ban_rel_range - match_rel_range)), by="time"],
                    by="time", all.x=TRUE)

    result <- merge(result,
                    market_cap[, list(market_cap=(ban_market_cap - match_market_cap)), by="time"],
                    by="time", all.x=TRUE)
    
    result <- merge(result,
                    daily_VWAP[, list(vwap=(ban_vwap - match_vwap)), by="time"],
                    by="time", all.x=TRUE)
    
    result <- merge(result,
                    daily_RQS[, list(NRQS=(NRQS_ban - NRQS_match), 
                                     NQRQS=(NQRQS_ban - NQRQS_match)), by="time"],
                    by="time", all.x=TRUE)
    
    result <- merge(result,
                    daily_RES[, list(NRES=(NRES_ban - NRES_match), 
                                     NQRES=(NQRES_ban - NQRES_match)), by="time"],
                    by="time", all.x=TRUE)
    
    result <- merge(result,
                    daily_RPI5[, list(NRPI5=(NRPI5_ban - NRPI5_match), 
                                     NQRPI5=(NQRPI5_ban - NQRPI5_match)), by="time"],
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
  
  # Mark all entries during the ban period with 1
  results[, ssb := 0]
  results[time >= ban_date & time < exp_date, ssb := 1]
  
  return(results)
}


# Create a table for regression analysis with RQS as dependent and daily volume as independent
gen_RQS_table <- function(dataset)
{  
  # The initial table of each date and symbol
  results <- dataset[, list(symbol=unique(symbol)), by=time]
  setkey(results, time, symbol)
  
  # Set the volume for hft and non-hft, shortsale and non-shortsale data
  results <- merge(results, 
                   dataset[type == "HFT_D" & shortsale == FALSE, list(hft_d=sum_vol), by="time,symbol"], all.x=TRUE)
  results <- merge(results, 
                   dataset[type == "HFT_D" & shortsale == TRUE, list(hft_d_short=sum_vol), by="time,symbol"], all.x=TRUE)
  results <- merge(results, 
                   dataset[type == "HFT_S" & shortsale == FALSE, list(hft_s=sum_vol), by="time,symbol"], all.x=TRUE)
  results <- merge(results, 
                   dataset[type == "HFT_S" & shortsale == TRUE, list(hft_s_short=sum_vol), by="time,symbol"], all.x=TRUE)
  results <- merge(results, 
                   dataset[type == "NHFT_D" & shortsale == FALSE, list(nhft_d=sum_vol), by="time,symbol"], all.x=TRUE)
  results <- merge(results, 
                   dataset[type == "NHFT_D" & shortsale == TRUE, list(nhft_d_short=sum_vol), by="time,symbol"], all.x=TRUE)
  results <- merge(results, 
                   dataset[type == "NHFT_S" & shortsale == FALSE, list(nhft_s=sum_vol), by="time,symbol"], all.x=TRUE)
  results <- merge(results, 
                   dataset[type == "NHFT_S" & shortsale == TRUE, list(nhft_s_short=sum_vol), by="time,symbol"], all.x=TRUE)
  
  # Set the RQS and ban dummy
  results <- merge(results, 
                   unique(dataset[, list(NRQS, NQRQS, ssb), by="time,symbol"]))

  # Set any entries that are NA to 0
  results[is.na(hft_d), hft_d := 0]
  results[is.na(hft_d_short), hft_d_short := 0]
  results[is.na(hft_s), hft_s := 0]
  results[is.na(hft_s_short), hft_s_short := 0]
  results[is.na(nhft_d), nhft_d := 0]
  results[is.na(nhft_d_short), nhft_d_short := 0]
  results[is.na(nhft_s), nhft_s := 0]
  results[is.na(nhft_s_short), nhft_s_short := 0]
  results[is.na(NRQS), NRQS := 0]
  results[is.na(NQRQS), NQRQS := 0]
  
  return(results)
}



# Performs regression analysis similar to the shackling shortsellers paper
# for RQS, RES, RPI5, and RVOL as the dependent variables given the formula
# for the indepedent variables and the dataset provided. The results of the 
# regression are saved to text files in the directory provided.
shackling_regression <- function(dataset, formula, rvol_formula, directory)
{
  # Regression analysis with RQS as dependent
  NRQS_model=lm(formula = as.formula(paste("NRQS", formula, sep=" ~ ")), 
                data = dataset)
  
  NQRQS_model=lm(formula = as.formula(paste("NQRQS", formula, sep=" ~ ")), 
                 data = dataset)
  
  # Save the regression results
  write("NATIONAL RQS REGRESSION\n==================================================", 
         file = paste(directory, "RQS_Regression.txt", sep="/"))
  capture.output(summary(NRQS_model), file = paste(directory, "RQS_Regression.txt", sep="/"), append = TRUE)
  write("\n\n\n\n\n\n\nNASDAQ RQS REGRESSION\n==================================================", 
        file = paste(directory, "RQS_Regression.txt", sep="/"), append = TRUE)
  capture.output(summary(NQRQS_model), file = paste(directory, "RQS_Regression.txt", sep="/"), append = TRUE)
  
  
  # Regression analysis with RES as dependent
  NRES_model=lm(formula = as.formula(paste("NRES", formula, sep=" ~ ")), 
                data = dataset)
  
  NQRES_model=lm(formula = as.formula(paste("NQRES", formula, sep=" ~ ")), 
                 data = dataset)
  
  # Save the regression results
  write("NATIONAL RES REGRESSION\n==================================================", 
        file = paste(directory, "RES_Regression.txt", sep="/"))
  capture.output(summary(NRES_model), file = paste(directory, "RES_Regression.txt", sep="/"), append = TRUE)
  write("\n\n\n\n\n\n\nNASDAQ RES REGRESSION\n==================================================", 
        file = paste(directory, "RES_Regression.txt", sep="/"), append = TRUE)
  capture.output(summary(NQRES_model), file = paste(directory, "RES_Regression.txt", sep="/"), append = TRUE)
  
  
  # Regression analysis with RPI5 as dependent
  NRPI5_model=lm(formula = as.formula(paste("NRPI5", formula, sep=" ~ ")), 
                data = dataset)
  
  NQRPI5_model=lm(formula = as.formula(paste("NQRPI5", formula, sep=" ~ ")), 
                 data = dataset)
  
  # Save the regression results
  write("NATIONAL RPI5 REGRESSION\n==================================================", 
        file = paste(directory, "RPI5_Regression.txt", sep="/"))
  capture.output(summary(NRPI5_model), file = paste(directory, "RPI5_Regression.txt", sep="/"), append = TRUE)
  write("\n\n\n\n\n\n\nNASDAQ RPI5 REGRESSION\n==================================================", 
        file = paste(directory, "RPI5_Regression.txt", sep="/"), append = TRUE)
  capture.output(summary(NQRPI5_model), file = paste(directory, "RPI5_Regression.txt", sep="/"), append = TRUE)
  
  
  # Regression analysis with RVOL as dependent
  RVOL_model=lm(formula = as.formula(paste("rel_range", rvol_formula, sep=" ~ ")), 
                 data = dataset)
  
  # Save the regression results
  write("RVOL (RELATIVE RANGE) REGRESSION\n==================================================", 
        file = paste(directory, "RVOL_Regression.txt", sep="/"))
  capture.output(summary(RVOL_model), file = paste(directory, "RVOL_Regression.txt", sep="/"), append = TRUE)
}


  
# Create regression analysis datasets for the mean daily volume, and RQS
dataset_original <- gen_dataset_orig(daily_results, time_weight_results, final_matched)


# Perform regression analysis for the orginal banned stocks with RQS as dependent and volume as independent
# 
# RQS = constant + hft_d + hft_d_short + hft_s + hft_s_short + nhft_d + nhft_d_short + nhft_s + nhft_s_short 
#     + hft_d * ssb + hft_d_short * ssb + hft_s * ssb + hft_s_short * ssb + nhft_d * ssb + nhft_d_short * ssb 
#     + nhft_s * ssb + nhft_s_short * ssb + ssb + epsilon
#
RQS_table <- gen_RQS_table(dataset_original)

NRQS_model=lm(NRQS~hft_d + hft_d_short + hft_s + hft_s_short + nhft_d + nhft_d_short + nhft_s + nhft_s_short 
              + hft_d * ssb + hft_d_short * ssb + hft_s * ssb + hft_s_short * ssb + nhft_d * ssb 
              + nhft_d_short * ssb + nhft_s * ssb + nhft_s_short * ssb + ssb, RQS_table)

NQRQS_model=lm(NQRQS~hft_d + hft_d_short + hft_s + hft_s_short + nhft_d + nhft_d_short + nhft_s + nhft_s_short 
               + hft_d * ssb + hft_d_short * ssb + hft_s * ssb + hft_s_short * ssb + nhft_d * ssb 
               + nhft_d_short * ssb + nhft_s * ssb + nhft_s_short * ssb + ssb, RQS_table)


# NRQS Regression Analysis for Original Banned Stocks
summary(NRQS_model)

#NQRQS Regression Analysis for Original Banned Stocks")
summary(NQRQS_model)






# Start by performing regression analysis similar to the shackling shortsellers paper
# for RQS, RES, RPI5, and RVOL as the dependent variables
#
# DEPENDENT = ssb + factor(symbol) + market_cap + sum_vol + rel_range + vwap 
#             + hft_d + hft_d * ssb + hft_s + hft_s * ssb + nhft_d + nhft_d * ssb
#             + nhft_s + nhft_s * ssb
#
shackling_table <- gen_shackling_table(daily_results, time_weight_results, final_matched)
setkey(shackling_table, time, symbol)

# Combine the shackling table the the HFT data from the RQS table
shackling_table <- merge(shackling_table,
                         RQS_table[, 
                                   list(hft_d, hft_d_short, hft_s, hft_s_short, nhft_d, nhft_d_short, nhft_s, nhft_s_short), 
                                   by="time,symbol"])


