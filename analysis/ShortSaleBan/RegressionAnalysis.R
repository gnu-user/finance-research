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
library(ggplot2)
library(plm)
#library(quantreg)
#library(robustbase)

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


# Simple helper function that removes the factor entries from the regression summary output
rm_factors <- function(output)
{
  clean_output <- capture.output(output)
  clean_output <- gsub('factor\\(symbol\\).+$', NA, clean_output)
  
  return(clean_output[!is.na(clean_output)])
}


# Simple helper function that removes NA entries that are a result of the merge operation from 
# the global variable summary table. NA values that are strings (e.g. "NA") as a result of 
# independent variables listed in the regression summary being NA are not removed.
rm_NAs <- function(columns=c())
{ 
  if (length(columns) == 0)
  {
    columns <- colnames(reg_summary)[3:NCOL(reg_summary)]
  }
  
  # Set any actual NA entries from the merge operation as empty strings
  for (column in columns)
  {
    reg_summary[is.na(eval(as.symbol(column))), column := "", with=FALSE]
  }
}


# Updates the global variable summary table containing the results of the regressions for each independent variable
# Adds the results of the current regression to the summary table from previous regressions
update_summary_table <- function(model, reg_name)
{
  regexp <- '\\(*([a-zA-Z_:-]+?)\\)*\\s+(<*\\s*[0-9\\.\\sNAe-]+)\\s+(<*\\s*[0-9\\.\\sNAe-]+)\\s+(<*\\s*[0-9\\.\\sNAe-]+)\\s+(<*\\s*[0-9\\.\\sNAe-]+)\\s{0,1}((.+)$|$)'
  #regexp_miss <- '\\s+\\(([0-9]+)\\s+observations\\sdeleted.+$'
  regexp_adj_r2 <- '.*?Adj\\.\\s+R-Squared\\s:\\s+([0-9\\.]+)\\s+$' 
  
  
  for (row in rm_factors(summary(model)))
  {
    match <- str_match(row, regexp)
    
    # Get the coefficient, the estimate and the significance
    if (!is.na(match[1]))
    {
      coefficient <- match[2]
      coefficient_err <- paste(coefficient, "Std. Error", sep=" ")
      estimate <- match[3]
      error <- match[4]
      significance <- match[7]
      
      if (coefficient == "Intercept")
      {
        result <- data.table(row=c(1,1), entry=c(coefficient, coefficient_err))
      }
      else
      {
        result <- data.table(row=c(2,2), entry=c(coefficient, coefficient_err))
      }
      result[, reg_name := c(paste(estimate, significance, sep=" "), error), with=FALSE]
      
      if (exists("results"))
      {
        results <- rbind(results, result)
      }
      else
      {
        results <- result
      }
    }

    
    # Get the number of observations and the number deleted (due to NAs)
    #match <- str_match(row, regexp_miss)
    
    #if (!is.na(match[1]))
    #{
    #  num_obs <- nobs(model)
    #  deleted <- match[2]
    #  
    #  result <- data.table(row=c(3, 3), entry=c("N", "N (Deleted)"))
    #  result[, reg_name := c(num_obs, deleted), with=FALSE]
    #  
    #  results <- rbind(results, result)
    #}

    
    # Get the Adjusted R2
    match <- str_match(row, regexp_adj_r2)
    
    if (!is.na(match[1]))
    {
      result <- data.table(row=c(4), entry=c("Adj. R-Squared"))
      result[, reg_name := c(match[2]), with=FALSE]
      
      results <- rbind(results, result)
    }
  }
  
  
  # Merge the current results with the existing regression summary
  if (NROW(reg_summary) > 0)
  {
    setkey(reg_summary, row, entry)
    setkey(results, row, entry)
  
    reg_summary <<- merge(reg_summary,
                          results, all=TRUE)
  }
  else
  {
    setkey(results, row, entry)
    reg_summary <<- results
  }
}


# Create a dataset from the aggregate daily and time-weighted data to perform regression 
# analysis on all stocks added to the banlist. 
# Daily_data, is the daily summary statistics, time_weight_data, which contains the time-weighted 
# RQS data and matches which contains the matches.
gen_dataset_hft <- function(daily_data, time_weight_data, matches)
{
  # Get the list of all banned symbols
  #ban_date <- as_date("2008-09-19")
  #exp_date <- as_date("2008-10-09")
  banned <- matches[, symbol]
  
  # Types of HFT and Non-HFT demanding/supplying liquidity
  types <- data.table(type=c("HFT_D","HFT_S", "NHFT_D", "NHFT_S"), 
                      v1=c("HH", "HH", "NN", "NN"), 
                      v2=c("HN", "NH", "NH", "HN"),
                      v3=c("B", "S", "B", "S"))
  setkey(types, type)
  
  # ShortSale status, NA means HFT/NHFT for both non-shortsale and shortsale
  short_status <- c(TRUE,FALSE, NA)
  
  
  # For each banned symbol get the match and calculate the differences of the variables
  for (ban_symbol in banned)
  {
    for (entry in types[, type])
    {
      for (status in short_status)
      {
        matched <- matches[ban_symbol]$match
        
        if (is.na(status))
        {
          daily_volume <- daily_data[symbol == ban_symbol & type %in% types[entry][,c(v1,v2)],
                                     list(type=entry, shortsale=status, ban_sum_vol=sum(sum_vol)), by="time,symbol"]
          
          daily_volume <- merge(daily_volume, 
                                daily_data[symbol == matched & type %in% types[entry][,c(v1,v2)],
                                           list(match_sum_vol=sum(sum_vol)), by="time"],
                                by="time", all.x=TRUE)
          
        }
        else if (status)
        {
          daily_volume <- daily_data[symbol == ban_symbol & buysell == types[entry][,v3] & type %in% types[entry][,c(v1,v2)] & ShortSale == status,
                               list(type=entry, shortsale=status, ban_sum_vol=sum(sum_vol)), by="time,symbol"]
          
          daily_volume <- merge(daily_volume, 
                                daily_data[symbol == matched & buysell == types[entry][,v3] & type %in% types[entry][,c(v1,v2)] & ShortSale == status,
                                           list(match_sum_vol=sum(sum_vol)), by="time"],
                                by="time", all.x=TRUE)
        }
        else
        {
          daily_volume <- daily_data[symbol == ban_symbol & type %in% types[entry][,c(v1,v2)] & ShortSale == status,
                                     list(type=entry, shortsale=status, ban_sum_vol=sum(sum_vol)), by="time,symbol"]
          
          daily_volume <- merge(daily_volume, 
                                daily_data[symbol == matched & type %in% types[entry][,c(v1,v2)] & ShortSale == status,
                                           list(match_sum_vol=sum(sum_vol)), by="time"],
                                by="time", all.x=TRUE)          
        }
        
        
        #daily_volume[is.na(ban_sum_vol), ban_sum_vol := 0]
        #daily_volume[is.na(match_sum_vol), match_sum_vol := 0]
        
        
        # Calculate the dependent variable, RQS
        daily_RQS <- time_weight_data[symbol == ban_symbol, 
                                      list(NRQS_ban=(2 * mean_NRQHS), NQRQS_ban=(2 * mean_NQRQHS)), by="time,symbol"]
        daily_RQS <- merge(daily_RQS,
                           time_weight_data[symbol == matched, 
                                            list(NRQS_match=(2 * mean_NRQHS), NQRQS_match=(2 * mean_NQRQHS)), by="time"], 
                           by="time", all.x=TRUE)
        #daily_RQS[is.na(NRQS_ban), NRQS_ban := 0]
        #daily_RQS[is.na(NQRQS_ban), NQRQS_ban := 0]
        #daily_RQS[is.na(NRQS_match), NRQS_match := 0]
        #daily_RQS[is.na(NQRQS_match), NQRQS_match := 0]
        
        
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
  
  
  # For each symbol mark all entries during the ban period with a 1
  results[, ssb := 0]
  for (ban_symbol in banned)
  {
    ban_date <- matches[symbol == ban_symbol, AddDate]
    exp_date <- matches[symbol == ban_symbol, ExpDate]
    
    results[symbol == ban_symbol & time >= ban_date & time < exp_date, ssb := 1]
  }
  
  return(results)
}


# Create a data table for performing the main set of regressions
gen_regression_table <- function(daily_data, time_weight_data, matches)
{
  # Get the list of banned symbols
  #ban_date <- as_date("2008-09-19")
  #exp_date <- as_date("2008-10-09")
  banned <- matches[, symbol]
  
  
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
  
  
  # For each banned symbol get the match and calculate the differences of the variables
  for (ban_symbol in banned)
  {
    matched <- matches[ban_symbol]$match
    
    # Determine the quartile of the current banned symbol based on market cap
    ban_cap <- mean(daily_data[symbol == ban_symbol, 
                               list(market_cap=mean(market_cap)), by="time"]$market_cap)
    
    if (ban_cap >= Q0 && ban_cap <= Q1)
    {
      quartile <- "Q1"
    }
    else if (ban_cap > Q1 && ban_cap <= Q2)
    {
      quartile <- "Q2"
    }
    else if (ban_cap > Q2 && ban_cap <= Q3)
    {
      quartile <- "Q3"
    }
    else if (ban_cap > Q3 && ban_cap <= Q4)
    {
      quartile <- "Q4"
    }
    else
    {
      quartile <- NA
    }
    
    
    # The daily trading volume
    daily_volume <- daily_data[symbol == ban_symbol,
                               list(Q=quartile, ban_sum_vol=sum(sum_vol)), by="time,symbol"]
    
    daily_volume <- merge(daily_volume, 
                          daily_data[symbol == matched,
                                     list(match_sum_vol=sum(sum_vol)), by="time"],
                          by="time", all.x=TRUE)
    #daily_volume[is.na(ban_sum_vol), ban_sum_vol := 0]
    #daily_volume[is.na(match_sum_vol), match_sum_vol := 0]
    
    
    # Relative range
    relative_range <- daily_data[symbol == ban_symbol,
                                 list(ban_rel_range=mean(rel_range)), by="time,symbol"]
    
    relative_range <- merge(relative_range, 
                            daily_data[symbol == matched,
                                       list(match_rel_range=mean(rel_range)), by="time"],
                            by="time", all.x=TRUE)
    #relative_range[is.na(ban_rel_range), ban_rel_range := 0]
    #relative_range[is.na(match_rel_range), match_rel_range := 0]
    
    
    # Market cap
    market_cap <- daily_data[symbol == ban_symbol,
                             list(ban_market_cap=mean(market_cap)), by="time,symbol"]
    
    market_cap <- merge(market_cap, 
                        daily_data[symbol == matched,
                                   list(match_market_cap=mean(market_cap)), by="time"],
                            by="time", all.x=TRUE)
    #market_cap[is.na(ban_market_cap), ban_market_cap := 0]
    #market_cap[is.na(match_market_cap), match_market_cap := 0]
    
    
    # Calculate the daily weighted average price (WAP) by volume (VWAP)
    # TODO this should be part of the daily aggregate results rather than having to derive it from rel_range
    daily_VWAP <- daily_data[symbol == ban_symbol,
                             list(ban_vwap=(mean(rel_range) / (mean(max_price) - mean(min_price)))^-1), by="time,symbol"]
    
    daily_VWAP <- merge(daily_VWAP, 
                        daily_data[symbol == matched,
                                   list(match_vwap=(mean(rel_range) / (mean(max_price) - mean(min_price)))^-1), by="time"],
                        by="time", all.x=TRUE)
    #daily_VWAP[is.na(ban_vwap) | is.nan(ban_vwap), ban_vwap := 0]
    #daily_VWAP[is.na(match_vwap) | is.nan(match_vwap), match_vwap := 0]
    
    
    # Calculate the dependent variable, RQS
    daily_RQS <- time_weight_data[symbol == ban_symbol, 
                                  list(NRQS_ban=(2 * mean_NRQHS), NQRQS_ban=(2 * mean_NQRQHS)), by="time,symbol"]
    daily_RQS <- merge(daily_RQS,
                       time_weight_data[symbol == matched, 
                                        list(NRQS_match=(2 * mean_NRQHS), NQRQS_match=(2 * mean_NQRQHS)), by="time"], by="time", all.x=TRUE)
    #daily_RQS[is.na(NRQS_ban), NRQS_ban := 0]
    #daily_RQS[is.na(NQRQS_ban), NQRQS_ban := 0]
    #daily_RQS[is.na(NRQS_match), NRQS_match := 0]
    #daily_RQS[is.na(NQRQS_match), NQRQS_match := 0]
    
    
    # Calculate the dependent variable, RES
    daily_RES <- daily_data[symbol == ban_symbol,
                            list(NRES_ban=mean(mean_NRES), NQRES_ban=mean(mean_NQRES)), by="time,symbol"]
    
    daily_RES <- merge(daily_RES, 
                       daily_data[symbol == matched,
                                  list(NRES_match=mean(mean_NRES), NQRES_match=mean(mean_NQRES)), by="time"],
                       by="time", all.x=TRUE)
    #daily_RES[is.na(NRES_ban), NRES_ban := 0]
    #daily_RES[is.na(NQRES_ban), NQRES_ban := 0]
    #daily_RES[is.na(NRES_match), NRES_match := 0]
    #daily_RES[is.na(NQRES_match), NQRES_match := 0]
    
    
    # Calculate the depndent variable, RPI5
    daily_RPI5 <- daily_data[symbol == ban_symbol,
                             list(NRPI5_ban=mean(mean_NRPI5), NQRPI5_ban=mean(mean_NQRPI5)), by="time,symbol"]
    
    daily_RPI5 <- merge(daily_RPI5, 
                        daily_data[symbol == matched,
                                   list(NRPI5_match=mean(mean_NRPI5), NQRPI5_match=mean(mean_NQRPI5)), by="time"],
                        by="time", all.x=TRUE)    
    #daily_RPI5[is.na(NRPI5_ban), NRPI5_ban := 0]
    #daily_RPI5[is.na(NQRPI5_ban), NQRPI5_ban := 0]
    #daily_RPI5[is.na(NRPI5_match), NRPI5_match := 0]
    #daily_RPI5[is.na(NQRPI5_match), NQRPI5_match := 0]
    
    
    # Calculate the difference for the current symbol, store results
    result <- daily_volume[, list(sum_vol=(ban_sum_vol - match_sum_vol)), by="time,symbol,Q"]
    
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
  
  
  # For each symbol mark all entries during the ban period with a 1
  results[, ssb := 0]
  for (ban_symbol in banned)
  {
    ban_date <- matches[symbol == ban_symbol, AddDate]
    exp_date <- matches[symbol == ban_symbol, ExpDate]
    
    results[symbol == ban_symbol & time >= ban_date & time < exp_date, ssb := 1]
  }
  
  return(results)
}


# Create a table for regression analysis with HFT daily volume as independent
gen_HFT_table <- function(dataset)
{  
  # The initial table of each date and symbol
  results <- dataset[, list(symbol=unique(symbol)), by=time]
  setkey(results, time, symbol)
  
  
  # Set the volume for hft, shortsale and non-shortsale data
  results <- merge(results, 
                   dataset[type == "HFT_D" & is.na(shortsale), list(hft_d=sum_vol), by="time,symbol"], all.x=TRUE)
  results <- merge(results, 
                   dataset[type == "HFT_S" & is.na(shortsale), list(hft_s=sum_vol), by="time,symbol"], all.x=TRUE)
  
  # Set any entries that are NA to 0
  #results[is.na(hft_d), hft_d := 0]
  #results[is.na(hft_s), hft_s := 0]
  
  # Calculate the combined volume for supply and demand
  results[!is.na(hft_d) & !is.na(hft_s), hft_a := (hft_d + hft_s)]
  results[!is.na(hft_d) & is.na(hft_s), hft_a := hft_d]
  results[is.na(hft_d) & !is.na(hft_s), hft_a := hft_s]
  
  
  results <- merge(results, 
                   dataset[type == "HFT_D" & shortsale == FALSE, list(hft_d_long=sum_vol), by="time,symbol"], all.x=TRUE)
  results <- merge(results, 
                   dataset[type == "HFT_S" & shortsale == FALSE, list(hft_s_long=sum_vol), by="time,symbol"], all.x=TRUE)
  
  # Set any entries that are NA to 0
  #results[is.na(hft_d_long), hft_d_long := 0]
  #results[is.na(hft_s_long), hft_s_long := 0]
  
  # Calculate the combined volume for supply and demand
  results[!is.na(hft_d_long) & !is.na(hft_s_long), hft_a_long := (hft_d_long + hft_s_long)]
  results[!is.na(hft_d_long) & is.na(hft_s_long), hft_a_long := hft_d_long]
  results[is.na(hft_d_long) & !is.na(hft_s_long), hft_a_long := hft_s_long]
  
  
  results <- merge(results, 
                   dataset[type == "HFT_D" & shortsale == TRUE, list(hft_d_short=sum_vol), by="time,symbol"], all.x=TRUE)
  results <- merge(results, 
                   dataset[type == "HFT_S" & shortsale == TRUE, list(hft_s_short=sum_vol), by="time,symbol"], all.x=TRUE)
  
  # Set any entries that are NA to 0
  #results[is.na(hft_d_short), hft_d_short := 0]
  #results[is.na(hft_s_short), hft_s_short := 0]
  
  # Calculate the combined volume for supply and demand
  results[!is.na(hft_d_short) & !is.na(hft_s_short), hft_a_short := (hft_d_short + hft_s_short)]
  results[!is.na(hft_d_short) & is.na(hft_s_short), hft_a_short := hft_d_short]
  results[is.na(hft_d_short) & !is.na(hft_s_short), hft_a_short := hft_s_short]
  
  
  
  
  # Set the volume for non-hft, shortsale and non-shortsale data
  results <- merge(results, 
                   dataset[type == "NHFT_D" & is.na(shortsale), list(nhft_d=sum_vol), by="time,symbol"], all.x=TRUE)
  results <- merge(results, 
                   dataset[type == "NHFT_S" & is.na(shortsale), list(nhft_s=sum_vol), by="time,symbol"], all.x=TRUE)
  
  # Set any entries that are NA to 0
  #results[is.na(nhft_d), nhft_d := 0]
  #results[is.na(nhft_s), nhft_s := 0]
  
  # Calculate the combined volume for supply and demand
  results[!is.na(nhft_d) & !is.na(nhft_s), nhft_a := (nhft_d + nhft_s)]
  results[!is.na(nhft_d) & is.na(nhft_s), nhft_a := nhft_d]
  results[is.na(nhft_d) & !is.na(nhft_s), nhft_a := nhft_s]
  
  
  results <- merge(results, 
                   dataset[type == "NHFT_D" & shortsale == FALSE, list(nhft_d_long=sum_vol), by="time,symbol"], all.x=TRUE)
  results <- merge(results, 
                   dataset[type == "NHFT_S" & shortsale == FALSE, list(nhft_s_long=sum_vol), by="time,symbol"], all.x=TRUE)
  
  # Set any entries that are NA to 0
  #results[is.na(nhft_d_long), nhft_d_long := 0]
  #results[is.na(nhft_s_long), nhft_s_long := 0]
  
  # Calculate the combined volume for supply and demand
  results[!is.na(nhft_d_long) & !is.na(nhft_s_long), nhft_a_long := (nhft_d_long + nhft_s_long)]
  results[!is.na(nhft_d_long) & is.na(nhft_s_long), nhft_a_long := nhft_d_long]
  results[is.na(nhft_d_long) & !is.na(nhft_s_long), nhft_a_long := nhft_s_long]
  
  
  results <- merge(results, 
                   dataset[type == "NHFT_D" & shortsale == TRUE, list(nhft_d_short=sum_vol), by="time,symbol"], all.x=TRUE)

  results <- merge(results, 
                   dataset[type == "NHFT_S" & shortsale == TRUE, list(nhft_s_short=sum_vol), by="time,symbol"], all.x=TRUE)
  
  # Set any entries that are NA to 0
  #results[is.na(nhft_d_short), nhft_d_short := 0]
  #results[is.na(nhft_s_short), nhft_s_short := 0]
  
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
  
  
  # Add additional columns for the ratio of HFT to NHFT data  
  results[nhft_d != 0, hft_d_ratio := (hft_d / nhft_d)]
  results[nhft_d == 0, hft_d_ratio := 0]
  
  results[nhft_s != 0, hft_s_ratio := (hft_s / nhft_s)]
  results[nhft_s == 0, hft_s_ratio := 0]
  
  results[nhft_a != 0, hft_a_ratio := (hft_a / nhft_a)]
  results[nhft_a == 0, hft_a_ratio := 0]
  
  
  # Add additional columns for the ratio of natural logarithm HFT to NHFT
  results[nhft_d_ln != 0, hft_d_ratio_ln := (hft_d_ln / nhft_d_ln)]
  results[nhft_d_ln == 0, hft_d_ratio_ln := 0]
  
  results[nhft_s_ln != 0, hft_s_ratio_ln := (hft_s_ln / nhft_s_ln)]
  results[nhft_s_ln == 0, hft_s_ratio_ln := 0]
  
  results[nhft_a_ln != 0, hft_a_ratio_ln := (hft_a_ln / nhft_a_ln)]
  results[nhft_a_ln == 0, hft_a_ratio_ln := 0]
  
  
  
  
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
  
  
  # Add additional columns for the ratio of HFT to NHFT data  
  results[nhft_d_long != 0, hft_d_long_ratio := (hft_d_long / nhft_d_long)]
  results[nhft_d_long == 0, hft_d_long_ratio := 0]
  
  results[nhft_s_long != 0, hft_s_long_ratio := (hft_s_long / nhft_s_long)]
  results[nhft_s_long == 0, hft_s_long_ratio := 0]
  
  results[nhft_a_long != 0, hft_a_long_ratio := (hft_a_long / nhft_a_long)]
  results[nhft_a_long == 0, hft_a_long_ratio := 0]
  
  
  # Add additional columns for the ratio of natural logarithm HFT to NHFT
  results[nhft_d_long_ln != 0, hft_d_long_ratio_ln := (hft_d_long_ln / nhft_d_long_ln)]
  results[nhft_d_long_ln == 0, hft_d_long_ratio_ln := 0]
  
  results[nhft_s_long_ln != 0, hft_s_long_ratio_ln := (hft_s_long_ln / nhft_s_long_ln)]
  results[nhft_s_long_ln == 0, hft_s_long_ratio_ln := 0]
  
  results[nhft_a_long_ln != 0, hft_a_long_ratio_ln := (hft_a_long_ln / nhft_a_long_ln)]
  results[nhft_a_long_ln == 0, hft_a_long_ratio_ln := 0]
  
  
  
  
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
  
  
  # Add additional columns for the ratio of HFT to NHFT data  
  results[nhft_d_short != 0, hft_d_short_ratio := (hft_d_short / nhft_d_short)]
  results[nhft_d_short == 0, hft_d_short_ratio := 0]
  
  results[nhft_s_short != 0, hft_s_short_ratio := (hft_s_short / nhft_s_short)]
  results[nhft_s_short == 0, hft_s_short_ratio := 0]
  
  results[nhft_a_short != 0, hft_a_short_ratio := (hft_a_short / nhft_a_short)]
  results[nhft_a_short == 0, hft_a_short_ratio := 0]
  
  
  # Add additional columns for the ratio of natural logarithm HFT to NHFT
  results[nhft_d_short_ln != 0, hft_d_short_ratio_ln := (hft_d_short_ln / nhft_d_short_ln)]
  results[nhft_d_short_ln == 0, hft_d_short_ratio_ln := 0]
  
  results[nhft_s_short_ln != 0, hft_s_short_ratio_ln := (hft_s_short_ln / nhft_s_short_ln)]
  results[nhft_s_short_ln == 0, hft_s_short_ratio_ln := 0]
  
  results[nhft_a_short_ln != 0, hft_a_short_ratio_ln := (hft_a_short_ln / nhft_a_short_ln)]
  results[nhft_a_short_ln == 0, hft_a_short_ratio_ln := 0]
  
  
  
  
  # Set the RQS and ban dummy
  #results <- merge(results, 
  #                 unique(dataset[, list(NRQS, NQRQS, ssb), by="time,symbol"]))
  
  
  # Set any entries that are NA to 0
  #results[is.na(NRQS), NRQS := 0]
  #results[is.na(NQRQS), NQRQS := 0]
  
  return(results)
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




# Symetrically trims the outlier values from the regression dataset by setting
# any outlier values to NA. It does not modify the original dataset provided,
# but instead returns a copy of the dataset that has been trimmed.
trim_outliers <- function(dataset, trim=0.05, columns=c())
{
  trim_dataset <- copy(dataset)
  
  if (length(columns) == 0)
  {
    columns <- colnames(trim_dataset)[4:NCOL(trim_dataset)]
  }
  
  # Symetrically trim any outlier values from the dataset
  for (column in columns)
  {
    quartiles <- quantile(trim_dataset[, column, with=FALSE], probs=c(trim, 1.0 - trim), na.rm=TRUE, names=FALSE)
    l_outlier <- quartiles[1]
    r_outlier <- quartiles[2]
    
    # Set any outlier entries as NA
    trim_dataset[eval(as.symbol(column)) <= l_outlier, column := NA, with=FALSE]
    trim_dataset[eval(as.symbol(column)) >= r_outlier, column := NA, with=FALSE]
  }
  
  return(trim_dataset)
}


# Performs regression analysis for RQS, RES, RPI5, and RVOL as the dependent variables 
# given the formula for the indepedent variables and the dataset provided. The results 
# of the regression are saved to text files in the directory provided and the postfix 
# is added to each file name if provided.
perform_regression <- function(dataset, formula, rvol_formula, directory, subdir, hft_type, postfix)
{
  # Create the directory for storing the results if it does not already exist
  directory <- paste(directory, subdir, sep="/")
  dir.create(directory, showWarnings = FALSE, recursive = TRUE)
  
  base_name <- paste(hft_type, subdir, sep=" ")
  
  
  # Regression analysis with RQS as dependent
  NRQS_model=plm(formula = as.formula(paste("NRQS", formula, sep=" ~ ")), 
                 data = dataset, model="within", effect="twoways", index=c("time", "symbol"), na.action=na.omit)
  
  NQRQS_model=plm(formula = as.formula(paste("NQRQS", formula, sep=" ~ ")), 
                 data = dataset, model="within", effect="twoways", index=c("time", "symbol"), na.action=na.omit)
  
  # Save the regression results
  filename <- paste("RQS_Regression", postfix, ".txt", sep="")
  write("NATIONAL RQS REGRESSION\n==================================================", 
         file = paste(directory, filename, sep="/"))
  write(rm_factors(summary(NRQS_model)), file = paste(directory, filename, sep="/"), append = TRUE)
  write("\n\n\n\n\n\n\nNASDAQ RQS REGRESSION\n==================================================", 
        file = paste(directory, filename, sep="/"), append = TRUE)
  write(rm_factors(summary(NQRQS_model)), file = paste(directory, filename, sep="/"), append = TRUE)
  
  # Add the regression results to the summary
  update_summary_table(NRQS_model, paste(base_name, " NRQS", postfix, sep=""))
  update_summary_table(NQRQS_model, paste(base_name, " NQRQS", postfix, sep=""))
  
  
  # Regression analysis with RES as dependent
  NRES_model=plm(formula = as.formula(paste("NRES", formula, sep=" ~ ")), 
                data = dataset, model="within", effect="twoways", index=c("time", "symbol"), na.action=na.omit)
  
  NQRES_model=plm(formula = as.formula(paste("NQRES", formula, sep=" ~ ")), 
                 data = dataset, model="within", effect="twoways", index=c("time", "symbol"), na.action=na.omit)
  
  # Save the regression results
  filename <- paste("RES_Regression", postfix, ".txt", sep="")
  write("NATIONAL RES REGRESSION\n==================================================", 
        file = paste(directory, filename, sep="/"))
  write(rm_factors(summary(NRES_model)), file = paste(directory, filename, sep="/"), append = TRUE)
  write("\n\n\n\n\n\n\nNASDAQ RES REGRESSION\n==================================================", 
        file = paste(directory, filename, sep="/"), append = TRUE)
  write(rm_factors(summary(NQRES_model)), file = paste(directory, filename, sep="/"), append = TRUE)
  
  # Add the regression results to the summary
  update_summary_table(NRES_model, paste(base_name, " NRES", postfix, sep=""))
  update_summary_table(NQRES_model, paste(base_name, " NQRES", postfix, sep=""))
  
  
  # Regression analysis with RPI5 as dependent
  NRPI5_model=plm(formula = as.formula(paste("NRPI5", formula, sep=" ~ ")), 
                data = dataset, model="within", effect="twoways", index=c("time", "symbol"), na.action=na.omit)
  
  NQRPI5_model=plm(formula = as.formula(paste("NQRPI5", formula, sep=" ~ ")), 
                 data = dataset, model="within", effect="twoways", index=c("time", "symbol"), na.action=na.omit)
  
  # Save the regression results
  filename <- paste("RPI5_Regression", postfix, ".txt", sep="")
  write("NATIONAL RPI5 REGRESSION\n==================================================", 
        file = paste(directory, filename, sep="/"))
  write(rm_factors(summary(NRPI5_model)), file = paste(directory, filename, sep="/"), append = TRUE)
  write("\n\n\n\n\n\n\nNASDAQ RPI5 REGRESSION\n==================================================", 
        file = paste(directory, filename, sep="/"), append = TRUE)
  write(rm_factors(summary(NQRPI5_model)), file = paste(directory, filename, sep="/"), append = TRUE)
  
  # Add the regression results to the summary
  update_summary_table(NRPI5_model, paste(base_name, " NRPI5", postfix, sep=""))
  update_summary_table(NQRPI5_model, paste(base_name, " NQRPI5", postfix, sep=""))
  
  
  # Regression analysis with RVOL as dependent
  RVOL_model=plm(formula = as.formula(paste("rel_range", rvol_formula, sep=" ~ ")), 
                 data = dataset, model="within", effect="twoways", index=c("time", "symbol"), na.action=na.omit)
  
  # Save the regression results
  filename <- paste("RVOL_Regression", postfix, ".txt", sep="")
  write("RVOL (RELATIVE RANGE) REGRESSION\n==================================================", 
        file = paste(directory, filename, sep="/"))
  write(rm_factors(summary(RVOL_model)), file = paste(directory, filename, sep="/"), append = TRUE)
  
  # Add the regression results to the summary
  update_summary_table(RVOL_model, paste(base_name, " RVOL", postfix, sep=""))
}


# Performs regression analysis for RQS, RES, RPI5, and RVOL as the dependent variables 
# given the formula for the indepedent variables and the dataset provided. The results 
# of the regression are saved to text files in the directory provided and the postfix 
# is added to each file name if provided.
perform_quantile_regression <- function(dataset, formula, rvol_formula, quantile, directory, subdir, postfix)
{
  # Create the directory for storing the results if it does not already exist
  directory <- paste(directory, subdir, sep="/")
  dir.create(directory, showWarnings = FALSE, recursive = TRUE)
  
  
  # Regression analysis with RQS as dependent
  NRQS_model=rq(formula = as.formula(paste("NRQS", formula, sep=" ~ ")), 
                tau=quantile, data = dataset, na.action=na.omit)
  
  NQRQS_model=rq(formula = as.formula(paste("NQRQS", formula, sep=" ~ ")), 
                 tau=quantile, data = dataset, na.action=na.omit)
  
  # Save the regression results
  filename <- paste("RQS_Regression", postfix, ".txt", sep="")
  write("NATIONAL RQS REGRESSION\n==================================================", 
        file = paste(directory, filename, sep="/"))
  capture.output(summary(NRQS_model), file = paste(directory, filename, sep="/"), append = TRUE)
  write("\n\n\n\n\n\n\nNASDAQ RQS REGRESSION\n==================================================", 
        file = paste(directory, filename, sep="/"), append = TRUE)
  capture.output(summary(NQRQS_model), file = paste(directory, filename, sep="/"), append = TRUE)
  
  
  # Regression analysis with RES as dependent
  NRES_model=rq(formula = as.formula(paste("NRES", formula, sep=" ~ ")), 
                tau=quantile, data = dataset, na.action=na.omit)
  
  NQRES_model=rq(formula = as.formula(paste("NQRES", formula, sep=" ~ ")), 
                 tau=quantile, data = dataset, na.action=na.omit)
  
  # Save the regression results
  filename <- paste("RES_Regression", postfix, ".txt", sep="")
  write("NATIONAL RES REGRESSION\n==================================================", 
        file = paste(directory, filename, sep="/"))
  capture.output(summary(NRES_model), file = paste(directory, filename, sep="/"), append = TRUE)
  write("\n\n\n\n\n\n\nNASDAQ RES REGRESSION\n==================================================", 
        file = paste(directory, filename, sep="/"), append = TRUE)
  capture.output(summary(NQRES_model), file = paste(directory, filename, sep="/"), append = TRUE)
  
  
  # Regression analysis with RPI5 as dependent
  NRPI5_model=lm(formula = as.formula(paste("NRPI5", formula, sep=" ~ ")), 
                 tau=quantile, data = dataset, na.action=na.omit)
  
  NQRPI5_model=lm(formula = as.formula(paste("NQRPI5", formula, sep=" ~ ")), 
                  tau=quantile, data = dataset, na.action=na.omit)
  
  # Save the regression results
  filename <- paste("RPI5_Regression", postfix, ".txt", sep="")
  write("NATIONAL RPI5 REGRESSION\n==================================================", 
        file = paste(directory, filename, sep="/"))
  capture.output(summary(NRPI5_model), file = paste(directory, filename, sep="/"), append = TRUE)
  write("\n\n\n\n\n\n\nNASDAQ RPI5 REGRESSION\n==================================================", 
        file = paste(directory, filename, sep="/"), append = TRUE)
  capture.output(summary(NQRPI5_model), file = paste(directory, filename, sep="/"), append = TRUE)
  
  
  # Regression analysis with RVOL as dependent
  RVOL_model=lm(formula = as.formula(paste("rel_range", rvol_formula, sep=" ~ ")), 
                tau=quantile, data = dataset, na.action=na.omit)
  
  # Save the regression results
  filename <- paste("RVOL_Regression", postfix, ".txt", sep="")
  write("RVOL (RELATIVE RANGE) REGRESSION\n==================================================", 
        file = paste(directory, filename, sep="/"))
  capture.output(summary(RVOL_model), file = paste(directory, filename, sep="/"), append = TRUE)
}


# Creates a plot of the residuals and the dependent variable for the RQS, RES, RPI5, and RVOL regressions
plot_residuals <- function(dataset, formula, rvol_formula, directory, subdir, postfix)
{
  # Create the directory for storing the results if it does not already exist
  directory <- paste(directory, subdir, "figures", sep="/")
  dir.create(directory, showWarnings = FALSE, recursive = TRUE)
  
  
  # Regression analysis with RQS as dependent
  NRQS_model=plm(formula = as.formula(paste("NRQS", formula, sep=" ~ ")), 
                data = dataset, model="within", effect="twoways", index=c("time", "symbol"), na.action=na.exclude)
  
  NQRQS_model=plm(formula = as.formula(paste("NQRQS", formula, sep=" ~ ")), 
                 data = dataset, model="within", effect="twoways", index=c("time", "symbol"), na.action=na.exclude)
  
  
  # Save the National RQS Residual plot
  figure_data <- data.table(NRQS = NRQS_model$model[[1]], residual = NRQS_model$residuals)
  p <- ggplot(figure_data, aes(x=NRQS, y=residual)) + geom_point() + geom_hline(yintercept = 0) + geom_smooth(se = F, size=1)
  p <- p + labs(x = "NRQS", y = "Residual", title=paste("National RQS Regression", postfix, sep=""))
  
  file <- paste("National RQS Regression", postfix, ".png", sep="")
  file <- paste(directory, file, sep="/")
  ggsave(filename=file, plot=p, width=10, height=6)
  
  
  # Save the NASDAQ RQS Residual plot
  figure_data <- data.table(NQRQS = NQRQS_model$model[[1]], residual = NQRQS_model$residuals)
  p <- ggplot(figure_data, aes(x=NQRQS, y=residual)) + geom_point() + geom_hline(yintercept = 0) + geom_smooth(se = F, size=1)
  p <- p + labs(x = "NQRQS", y = "Residual", title=paste("NASDAQ RQS Regression", postfix, sep=""))
  
  file <- paste("NASDAQ RQS Regression", postfix, ".png", sep="")
  file <- paste(directory, file, sep="/")
  ggsave(filename=file, plot=p, width=10, height=6)
  
  
  
  
  # Regression analysis with RES as dependent
  NRES_model=plm(formula = as.formula(paste("NRES", formula, sep=" ~ ")), 
                data = dataset, model="within", effect="twoways", index=c("time", "symbol"), na.action=na.exclude)
  
  NQRES_model=plm(formula = as.formula(paste("NQRES", formula, sep=" ~ ")), 
                 data = dataset, model="within", effect="twoways", index=c("time", "symbol"), na.action=na.exclude)
  
  
  # Save the National RES Residual plot
  figure_data <- data.table(NRES = NRES_model$model[[1]], residual = NRES_model$residuals)
  p <- ggplot(figure_data, aes(x=NRES, y=residual)) + geom_point() + geom_hline(yintercept = 0) + geom_smooth(se = F, size=1)
  p <- p + labs(x = "NRES", y = "Residual", title=paste("National RES Regression", postfix, sep=""))
  
  file <- paste("National RES Regression", postfix, ".png", sep="")
  file <- paste(directory, file, sep="/")
  ggsave(filename=file, plot=p, width=10, height=6)
  
  
  # Save the NASDAQ RES Residual plot
  figure_data <- data.table(NQRES = NQRES_model$model[[1]], residual = NQRES_model$residuals)
  p <- ggplot(figure_data, aes(x=NQRES, y=residual)) + geom_point() + geom_hline(yintercept = 0) + geom_smooth(se = F, size=1)
  p <- p + labs(x = "NQRES", y = "Residual", title=paste("NASDAQ RES Regression", postfix, sep=""))
  
  file <- paste("NASDAQ RES Regression", postfix, ".png", sep="")
  file <- paste(directory, file, sep="/")
  ggsave(filename=file, plot=p, width=10, height=6)
  
  
  
  
  # Regression analysis with RPI5 as dependent
  NRPI5_model=plm(formula = as.formula(paste("NRPI5", formula, sep=" ~ ")), 
                 data = dataset, model="within", effect="twoways", index=c("time", "symbol"), na.action=na.exclude)
  
  NQRPI5_model=plm(formula = as.formula(paste("NQRPI5", formula, sep=" ~ ")), 
                  data = dataset, model="within", effect="twoways", index=c("time", "symbol"), na.action=na.exclude)
  
  
  # Save the National RPI5 Residual plot
  figure_data <- data.table(NRPI5 = NRPI5_model$model[[1]], residual = NRPI5_model$residuals)
  p <- ggplot(figure_data, aes(x=NRPI5, y=residual)) + geom_point() + geom_hline(yintercept = 0) + geom_smooth(se = F, size=1)
  p <- p + labs(x = "NRPI5", y = "Residual", title=paste("National RPI5 Regression", postfix, sep=""))
  
  file <- paste("National RPI5 Regression", postfix, ".png", sep="")
  file <- paste(directory, file, sep="/")
  ggsave(filename=file, plot=p, width=10, height=6)
  
  
  # Save the NASDAQ RPI5 Residual plot
  figure_data <- data.table(NQRPI5 = NQRPI5_model$model[[1]], residual = NQRPI5_model$residuals)
  p <- ggplot(figure_data, aes(x=NQRPI5, y=residual)) + geom_point() + geom_hline(yintercept = 0) + geom_smooth(se = F, size=1)
  p <- p + labs(x = "NQRPI5", y = "Residual", title=paste("NASDAQ RPI5 Regression", postfix, sep=""))
  
  file <- paste("NASDAQ RPI5 Regression", postfix, ".png", sep="")
  file <- paste(directory, file, sep="/")
  ggsave(filename=file, plot=p, width=10, height=6)
  
  
  
  
  # Regression analysis with RVOL as dependent
  RVOL_model=plm(formula = as.formula(paste("rel_range", rvol_formula, sep=" ~ ")), 
                data = dataset, model="within", effect="twoways", index=c("time", "symbol"), na.action=na.exclude)
  
  
  # Save the RVOL Residual plot
  figure_data <- data.table(RVOL = RVOL_model$model[[1]], residual = RVOL_model$residuals)
  p <- ggplot(figure_data, aes(x=RVOL, y=residual)) + geom_point() + geom_hline(yintercept = 0) + geom_smooth(se = F, size=1)
  p <- p + labs(x = "RVOL", y = "Residual", title=paste("Relative Range (RVOL) Regression", postfix, sep=""))
  
  file <- paste("RVOL Regression", postfix, ".png", sep="")
  file <- paste(directory, file, sep="/")
  ggsave(filename=file, plot=p, width=10, height=6)
}




# The root directory to store the regression results
root_dir <- "/home/jon/Source/RESEARCH/finance-research/analysis/ShortSaleBan/regressions/19-08-2013"


# The directory to store the datasets used in the regressions
data_dir <- "/home/jon/Source/RESEARCH/finance-research/analysis/ShortSaleBan/data"


# A global variable containing the summary results of the regressions, populated by gen_summary_table(...)
reg_summary <- data.table()

# Create regression analysis datasets for the mean daily volume, and RQS
dataset_all <- gen_dataset_hft(daily_results, time_weight_results, final_matched)

# Create regression analysis datasets for the mean daily volume, and RQS
dataset_all_nodiff <- gen_dataset_hft_all(daily_results, time_weight_results, final_matched)


# Perform regression analysis for the orginal banned stocks with RQS as dependent and HFT volume as independent
# 
# RQS = constant + hft_d + hft_d_short + hft_s + hft_s_short + nhft_d + nhft_d_short + nhft_s + nhft_s_short 
#     + hft_d * ssb + hft_d_short * ssb + hft_s * ssb + hft_s_short * ssb + nhft_d * ssb + nhft_d_short * ssb 
#     + nhft_s * ssb + nhft_s_short * ssb + ssb + epsilon
#
HFT_table <- gen_HFT_table(dataset_all)

HFT_table_all <- gen_HFT_table_all(dataset_all_nodiff)


NRQS_model=lm(NRQS~hft_d + hft_d_short + hft_s + hft_s_short + nhft_d + nhft_d_short + nhft_s + nhft_s_short 
              + hft_d * ssb + hft_d_short * ssb + hft_s * ssb + hft_s_short * ssb + nhft_d * ssb 
              + nhft_d_short * ssb + nhft_s * ssb + nhft_s_short * ssb + ssb, HFT_table)

NQRQS_model=lm(NQRQS~hft_d + hft_d_short + hft_s + hft_s_short + nhft_d + nhft_d_short + nhft_s + nhft_s_short 
               + hft_d * ssb + hft_d_short * ssb + hft_s * ssb + hft_s_short * ssb + nhft_d * ssb 
               + nhft_d_short * ssb + nhft_s * ssb + nhft_s_short * ssb + ssb, HFT_table)


# NRQS Regression Analysis for Original Banned Stocks
summary(NRQS_model)

#NQRQS Regression Analysis for Original Banned Stocks")
summary(NQRQS_model)





# Create the dataset for performing the main set of regressions
regression_table <- gen_regression_table(daily_results, time_weight_results, final_matched)
setkey(regression_table, time, symbol, Q)


# Combine the regression table and the HFT table
regression_table <- merge(regression_table,
                          HFT_table[, 
                                   list(hft_d, hft_s, hft_a, hft_d_long, hft_s_long, hft_a_long, hft_d_short, hft_s_short, hft_a_short, 
                                        nhft_d, nhft_s, nhft_a, nhft_d_long, nhft_s_long, nhft_a_long, nhft_d_short, nhft_s_short, nhft_a_short,
                                        hft_d_ln, hft_s_ln, hft_a_ln, 
                                        nhft_d_ln, nhft_s_ln, nhft_a_ln,
                                        hft_d_ratio, hft_s_ratio, hft_a_ratio,
                                        hft_d_ratio_ln, hft_s_ratio_ln, hft_a_ratio_ln,
                                        
                                        hft_d_long_ln, hft_s_long_ln, hft_a_long_ln, 
                                        nhft_d_long_ln, nhft_s_long_ln, nhft_a_long_ln,
                                        hft_d_long_ratio, hft_s_long_ratio, hft_a_long_ratio,
                                        hft_d_long_ratio_ln, hft_s_long_ratio_ln, hft_a_long_ratio_ln,
                                        
                                        hft_d_short_ln, hft_s_short_ln, hft_a_short_ln, 
                                        nhft_d_short_ln, nhft_s_short_ln, nhft_a_short_ln,
                                        hft_d_short_ratio, hft_s_short_ratio, hft_a_short_ratio,
                                        hft_d_short_ratio_ln, hft_s_short_ratio_ln, hft_a_short_ratio_ln), 
                                   by="time,symbol"])




# Create the dataset for all symbols with the original values rather than the difference
regression_table_all <- gen_regression_table_all(daily_results, time_weight_results, final_matched)
setkey(regression_table_all, time, symbol, Q)


# Combine the regression table and the HFT table
setkey(HFT_table_all, time, symbol)
regression_table_all <-  merge(regression_table_all, HFT_table_all)


# Execute the set of regressions for each HFT type (long + short, long, short)
hft_types <- data.table(type=c("long_short","long", "short"), 
                        var=c("\\1", "\\1_long", "\\1_short"))
setkey(hft_types, type)

# Execute the set of regressions for all quartiles and then for each quartile
reg_quartiles <- c("ALL", "Q1", "Q2", "Q3", "Q4")

for (hft_type in hft_types[, type])
{
    output_dir <- paste(root_dir, hft_type, sep="/")
    
    for (quartile in reg_quartiles)
    {
        # Get the subset of the regression table to use for the current regression
        if (quartile == "ALL")
        {
          subdir <- "default"
          postfix <- ""
          regression_data <- regression_table
          #regression_data <- trim_outliers(regression_table, 
          #                                 columns=c("NRQS", "NQRQS", "NRES", "NQRES", "NRPI5", "NQRPI5", "rel_range"))
        }
        else
        {
          subdir <- "quartiles"
          postfix <- paste(" -", quartile)
          regression_data <- regression_table[Q == quartile]
          #regression_data <- trim_outliers(regression_table[Q == quartile],
          #                                 columns=c("NRQS", "NQRQS", "NRES", "NQRES", "NRPI5", "NQRPI5", "rel_range"))
        }
    
        
        # Start by performing regression analysis similar to the shackling shortsellers paper
        # for RQS, RES, RPI5, and RVOL as the dependent variables
        #
        # DEPENDENT = ssb + factor(symbol) + market_cap + sum_vol + rel_range + vwap 
        #             + hft_d + hft_d * ssb + hft_s + hft_s * ssb + nhft_d + nhft_d * ssb
        #             + nhft_s + nhft_s * ssb
        #
        
        # The two formulas to use for the shackling shortsellers regressions
        #eqn = gsub('(hft_[a-z])', hft_types[hft_type][, var], "ssb + market_cap + sum_vol + rel_range + vwap + 
        #hft_d + hft_d * ssb + hft_s + hft_s * ssb + nhft_d + nhft_d * ssb + nhft_s + nhft_s * ssb")
        
        #eqn_RVOL = gsub('(hft_[a-z])', hft_types[hft_type][, var], "ssb + market_cap + sum_vol + vwap + 
        #hft_d + hft_d * ssb + hft_s + hft_s * ssb + nhft_d + nhft_d * ssb + nhft_s + nhft_s * ssb")
        
        #perform_regression(regression_data, eqn, eqn_RVOL, paste(output_dir, "default_regression", subdir, sep="/"), "shackling", hft_type, postfix)
        #perform_quantile_regression(regression_data, eqn, eqn_RVOL, 0.50, paste(output_dir, "quantile_regression", subdir, sep="/"), "shackling", hft_type, postfix)
        #plot_residuals(regression_data, eqn, eqn_RVOL, paste(output_dir, "default_regression", subdir, sep="/"), "shackling", postfix)
        
        
        
        
        
        # Perform several regressions, using only one HFT type in the formula at a time
        # HFT_D only
        #eqn = gsub('(hft_[a-z])', hft_types[hft_type][, var], "ssb + factor(symbol) + market_cap + sum_vol + rel_range + vwap + 
        #hft_d + hft_d * ssb")
        
        #eqn_RVOL = gsub('(hft_[a-z])', hft_types[hft_type][, var], "ssb + factor(symbol) + market_cap + sum_vol + vwap + 
        #hft_d + hft_d * ssb")
        
        #perform_regression(regression_data, eqn, eqn_RVOL, paste(output_dir, "default_regression", subdir, sep="/"), "HFT_D", hft_type, postfix)
        #perform_quantile_regression(regression_data, eqn, eqn_RVOL, 0.50, paste(output_dir, "quantile_regression", subdir, sep="/"), "HFT_D", hft_type, postfix)
        
        
        # HFT_S only
        #eqn = gsub('(hft_[a-z])', hft_types[hft_type][, var], "ssb + factor(symbol) + market_cap + sum_vol + rel_range + vwap + 
        #hft_s + hft_s * ssb")
        
        #eqn_RVOL = gsub('(hft_[a-z])', hft_types[hft_type][, var], "ssb + factor(symbol) + market_cap + sum_vol + vwap + 
        #hft_s + hft_s * ssb")
        
        #perform_regression(regression_data, eqn, eqn_RVOL, paste(output_dir, "default_regression", subdir, sep="/"), "HFT_S", hft_type, postfix)
        #perform_quantile_regression(regression_data, eqn, eqn_RVOL, 0.50, paste(output_dir, "quantile_regression", subdir, sep="/"), "HFT_S", hft_type, postfix)
        
        
        # HFT_A only
        eqn = gsub('(hft_[a-z])', hft_types[hft_type][, var], "ssb + market_cap + sum_vol + rel_range + vwap + hft_a + hft_a * ssb")
        
        eqn_RVOL = gsub('(hft_[a-z])', hft_types[hft_type][, var], "ssb + market_cap + sum_vol + vwap + hft_a + hft_a * ssb")
        
        #perform_regression(regression_data, eqn, eqn_RVOL, paste(output_dir, subdir, sep="/"), "HFT_A", hft_type, postfix)
        #perform_quantile_regression(regression_data, eqn, eqn_RVOL, 0.50, paste(output_dir, "quantile_regression", subdir, sep="/"), "HFT_A", hft_type, postfix)
        #plot_residuals(regression_data, eqn, eqn_RVOL, paste(output_dir, subdir, sep="/"), "HFT_A", postfix)
        
        
        
        
        # Perform several regressions, using only one HFT type in the formula at a time
        # NHFT_A only
        eqn = gsub('(hft_[a-z])', hft_types[hft_type][, var], "ssb + market_cap + sum_vol + rel_range + vwap + nhft_a + nhft_a * ssb")
        
        eqn_RVOL = gsub('(hft_[a-z])', hft_types[hft_type][, var], "ssb + market_cap + sum_vol + vwap + nhft_a + nhft_a * ssb")
        
        #perform_regression(regression_data, eqn, eqn_RVOL, paste(output_dir, subdir, sep="/"), "NHFT_A", hft_type, postfix)
        #perform_quantile_regression(regression_data, eqn, eqn_RVOL, 0.50, paste(output_dir, "quantile_regression", subdir, sep="/"), "HFT_A", hft_type, postfix)
        #plot_residuals(regression_data, eqn, eqn_RVOL, paste(output_dir, subdir, sep="/"), "NHFT_A", postfix)
        
        
        
        
        # Perform several regressions, using the natural logarithm of the HFT data
        # HFT_D only
        #eqn = gsub('(hft_[a-z])', hft_types[hft_type][, var], "ssb + factor(symbol) + market_cap + sum_vol + rel_range + vwap + 
        #hft_d_ln + hft_d_ln * ssb")
        
        #eqn_RVOL = gsub('(hft_[a-z])', hft_types[hft_type][, var], "ssb + factor(symbol) + market_cap + sum_vol + vwap + 
        #hft_d_ln + hft_d_ln * ssb")
        
        #perform_regression(regression_data, eqn, eqn_RVOL, paste(output_dir, "default_regression", subdir, sep="/"), "HFT_D_ln", hft_type, postfix)
        #perform_quantile_regression(regression_data, eqn, eqn_RVOL, 0.50, paste(output_dir, "quantile_regression", subdir, sep="/"), "HFT_D_ln", hft_type, postfix)
        #plot_residuals(regression_data, eqn, eqn_RVOL, paste(output_dir, "default_regression", subdir, sep="/"), "HFT_D_ln", postfix)
        
        
        # HFT_S only
        #eqn = gsub('(hft_[a-z])', hft_types[hft_type][, var], "ssb + factor(symbol) + market_cap + sum_vol + rel_range + vwap + 
        #hft_s_ln + hft_s_ln * ssb")
        
        #eqn_RVOL = gsub('(hft_[a-z])', hft_types[hft_type][, var], "ssb + factor(symbol) + market_cap + sum_vol + vwap + 
        #hft_s_ln + hft_s_ln * ssb")
        
        #perform_regression(regression_data, eqn, eqn_RVOL, paste(output_dir, "default_regression", subdir, sep="/"), "HFT_S_ln", hft_type, postfix)
        #perform_quantile_regression(regression_data, eqn, eqn_RVOL, 0.50, paste(output_dir, "quantile_regression", subdir, sep="/"), "HFT_S_ln", hft_type, postfix)
        #plot_residuals(regression_data, eqn, eqn_RVOL, paste(output_dir, "default_regression", subdir, sep="/"), "HFT_S_ln", postfix)
        
        
        # HFT_A ln only
        eqn = gsub('(hft_[a-z])', hft_types[hft_type][, var], "ssb + market_cap + sum_vol + rel_range + vwap + hft_a_ln + hft_a_ln * ssb")
        eqn_RVOL = gsub('(hft_[a-z])', hft_types[hft_type][, var], "ssb + market_cap + sum_vol + vwap + hft_a_ln + hft_a_ln * ssb")
        
        perform_regression(regression_data, eqn, eqn_RVOL, paste(output_dir, subdir, sep="/"), "HFT_A_ln", hft_type, postfix)
        #perform_quantile_regression(regression_data, eqn, eqn_RVOL, 0.50, paste(output_dir, "quantile_regression", subdir, sep="/"), "HFT_A_ln", hft_type, postfix)
        plot_residuals(regression_data, eqn, eqn_RVOL, paste(output_dir, subdir, sep="/"), "HFT_A_ln", postfix)
        
        
    }
        
        
        # Perform several regressions, using the natural logarithm of the NHFT data
        # NHFT_D only
        #eqn = gsub('(hft_[a-z])', hft_types[hft_type][, var], "ssb + factor(symbol) + market_cap + sum_vol + rel_range + vwap + 
        #nhft_d_ln + nhft_d_ln * ssb")
        
        #eqn_RVOL = gsub('(hft_[a-z])', hft_types[hft_type][, var], "ssb + factor(symbol) + market_cap + sum_vol + vwap + 
        #nhft_d_ln + nhft_d_ln * ssb")
        
        #perform_regression(regression_data, eqn, eqn_RVOL, paste(output_dir, "default_regression", subdir, sep="/"), "NHFT_D_ln", hft_type, postfix)
        #perform_quantile_regression(regression_data, eqn, eqn_RVOL, 0.50, paste(output_dir, "quantile_regression", subdir, sep="/"), "NHFT_D_ln", hft_type, postfix)
        #plot_residuals(regression_data, eqn, eqn_RVOL, paste(output_dir, "default_regression", subdir, sep="/"), "NHFT_D_ln", postfix)
        
        
        # NHFT_S only
        #eqn = gsub('(hft_[a-z])', hft_types[hft_type][, var], "ssb + factor(symbol) + market_cap + sum_vol + rel_range + vwap + 
        #nhft_s_ln + nhft_s_ln * ssb")
        
        #eqn_RVOL = gsub('(hft_[a-z])', hft_types[hft_type][, var], "ssb + factor(symbol) + market_cap + sum_vol + vwap + 
        #nhft_s_ln + nhft_s_ln * ssb")
        
        #perform_regression(regression_data, eqn, eqn_RVOL, paste(output_dir, "default_regression", subdir, sep="/"), "NHFT_S_ln", hft_type, postfix)
        #perform_quantile_regression(regression_data, eqn, eqn_RVOL, 0.50, paste(output_dir, "quantile_regression", subdir, sep="/"), "NHFT_S_ln", hft_type, postfix)
        #plot_residuals(regression_data, eqn, eqn_RVOL, paste(output_dir, "default_regression", subdir, sep="/"), "NHFT_S_ln", postfix)
        
        
        # NHFT_A ln only
        eqn = gsub('(hft_[a-z])', hft_types[hft_type][, var], "ssb + market_cap + sum_vol + rel_range + vwap + nhft_a_ln + nhft_a_ln * ssb")
        
        eqn_RVOL = gsub('(hft_[a-z])', hft_types[hft_type][, var], "ssb + market_cap + sum_vol + vwap + nhft_a_ln + nhft_a_ln * ssb")
        
        #perform_regression(regression_data, eqn, eqn_RVOL, paste(output_dir,subdir, sep="/"), "NHFT_A_ln", hft_type, postfix)
        #perform_quantile_regression(regression_data, eqn, eqn_RVOL, 0.50, paste(output_dir, "quantile_regression", subdir, sep="/"), "NHFT_A_ln", hft_type, postfix)
        #plot_residuals(regression_data, eqn, eqn_RVOL, paste(output_dir, subdir, sep="/"), "NHFT_A_ln", postfix)

        
        
        
        
        # Perform several regressions, using the ratio of HFT to NHFT data
        # HFT_D only
        #eqn = gsub('(hft_[a-z])', hft_types[hft_type][, var], "ssb + factor(symbol) + market_cap + sum_vol + rel_range + vwap + 
        #hft_d_ratio + hft_d_ratio * ssb")
        
        #eqn_RVOL = gsub('(hft_[a-z])', hft_types[hft_type][, var], "ssb + factor(symbol) + market_cap + sum_vol + vwap + 
        #hft_d_ratio + hft_d_ratio * ssb")
        
        #perform_regression(regression_data, eqn, eqn_RVOL, paste(output_dir, "default_regression", subdir, sep="/"), "HFT_D_ratio", hft_type, postfix)
        #perform_quantile_regression(regression_data, eqn, eqn_RVOL, 0.50, paste(output_dir, "quantile_regression", subdir, sep="/"), "HFT_D_ratio", hft_type, postfix)
        
        
        # HFT_S only
        #eqn = gsub('(hft_[a-z])', hft_types[hft_type][, var], "ssb + factor(symbol) + market_cap + sum_vol + rel_range + vwap + 
        #hft_s_ratio + hft_s_ratio * ssb")
        
        #eqn_RVOL = gsub('(hft_[a-z])', hft_types[hft_type][, var], "ssb + factor(symbol) + market_cap + sum_vol + vwap + 
        #hft_s_ratio + hft_s_ratio * ssb")
        
        #perform_regression(regression_data, eqn, eqn_RVOL, paste(output_dir, "default_regression", subdir, sep="/"), "HFT_S_ratio", hft_type, postfix)
        #perform_quantile_regression(regression_data, eqn, eqn_RVOL, 0.50, paste(output_dir, "quantile_regression", subdir, sep="/"), "HFT_S_ratio", hft_type, postfix)
        
        
        # HFT_A only
        #eqn = gsub('(hft_[a-z])', hft_types[hft_type][, var], "ssb + factor(symbol) + market_cap + sum_vol + rel_range + vwap + 
        #hft_a_ratio + hft_a_ratio * ssb")
        
        #eqn_RVOL = gsub('(hft_[a-z])', hft_types[hft_type][, var], "ssb + factor(symbol) + market_cap + sum_vol + vwap + 
        #hft_a_ratio + hft_a_ratio * ssb")
        
        #perform_regression(regression_data, eqn, eqn_RVOL, paste(output_dir, "default_regression", subdir, sep="/"), "HFT_A_ratio", hft_type, postfix)
        #perform_quantile_regression(regression_data, eqn, eqn_RVOL, 0.50, paste(output_dir, "quantile_regression", subdir, sep="/"), "HFT_A_ratio", hft_type, postfix)
        
        
        
        
        
        # Perform several regressions, using the ratio of natural log HFT to NHFT
        # HFT_D only
        #eqn = gsub('(hft_[a-z])', hft_types[hft_type][, var], "ssb + factor(symbol) + market_cap + sum_vol + rel_range + vwap + 
        #hft_d_ratio_ln + hft_d_ratio_ln * ssb")
        
        #eqn_RVOL = gsub('(hft_[a-z])', hft_types[hft_type][, var], "ssb + factor(symbol) + market_cap + sum_vol + vwap + 
        #hft_d_ratio_ln + hft_d_ratio_ln * ssb")
        
        #perform_regression(regression_data, eqn, eqn_RVOL, paste(output_dir, "default_regression", subdir, sep="/"), "HFT_D_ratio_ln", hft_type, postfix)
        #perform_quantile_regression(regression_data, eqn, eqn_RVOL, 0.50, paste(output_dir, "quantile_regression", subdir, sep="/"), "HFT_D_ratio_ln", hft_type, postfix)
        #plot_residuals(regression_data, eqn, eqn_RVOL, paste(output_dir, "default_regression", subdir, sep="/"), "HFT_D_ratio_ln", postfix)
        
        
        # HFT_S only
        #eqn = gsub('(hft_[a-z])', hft_types[hft_type][, var], "ssb + factor(symbol) + market_cap + sum_vol + rel_range + vwap + 
        #hft_s_ratio_ln + hft_s_ratio_ln * ssb")
        
        #eqn_RVOL = gsub('(hft_[a-z])', hft_types[hft_type][, var], "ssb + factor(symbol) + market_cap + sum_vol + vwap + 
        #hft_s_ratio_ln + hft_s_ratio_ln * ssb")
        
        #perform_regression(regression_data, eqn, eqn_RVOL, paste(output_dir, "default_regression", subdir, sep="/"), "HFT_S_ratio_ln", hft_type, postfix)
        #perform_quantile_regression(regression_data, eqn, eqn_RVOL, 0.50, paste(output_dir, "quantile_regression", subdir, sep="/"), "HFT_S_ratio_ln", hft_type, postfix)
        #plot_residuals(regression_data, eqn, eqn_RVOL, paste(output_dir, "default_regression", subdir, sep="/"), "HFT_S_ratio_ln", postfix)
        
        
        # HFT_A only
        #eqn = gsub('(hft_[a-z])', hft_types[hft_type][, var], "ssb + factor(symbol) + market_cap + sum_vol + rel_range + vwap + 
        #hft_a_ratio_ln + hft_a_ratio_ln * ssb")
        
        #eqn_RVOL = gsub('(hft_[a-z])', hft_types[hft_type][, var], "ssb + factor(symbol) + market_cap + sum_vol + vwap + 
        #hft_a_ratio_ln + hft_a_ratio_ln * ssb")
        
        #perform_regression(regression_data, eqn, eqn_RVOL, paste(output_dir, "default_regression", subdir, sep="/"), "HFT_A_ratio_ln", hft_type, postfix)
        #perform_quantile_regression(regression_data, eqn, eqn_RVOL, 0.50, paste(output_dir, "quantile_regression", subdir, sep="/"), "HFT_A_ratio_ln", hft_type, postfix)
        #plot_residuals(regression_data, eqn, eqn_RVOL, paste(output_dir, "default_regression", subdir, sep="/"), "HFT_A_ratio_ln", postfix)
    }
}






# Remove any actual NA entries from the regression summary
rm_NAs()

# Write the summary results of the regressions to file
reg_summary[, row := NULL]
write.csv(reg_summary, paste(root_dir, "regression_summary.csv", sep="/"), row.names = FALSE)





# Write the raw data used in the difference regressions to file
write.csv(regression_table[, list(time, symbol, Q, sum_vol, rel_range, market_cap, vwap,
                        NRQS, NQRQS, NRES, NQRES, NRPI5, NQRPI5, ssb,
                        hft_d, hft_s, hft_a,
                        hft_d_long, hft_s_long, hft_a_long,   
                        hft_d_short, hft_s_short, hft_a_short, 
                        
                        hft_d_ln, hft_s_ln, hft_a_ln,
                        hft_d_long_ln, hft_s_long_ln, hft_a_long_ln,
                        hft_d_short_ln, hft_s_short_ln, hft_a_short_ln,
                        
                        nhft_d, nhft_s, nhft_a,
                        nhft_d_long, nhft_s_long, nhft_a_long,
                        nhft_d_short, nhft_s_short, nhft_a_short,
                                  
                        nhft_d_ln, nhft_s_ln, nhft_a_ln,
                        nhft_d_long_ln, nhft_s_long_ln, nhft_a_long_ln,
                        nhft_d_short_ln, nhft_s_short_ln, nhft_a_short_ln)],
          paste(data_dir, "regression_diff_panel.csv", sep="/"), row.names = FALSE)




# Write the raw data used in the regressions for all symbols to file
write.csv(regression_table_all, paste(data_dir, "regression_all_panel.csv", sep="/"), row.names = FALSE)
