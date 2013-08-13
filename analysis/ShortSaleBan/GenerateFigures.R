###############################################################################
#
# Generates figures for the paper based on the results of the data and 
# regression analysis.
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
library(scales)
library(reshape)


# Create the dataset for the figures, the data is calculated for each stock each day.
# For each dataset the results are broken up into 4 quantiles based on market cap
gen_figure_dataset <- function(daily_data, matches)
{
  # Get the list of banned symbols
  #ban_date <- as_date("2008-09-19")
  #exp_date <- as_date("2008-10-09")
  banned <- matches[, symbol]
  
  # Types of HFT and Non-HFT demanding/supplying liquidity
  types <- data.table(type=c("HFT_D","HFT_S", "NHFT_D", "NHFT_S"), 
                      v1=c("HH", "HH", "NN", "NN"), 
                      v2=c("HN", "NH", "NH", "HN"))
  setkey(types, type)
  
  # ShortSale status 
  short_status <- c(TRUE,FALSE)
  
  
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
  
  # For each banned symbol get the match and calculate variables for the figures
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
    
    
    # Calculate the mean of the variables RES, RPI5, and relative range
    result <- daily_data[symbol == ban_symbol, 
                         list(type="Banned", Q=quartile, NRES=mean(mean_NRES), NQRES=mean(mean_NQRES), 
                              NRPI5=mean(mean_NRPI5), NQRPI5=mean(mean_NQRPI5), 
                              rel_range=mean(rel_range)), 
                         by="time,symbol"]
    
    result <- rbind(result, 
                    daily_data[symbol == matched, 
                               list(type="Matched", Q=quartile, NRES=mean(mean_NRES), NQRES=mean(mean_NQRES), 
                                    NRPI5=mean(mean_NRPI5), NQRPI5=mean(mean_NQRPI5), 
                                    rel_range=mean(rel_range)), 
                               by="time,symbol"])
    
    
    # Calculate the shorting and total volume, which is used to calculate RELSS
    daily_volume <- daily_data[symbol == ban_symbol & ShortSale == TRUE,
                               list(type="Banned", Q=quartile, short_vol=sum(sum_vol)), by="time,symbol"]
    
    daily_volume <- merge(daily_volume, 
                          daily_data[symbol == ban_symbol,
                                     list(total_vol=sum(sum_vol)), by="time"],
                          by="time")
    
    temp <- daily_data[symbol == matched & ShortSale == TRUE,
                       list(type="Matched", Q=quartile, short_vol=sum(sum_vol)), by="time,symbol"]
    
    temp <- merge(temp, 
                  daily_data[symbol == matched, 
                             list(total_vol=sum(sum_vol)), by="time"], 
                  by="time")
    
    daily_volume <- rbind(daily_volume, temp)
    
    
    # Calculate RELSS, which is shortsale volume divided by the total trading volume
    daily_volume[, RELSS := (short_vol / total_vol)]
    setkey(daily_volume, time, symbol, type, Q)
    
    
    # Calculate the effective spreads (RES) for short sales only
    shorting_RES <- daily_data[symbol == ban_symbol & ShortSale == TRUE,
                               list(type="Banned", Q=quartile, NRES_short=mean(mean_NRES), NQRES_short=mean(mean_NQRES)), 
                               by="time,symbol"]
    
    shorting_RES <- rbind(shorting_RES, 
                          daily_data[symbol == matched & ShortSale == TRUE,
                                     list(type="Matched", Q=quartile, NRES_short=mean(mean_NRES), NQRES_short=mean(mean_NQRES)), 
                                     by="time,symbol"])
    
    setkey(shorting_RES, time, symbol, type, Q)
    
                          
    # Add RELSS and shorting RES to the result for the banned and matched symbol
    setkey(result, time, symbol, type, Q)
    result <- merge(result, 
                    daily_volume[, list(RELSS), by="time,symbol,type,Q"])
                          
    result <- merge(result, 
                    shorting_RES[, list(NRES_short, NQRES_short), by="time,symbol,type,Q"])
    
    
    # Add results for current symbol to the list of results
    if (exists("results"))
    {
      results <- rbind(results, result)
    }
    else
    {
      results <- result
    }
    setkey(results, time, symbol, type, Q)
  }
  
  # Return the mean of all stocks for each day
  results[, time := as.Date(time)]
  return(results[, list(NRES=mean(NRES), NQRES=mean(NQRES),
                        NRES_short=mean(NRES_short), NQRES_short=mean(NQRES_short),
                        NRPI5=mean(NRPI5, trim=0.05), NQRPI5=mean(NQRPI5, trim=0.05), 
                        rel_range=mean(rel_range), RELSS=mean(RELSS)), 
                 by="time,type,Q"])
}


# Create the dataset for the figures based on the regressions data, which is the
# difference between the banned and unbanned symbols
gen_reg_dataset <- function(daily_data)
{
  return(daily_data[, list(hft_d=mean(hft_d, na.rm=TRUE), hft_s=mean(hft_s, na.rm=TRUE), hft_a=mean(hft_a, na.rm=TRUE),
                           hft_d_short=mean(hft_d_short, na.rm=TRUE), hft_s_short=mean(hft_s_short, na.rm=TRUE), hft_a_short=mean(hft_a_short, na.rm=TRUE),
                           nhft_d=mean(nhft_d, na.rm=TRUE), nhft_s=mean(nhft_s, na.rm=TRUE), nhft_a=mean(nhft_a, na.rm=TRUE),
                           nhft_d_short=mean(nhft_d_short, na.rm=TRUE), nhft_s_short=mean(nhft_s_short, na.rm=TRUE), nhft_a_short=mean(nhft_a_short, na.rm=TRUE)
                           ), by="time,Q"])
}



# Create the initial HFT dataset for the HFT and NHFT daily total dollar volume for all stocks.
# Daily_data, is the daily summary statistics, time_weight_data, which contains the time-weighted 
gen_hft_dataset <- function(daily_data, matches)
{
  # Get the list of banned symbols
  #ban_date <- as_date("2008-09-19")
  #exp_date <- as_date("2008-10-09")
  banned <- matches[, symbol]
  
  # Types of HFT and Non-HFT demanding/supplying liquidity
  types <- data.table(type=c("HFT_D","HFT_S", "NHFT_D", "NHFT_S"), 
                      v1=c("HH", "HH", "NN", "NN"), 
                      v2=c("HN", "NH", "NH", "HN"))
  setkey(types, type)
  
  # ShortSale status 
  short_status <- c(TRUE,FALSE)
  
  
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
  
  # For each banned symbol get the match and calculate the volume
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
  
    # For each symbol get the HFT and NHFT total daily dollar volume
    for (entry in types[, type])
    {      
      # Calculate the total volume and shortsale volume, which is used to calculate RELSS
      daily_volume <- daily_data[symbol == ban_symbol & type %in% types[entry][,c(v1,v2)],
                                 list(type=entry, status="Banned", Q=quartile, total_vol=sum(sum_vol)), by="time,symbol"]
      
      daily_volume <- merge(daily_volume, 
                            daily_data[symbol == ban_symbol & type %in% types[entry][,c(v1,v2)] & ShortSale == TRUE,
                                       list(short_vol=sum(sum_vol)), by="time"],
                            by="time", all.x=TRUE)
      
      temp <- daily_data[symbol == matched & type %in% types[entry][,c(v1,v2)],
                         list(type=entry, status="Matched", Q=quartile, total_vol=sum(sum_vol)), by="time,symbol"]
      
      temp <- merge(temp, 
                    daily_data[symbol == matched & type %in% types[entry][,c(v1,v2)] & ShortSale == TRUE, 
                               list(short_vol=sum(sum_vol)), by="time"], 
                    by="time", all.x=TRUE)
      
      daily_volume <- rbind(daily_volume, temp)
      
      
      # Set any NA entries to 0
      daily_volume[is.na(total_vol), total_vol := 0]
      daily_volume[is.na(short_vol), short_vol := 0]
      
      
      if (exists("results"))
      {
        results <- rbind(results, daily_volume)
      }
      else
      {
        results <- daily_volume
      }
      setkey(results, time, symbol, type, status, Q)
    }
  }
  
  return(results)
}


# Create a final table for generating figures for HFT and NHFT daily total dollar volume for all stocks.
gen_hft_table <- function(dataset)
{  
  # The initial table of each date, symbol, and status
  results <- dataset[, list(Q=unique(Q)), by="time,symbol,status"]
  setkey(results, time, symbol, status, Q)
  
  
  # Set the volume for hft for supply, demand, and all
  results <- merge(results, 
                   dataset[type == "HFT_D", list(hft_d=total_vol), by="time,symbol"], all.x=TRUE)
  results <- merge(results, 
                   dataset[type == "HFT_S", list(hft_s=total_vol), by="time,symbol"], all.x=TRUE)
  
  # Set any entries that are NA to 0
  results[is.na(hft_d), hft_d := 0]
  results[is.na(hft_s), hft_s := 0]
  
  # Calculate the combined volume for HFT supply and demand
  results[, hft_a := (hft_d + hft_s)]
  
  
  # Set the volume for hft for supply, demand, and all for shortsale ONLY
  results <- merge(results, 
                   dataset[type == "HFT_D", list(hft_d_short=short_vol), by="time,symbol"], all.x=TRUE)
  results <- merge(results, 
                   dataset[type == "HFT_S", list(hft_s_short=short_vol), by="time,symbol"], all.x=TRUE)
  
  # Set any entries that are NA to 0
  results[is.na(hft_d_short), hft_d_short := 0]
  results[is.na(hft_s_short), hft_s_short := 0]
  
  # Calculate the combined volume for HFT supply and demand
  results[, hft_a_short := (hft_d_short + hft_s_short)]
  
  
  
  
  # Set the volume for non-hft for supply, demand, and all
  results <- merge(results, 
                   dataset[type == "NHFT_D", list(nhft_d=total_vol), by="time,symbol"], all.x=TRUE)
  results <- merge(results, 
                   dataset[type == "NHFT_S", list(nhft_s=total_vol), by="time,symbol"], all.x=TRUE)
  
  # Set any entries that are NA to 0
  results[is.na(nhft_d), nhft_d := 0]
  results[is.na(nhft_s), nhft_s := 0]
  
  # Calculate the combined volume for NHFT supply and demand
  results[, nhft_a := (nhft_d + nhft_s)]
  
  
  # Set the volume for non-hft for supply, demand, and all for shortsale ONLY
  results <- merge(results, 
                   dataset[type == "NHFT_D", list(nhft_d_short=short_vol), by="time,symbol"], all.x=TRUE)
  results <- merge(results, 
                   dataset[type == "NHFT_S", list(nhft_s_short=short_vol), by="time,symbol"], all.x=TRUE)
  
  # Set any entries that are NA to 0
  results[is.na(nhft_d_short), nhft_d_short := 0]
  results[is.na(nhft_s_short), nhft_s_short := 0]
  
  # Calculate the combined volume for NHFT supply and demand
  results[, nhft_a_short := (nhft_d_short + nhft_s_short)]
  
  
  return(results)
}


# The output directory for images
output_dir <- "/home/jon/Source/RESEARCH/finance-research/analysis/ShortSaleBan/figures/13-08-2013"


# The start and end of the ban period 
ban_date <- as.Date("2008-09-19")
exp_date <- as.Date("2008-10-09")

# The quartiles for each plot
plot_quartiles <- c("Q1", "Q2", "Q3", "Q4")


# Create the dataset for the HFT/NHFT volume figures
hft_dataset <- gen_hft_dataset(daily_results, final_matched)
hft_table <- gen_hft_table(hft_dataset)


# Create a final dataset for plotting the hft figures of the mean total daily volume
hft_figure_dataset <- hft_table[, list(hft_d=mean(hft_d), hft_s=mean(hft_s), hft_a=mean(hft_a),
                                       hft_d_short=mean(hft_d_short), hft_s_short=mean(hft_s_short), hft_a_short=mean(hft_a_short),
                                       nhft_d=mean(nhft_d), nhft_s=mean(nhft_s), nhft_a=mean(nhft_a),
                                       nhft_d_short=mean(nhft_d_short), nhft_s_short=mean(nhft_s_short), nhft_a_short=mean(nhft_a_short)), 
                              by="time,status,Q"]
hft_figure_dataset[, time := as.Date(time)]


# Display one of each HFT/NHFT volume plot for each quartile
for (quartile in plot_quartiles)
{
  # Display the HFT_d results for all transactions, identify the ban period on the plot
  p <- ggplot(hft_figure_dataset[Q == quartile]) + geom_line(aes(x=time, y=hft_d, linetype=status), size=1)
  rect <- data.table(xmin=ban_date, xmax=exp_date, ymin=-Inf, ymax=Inf)
  p <- p + scale_x_date(labels = date_format("%d-%b"), breaks = date_breaks("2 weeks")) + 
    scale_y_continuous(labels = dollar) +
    geom_rect(data=rect, aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax), color="grey50", alpha=0.2, inherit.aes = FALSE) + 
    labs(x = "Date", y = "Daily Volume ($)", title=paste("HFT_D Average Total Daily Volume ($)", quartile, sep=" - "), linetype="Type")
  #print(p)
  directory <- paste(output_dir, "HFT_D", sep="/")
  dir.create(directory, showWarnings = FALSE, recursive = TRUE)
  file <- paste("HFT_D - ", quartile, ".png", sep="")
  file <- paste(directory, file, sep="/")
  ggsave(filename=file, plot=p, width=12, height=6)
  
  
  # Display the NHFT_d results for all transactions, identify the ban period on the plot
  p <- ggplot(hft_figure_dataset[Q == quartile]) + geom_line(aes(x=time, y=nhft_d, linetype=status), size=1)
  rect <- data.table(xmin=ban_date, xmax=exp_date, ymin=-Inf, ymax=Inf)
  p <- p + scale_x_date(labels = date_format("%d-%b"), breaks = date_breaks("2 weeks")) + 
    scale_y_continuous(labels = dollar) +
    geom_rect(data=rect, aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax), color="grey50", alpha=0.2, inherit.aes = FALSE) + 
    labs(x = "Date", y = "Daily Volume ($)", title=paste("NHFT_D Average Total Daily Volume ($)", quartile, sep=" - "), linetype="Type")
  #print(p)
  directory <- paste(output_dir, "NHFT_D", sep="/")
  dir.create(directory, showWarnings = FALSE, recursive = TRUE)
  file <- paste("NHFT_D - ", quartile, ".png", sep="")
  file <- paste(directory, file, sep="/")
  ggsave(filename=file, plot=p, width=12, height=6)
  
  
  
  
  # Display the HFT_s results for all transactions, identify the ban period on the plot
  p <- ggplot(hft_figure_dataset[Q == quartile]) + geom_line(aes(x=time, y=hft_s, linetype=status), size=1)
  rect <- data.table(xmin=ban_date, xmax=exp_date, ymin=-Inf, ymax=Inf)
  p <- p + scale_x_date(labels = date_format("%d-%b"), breaks = date_breaks("2 weeks")) + 
    scale_y_continuous(labels = dollar) +
    geom_rect(data=rect, aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax), color="grey50", alpha=0.2, inherit.aes = FALSE) + 
    labs(x = "Date", y = "Daily Volume ($)", title=paste("HFT_S Average Total Daily Volume ($)", quartile, sep=" - "), linetype="Type")
  #print(p)
  directory <- paste(output_dir, "HFT_S", sep="/")
  dir.create(directory, showWarnings = FALSE, recursive = TRUE)
  file <- paste("HFT_S - ", quartile, ".png", sep="")
  file <- paste(directory, file, sep="/")
  ggsave(filename=file, plot=p, width=12, height=6)
  
  
  # Display the NHFT_s results for all transactions, identify the ban period on the plot
  p <- ggplot(hft_figure_dataset[Q == quartile]) + geom_line(aes(x=time, y=nhft_s, linetype=status), size=1)
  rect <- data.table(xmin=ban_date, xmax=exp_date, ymin=-Inf, ymax=Inf)
  p <- p + scale_x_date(labels = date_format("%d-%b"), breaks = date_breaks("2 weeks")) + 
    scale_y_continuous(labels = dollar) +
    geom_rect(data=rect, aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax), color="grey50", alpha=0.2, inherit.aes = FALSE) + 
    labs(x = "Date", y = "Daily Volume ($)", title=paste("NHFT_S Average Total Daily Volume ($)", quartile, sep=" - "), linetype="Type")
  #print(p)
  directory <- paste(output_dir, "NHFT_S", sep="/")
  dir.create(directory, showWarnings = FALSE, recursive = TRUE)
  file <- paste("NHFT_S - ", quartile, ".png", sep="")
  file <- paste(directory, file, sep="/")
  ggsave(filename=file, plot=p, width=12, height=6)
  
  
  
  
  # Display HFT and NHFT on the same plot for all transactions, identify the ban period on the plot
  hft_nhft <- melt(hft_figure_dataset[Q == quartile, list(time, status, hft_a, nhft_a)], id=c("time", "status"))
  p <- ggplot(hft_nhft) + geom_line(aes(x=time, y=value, colour=variable, linetype=status), size=1)
  rect <- data.table(xmin=ban_date, xmax=exp_date, ymin=-Inf, ymax=Inf)
  p <- p + scale_x_date(labels = date_format("%d-%b"), breaks = date_breaks("2 weeks")) + 
    scale_y_continuous(labels = dollar) +
    geom_rect(data=rect, aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax), color="grey50", alpha=0.2, inherit.aes = FALSE) + 
    labs(x = "Date", y = "Daily Volume ($)", title=paste("HFT and NHFT Average Total Daily Volume ($)", quartile, sep=" - "), colour="Category", linetype="Type") +
    scale_colour_grey(start = 0, end = .6) + theme_bw()
  directory <- paste(output_dir, "HFT_NHFT", sep="/")
  dir.create(directory, showWarnings = FALSE, recursive = TRUE)
  file <- paste("HFT NHFT - ", quartile, ".png", sep="")
  file <- paste(directory, file, sep="/")
  ggsave(filename=file, plot=p, width=12, height=6)
  
  
  
  
  # Display HFT and NHFT on the same plot for all shortsale transactions, identify the ban period on the plot
  hft_nhft_short <- melt(hft_figure_dataset[Q == quartile, list(time, status, hft_a_short, nhft_a_short)], id=c("time", "status"))
  p <- ggplot(hft_nhft_short) + geom_line(aes(x=time, y=value, colour=variable, linetype=status), size=1) 
  rect <- data.table(xmin=ban_date, xmax=exp_date, ymin=-Inf, ymax=Inf)
  p <- p + scale_x_date(labels = date_format("%d-%b"), breaks = date_breaks("2 weeks")) + 
    scale_y_continuous(labels = dollar) +
    geom_rect(data=rect, aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax), color="grey50", alpha=0.2, inherit.aes = FALSE) + 
    labs(x = "Date", y = "Daily Volume ($)", title=paste("HFT and NHFT Short Sale Only Average Total Daily Volume ($)", quartile, sep=" - "), colour="Category", linetype="Type") +
    scale_colour_grey(start = 0, end = .6) + theme_bw()
  directory <- paste(output_dir, "HFT_NHFT", sep="/")
  file <- paste("HFT NHFT Short Sale Only - ", quartile, ".png", sep="")
  file <- paste(directory, file, sep="/")
  ggsave(filename=file, plot=p, width=12, height=6)
  
  
  
  
  #print(p)
  directory <- paste(output_dir, "HFT_A", sep="/")
  dir.create(directory, showWarnings = FALSE, recursive = TRUE)
  file <- paste("HFT_A - ", quartile, ".png", sep="")
  file <- paste(directory, file, sep="/")
  ggsave(filename=file, plot=p, width=12, height=6)
  
  
  # Display the NHFT_a results for all transactions, identify the ban period on the plot
  p <- ggplot(hft_figure_dataset[Q == quartile]) + geom_line(aes(x=time, y=nhft_a, linetype=status), size=1)
  rect <- data.table(xmin=ban_date, xmax=exp_date, ymin=-Inf, ymax=Inf)
  p <- p + scale_x_date(labels = date_format("%d-%b"), breaks = date_breaks("2 weeks")) + 
    scale_y_continuous(labels = dollar) +
    geom_rect(data=rect, aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax), color="grey50", alpha=0.2, inherit.aes = FALSE) + 
    labs(x = "Date", y = "Daily Volume ($)", title=paste("NHFT_A Average Total Daily Volume ($)", quartile, sep=" - "), linetype="Type")
  #print(p)
  directory <- paste(output_dir, "NHFT_A", sep="/")
  dir.create(directory, showWarnings = FALSE, recursive = TRUE)
  file <- paste("NHFT_A - ", quartile, ".png", sep="")
  file <- paste(directory, file, sep="/")
  ggsave(filename=file, plot=p, width=12, height=6)
  
  
  
  
  # Display the HFT_d shortsale ONLY results for all transactions, identify the ban period on the plot
  p <- ggplot(hft_figure_dataset[Q == quartile]) + geom_line(aes(x=time, y=hft_d_short, linetype=status), size=1)
  rect <- data.table(xmin=ban_date, xmax=exp_date, ymin=-Inf, ymax=Inf)
  p <- p + scale_x_date(labels = date_format("%d-%b"), breaks = date_breaks("2 weeks")) + 
    scale_y_continuous(labels = dollar) +
    geom_rect(data=rect, aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax), color="grey50", alpha=0.2, inherit.aes = FALSE) + 
    labs(x = "Date", y = "Daily Volume ($)", title=paste("HFT_D Short Sale Only Average Total Daily Volume ($)", quartile, sep=" - "), linetype="Type")
  #print(p)
  directory <- paste(output_dir, "HFT_D", sep="/")
  file <- paste("HFT_D Short Sale Only - ", quartile, ".png", sep="")
  file <- paste(directory, file, sep="/")
  ggsave(filename=file, plot=p, width=12, height=6)
  
  
  # Display the NHFT_d shortsale ONLY results for all transactions, identify the ban period on the plot
  p <- ggplot(hft_figure_dataset[Q == quartile]) + geom_line(aes(x=time, y=nhft_d_short, linetype=status), size=1)
  rect <- data.table(xmin=ban_date, xmax=exp_date, ymin=-Inf, ymax=Inf)
  p <- p + scale_x_date(labels = date_format("%d-%b"), breaks = date_breaks("2 weeks")) + 
    scale_y_continuous(labels = dollar) +
    geom_rect(data=rect, aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax), color="grey50", alpha=0.2, inherit.aes = FALSE) + 
    labs(x = "Date", y = "Daily Volume ($)", title=paste("NHFT_D Short Sale Only Average Total Daily Volume ($)", quartile, sep=" - "), linetype="Type")
  #print(p)
  directory <- paste(output_dir, "NHFT_D", sep="/")
  file <- paste("NHFT_D Short Sale Only - ", quartile, ".png", sep="")
  file <- paste(directory, file, sep="/")
  ggsave(filename=file, plot=p, width=12, height=6)
  
  
  
  
  # Display the HFT_s shortsale ONLY results for all transactions, identify the ban period on the plot
  p <- ggplot(hft_figure_dataset[Q == quartile]) + geom_line(aes(x=time, y=hft_s_short, linetype=status), size=1)
  rect <- data.table(xmin=ban_date, xmax=exp_date, ymin=-Inf, ymax=Inf)
  p <- p + scale_x_date(labels = date_format("%d-%b"), breaks = date_breaks("2 weeks")) + 
    scale_y_continuous(labels = dollar) +
    geom_rect(data=rect, aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax), color="grey50", alpha=0.2, inherit.aes = FALSE) + 
    labs(x = "Date", y = "Daily Volume ($)", title=paste("HFT_S Short Sale Only Average Total Daily Volume ($)", quartile, sep=" - "), linetype="Type")
  #print(p)
  directory <- paste(output_dir, "HFT_S", sep="/")
  file <- paste("HFT_S Short Sale Only - ", quartile, ".png", sep="")
  file <- paste(directory, file, sep="/")
  ggsave(filename=file, plot=p, width=12, height=6)
  
  
  # Display the NHFT_s shortsale ONLY results for all transactions, identify the ban period on the plot
  p <- ggplot(hft_figure_dataset[Q == quartile]) + geom_line(aes(x=time, y=nhft_s_short, linetype=status), size=1)
  rect <- data.table(xmin=ban_date, xmax=exp_date, ymin=-Inf, ymax=Inf)
  p <- p + scale_x_date(labels = date_format("%d-%b"), breaks = date_breaks("2 weeks")) + 
    scale_y_continuous(labels = dollar) +
    geom_rect(data=rect, aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax), color="grey50", alpha=0.2, inherit.aes = FALSE) + 
    labs(x = "Date", y = "Daily Volume ($)", title=paste("NHFT_S Short Sale Only Average Total Daily Volume ($)", quartile, sep=" - "), linetype="Type")
  #print(p)
  directory <- paste(output_dir, "NHFT_S", sep="/")
  file <- paste("NHFT_S Short Sale Only - ", quartile, ".png", sep="")
  file <- paste(directory, file, sep="/")
  ggsave(filename=file, plot=p, width=12, height=6)
  
  
  
  
  # Display the HFT_a shortsale ONLY results for all transactions, identify the ban period on the plot  
  p <- ggplot(hft_figure_dataset[Q == quartile]) + geom_line(aes(x=time, y=hft_a_short, linetype=status), size=1)
  rect <- data.table(xmin=ban_date, xmax=exp_date, ymin=-Inf, ymax=Inf)
  p <- p + scale_x_date(labels = date_format("%d-%b"), breaks = date_breaks("2 weeks")) + 
    scale_y_continuous(labels = dollar) +
    geom_rect(data=rect, aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax), color="grey50", alpha=0.2, inherit.aes = FALSE) + 
    labs(x = "Date", y = "Daily Volume ($)", title=paste("HFT_A Short Sale Only Average Total Daily Volume ($)", quartile, sep=" - "), linetype="Type")
  #print(p)
  directory <- paste(output_dir, "HFT_A", sep="/")
  file <- paste("HFT_A Short Sale Only - ", quartile, ".png", sep="")
  file <- paste(directory, file, sep="/")
  ggsave(filename=file, plot=p, width=12, height=6)
  
  
  # Display the NHFT_a shortsale ONLY results for all transactions, identify the ban period on the plot
  p <- ggplot(hft_figure_dataset[Q == quartile]) + geom_line(aes(x=time, y=nhft_a_short, linetype=status), size=1)
  rect <- data.table(xmin=ban_date, xmax=exp_date, ymin=-Inf, ymax=Inf)
  p <- p + scale_x_date(labels = date_format("%d-%b"), breaks = date_breaks("2 weeks")) + 
    scale_y_continuous(labels = dollar) +
    geom_rect(data=rect, aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax), color="grey50", alpha=0.2, inherit.aes = FALSE) + 
    labs(x = "Date", y = "Daily Volume ($)", title=paste("NHFT_A Short Sale Only Average Total Daily Volume ($)", quartile, sep=" - "), linetype="Type")
  #print(p)
  directory <- paste(output_dir, "NHFT_A", sep="/")
  file <- paste("NHFT_A Short Sale Only - ", quartile, ".png", sep="")
  file <- paste(directory, file, sep="/")
  ggsave(filename=file, plot=p, width=12, height=6)
}






# Create the dataset for the shackling table figures and plot the results
figure_dataset <- gen_figure_dataset(daily_results, final_matched)

# Display one of each plot for each quartile
for (quartile in plot_quartiles)
{
  # Display the RELSS results for all transactions, identify the ban period on the plot
  p <- ggplot(figure_dataset[Q == quartile]) + geom_line(aes(x=time, y=RELSS, linetype=type), size=1)
  rect <- data.table(xmin=ban_date, xmax=exp_date, ymin=-Inf, ymax=Inf)
  p <- p + scale_x_date(labels = date_format("%d-%b"), breaks = date_breaks("2 weeks")) + 
       geom_rect(data=rect, aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax), color="grey50", alpha=0.2, inherit.aes = FALSE) + 
       labs(x = "Date", y = "RELSS", title=paste("Short-selling Activity (RELSS)", quartile, sep=" - "), linetype="Type")
  #print(p)
  directory <- paste(output_dir, "RELSS", sep="/")
  dir.create(directory, showWarnings = FALSE, recursive = TRUE)
  file <- paste("RELSS - ", quartile, ".png", sep="")
  file <- paste(directory, file, sep="/")
  ggsave(filename=file, plot=p, width=12, height=6)
  
  
  
  
  # Display the National RES results for all transacitons, identify the ban period on the plot
  p <- ggplot(figure_dataset[Q == quartile]) + geom_line(aes(x=time, y=NRES, linetype=type), size=1)
  rect <- data.table(xmin=ban_date, xmax=exp_date, ymin=-Inf, ymax=Inf)
  p <- p + scale_x_date(labels = date_format("%d-%b"), breaks = date_breaks("2 weeks")) + 
       geom_rect(data=rect, aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax), color="grey50", alpha=0.2, inherit.aes = FALSE) + 
       labs(x = "Date", y = "RES", title=paste("National Effective Spreads (RES)", quartile, sep=" - "), linetype="Type")
  #print(p)
  directory <- paste(output_dir, "RES", sep="/")
  dir.create(directory, showWarnings = FALSE, recursive = TRUE)
  file <- paste("National RES - ", quartile, ".png", sep="")
  file <- paste(directory, file, sep="/")
  ggsave(filename=file, plot=p, width=12, height=6)
  
  
  # Display the NASDAQ RES results for all transacitons, identify the ban period on the plot
  p <- ggplot(figure_dataset[Q == quartile]) + geom_line(aes(x=time, y=NQRES, linetype=type), size=1)
  rect <- data.table(xmin=ban_date, xmax=exp_date, ymin=-Inf, ymax=Inf)
  p <- p + scale_x_date(labels = date_format("%d-%b"), breaks = date_breaks("2 weeks")) + 
       geom_rect(data=rect, aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax), color="grey50", alpha=0.2, inherit.aes = FALSE) + 
       labs(x = "Date", y = "RES", title=paste("NASDAQ Effective Spreads (RES)", quartile, sep=" - "), linetype="Type")
  #print(p)
  directory <- paste(output_dir, "RES", sep="/")
  file <- paste("NASDAQ RES - ", quartile, ".png", sep="")
  file <- paste(directory, file, sep="/")
  ggsave(filename=file, plot=p, width=12, height=6)
  
  
  
  
  # Display the National RPI5 results for all transacitons, identify the ban period on the plot
  p <- ggplot(figure_dataset[Q == quartile]) + geom_line(aes(x=time, y=NRPI5, linetype=type), size=1)
  rect <- data.table(xmin=ban_date, xmax=exp_date, ymin=-Inf, ymax=Inf)
  p <- p + scale_x_date(labels = date_format("%d-%b"), breaks = date_breaks("2 weeks")) + 
       geom_rect(data=rect, aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax), color="grey50", alpha=0.2, inherit.aes = FALSE) + 
       labs(x = "Date", y = "RPI5", title=paste("National Price Impacts (RPI5)", quartile, sep=" - "), linetype="Type")
  #print(p)
  directory <- paste(output_dir, "RPI5", sep="/")
  dir.create(directory, showWarnings = FALSE, recursive = TRUE)
  file <- paste("National RPI5 - ", quartile, ".png", sep="")
  file <- paste(directory, file, sep="/")
  ggsave(filename=file, plot=p, width=12, height=6)
  
  
  # Display the NASDAQ RPI5 results for all transacitons, identify the ban period on the plot
  p <- ggplot(figure_dataset[Q == quartile]) + geom_line(aes(x=time, y=NQRPI5, linetype=type), size=1)
  rect <- data.table(xmin=ban_date, xmax=exp_date, ymin=-Inf, ymax=Inf)
  p <- p + scale_x_date(labels = date_format("%d-%b"), breaks = date_breaks("2 weeks")) + 
       geom_rect(data=rect, aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax), color="grey50", alpha=0.2, inherit.aes = FALSE) + 
       labs(x = "Date", y = "RPI5", title=paste("NASDAQ Price Impacts (RPI5)", quartile, sep=" - "), linetype="Type")
  #print(p)
  directory <- paste(output_dir, "RPI5", sep="/")
  file <- paste("NASDAQ RPI5 - ", quartile, ".png", sep="")
  file <- paste(directory, file, sep="/")
  ggsave(filename=file, plot=p, width=12, height=6)
  
  
  
  
  # Display the relative range (RVOL)
  p <- ggplot(figure_dataset[Q == quartile]) + geom_line(aes(x=time, y=rel_range, linetype=type), size=1)
  rect <- data.table(xmin=ban_date, xmax=exp_date, ymin=-Inf, ymax=Inf)
  p <- p + scale_x_date(labels = date_format("%d-%b"), breaks = date_breaks("2 weeks")) + 
       geom_rect(data=rect, aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax), color="grey50", alpha=0.2, inherit.aes = FALSE) + 
       labs(x = "Date", y = "RVOL", title=paste("Proportional Trading Range (RVOL)", quartile, sep=" - "), linetype="Type")
  #print(p)
  directory <- paste(output_dir, "RVOL", sep="/")
  dir.create(directory, showWarnings = FALSE, recursive = TRUE)
  file <- paste("RVOL - ", quartile, ".png", sep="")
  file <- paste(directory, file, sep="/")
  ggsave(filename=file, plot=p, width=12, height=6)
  
  
  
  
  # Display the National effective spread for short sales only
  p <- ggplot(figure_dataset[Q == quartile]) + geom_line(aes(x=time, y=NRES_short, linetype=type), size=1)
  rect <- data.table(xmin=ban_date, xmax=exp_date, ymin=-Inf, ymax=Inf)
  p <- p + scale_x_date(labels = date_format("%d-%b"), breaks = date_breaks("2 weeks")) + 
       geom_rect(data=rect, aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax), color="grey50", alpha=0.2, inherit.aes = FALSE) + 
       labs(x = "Date", y = "RES", title=paste("National Effective Spreads for Short Sales Only (RES)", quartile, sep=" - "), linetype="Type")
  #print(p)
  directory <- paste(output_dir, "RES", sep="/")
  file <- paste("National RES Short Sales Only - ", quartile, ".png", sep="")
  file <- paste(directory, file, sep="/")
  ggsave(filename=file, plot=p, width=12, height=6)
  
  
  # Display the NASDAQ effective spread for short sales only
  p <- ggplot(figure_dataset[Q == quartile]) + geom_line(aes(x=time, y=NQRES_short, linetype=type), size=1)
  rect <- data.table(xmin=ban_date, xmax=exp_date, ymin=-Inf, ymax=Inf)
  p <- p + scale_x_date(labels = date_format("%d-%b"), breaks = date_breaks("2 weeks")) + 
       geom_rect(data=rect, aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax), color="grey50", alpha=0.2, inherit.aes = FALSE) + 
       labs(x = "Date", y = "RES", title=paste("NASDAQ Effective Spreads for Short Sales Only (RES)", quartile, sep=" - "), linetype="Type")
  #print(p)
  directory <- paste(output_dir, "RES", sep="/")
  file <- paste("NASDAQ RES Short Sales Only - ", quartile, ".png", sep="")
  file <- paste(directory, file, sep="/")
  ggsave(filename=file, plot=p, width=12, height=6)
}






# Create the dataset for the figures based on the differences used for the regressions
reg_figure_dataset <- gen_reg_dataset(regression_table)
reg_figure_dataset[, time := as.Date(time)]

# Display one of each plot for each quartile
for (quartile in plot_quartiles)
{
  # Display HFT_a and NHFT_a on the same plot, identify the ban period on the plot
  hft_nhft <- melt(reg_figure_dataset[Q == quartile, list(time, Q, hft_a, nhft_a)], id=c("time", "Q"))
  p <- ggplot(hft_nhft) + geom_line(aes(x=time, y=value, colour=variable), size=1) 
  rect <- data.table(xmin=ban_date, xmax=exp_date, ymin=-Inf, ymax=Inf)
  p <- p + scale_x_date(labels = date_format("%d-%b"), breaks = date_breaks("2 weeks")) + 
    scale_y_continuous(labels = dollar) +
    geom_rect(data=rect, aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax), color="grey50", alpha=0.2, inherit.aes = FALSE) + 
    labs(x = "Date", y = "Daily Volume Difference ($)", title=paste("HFT and NHFT Average Daily Volume Difference ($)", quartile, sep=" - "), colour="Category") +
    scale_colour_grey(start = 0, end = .6) + theme_bw()
  directory <- paste(output_dir, "HFT_NHFT_DIFF", sep="/")
  dir.create(directory, showWarnings = FALSE, recursive = TRUE)
  file <- paste("HFT NHFT Difference - ", quartile, ".png", sep="")
  file <- paste(directory, file, sep="/")
  ggsave(filename=file, plot=p, width=12, height=6)
  
  
  
  
  # Display HFT_a and NHFT_a on the same plot, identify the ban period on the plot
  hft_nhft_short <- melt(reg_figure_dataset[Q == quartile, list(time, Q, hft_a_short, nhft_a_short)], id=c("time", "Q"))
  p <- ggplot(hft_nhft_short) + geom_line(aes(x=time, y=value, colour=variable), size=1) 
  rect <- data.table(xmin=ban_date, xmax=exp_date, ymin=-Inf, ymax=Inf)
  p <- p + scale_x_date(labels = date_format("%d-%b"), breaks = date_breaks("2 weeks")) + 
    scale_y_continuous(labels = dollar) +
    geom_rect(data=rect, aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax), color="grey50", alpha=0.2, inherit.aes = FALSE) + 
    labs(x = "Date", y = "Daily Volume Difference ($)", title=paste("HFT and NHFT Average Daily Volume Difference ($)", quartile, sep=" - "), colour="Category") +
    scale_colour_grey(start = 0, end = .6) + theme_bw()
  directory <- paste(output_dir, "HFT_NHFT_DIFF", sep="/")
  file <- paste("HFT NHFT Difference Short Sale Only - ", quartile, ".png", sep="")
  file <- paste(directory, file, sep="/")
  ggsave(filename=file, plot=p, width=12, height=6)
}






# Save the figure datasets
write.csv(figure_dataset, paste("/home/jon/Source/RESEARCH/finance-research/analysis/ShortSaleBan/data", 
                                "figure_dataset.csv", sep="/"), row.names = FALSE)

write.csv(hft_figure_dataset, paste("/home/jon/Source/RESEARCH/finance-research/analysis/ShortSaleBan/data", 
                                "hft_figure_dataset.csv", sep="/"), row.names = FALSE)

write.csv(reg_figure_dataset, paste("/home/jon/Source/RESEARCH/finance-research/analysis/ShortSaleBan/data", 
                                    "difference_figure_dataset.csv", sep="/"), row.names = FALSE)

