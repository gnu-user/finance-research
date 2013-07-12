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


# Create the dataset for the figures, the data is calculated for each stock each day 
gen_figure_dataset <- function(daily_data, matches)
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
  
  # For each banned symbol get the match and calculate variables for the figures
  for (ban_symbol in banned)
  {
    matched <- matches[ban_symbol]$match
        
    # Calculate the mean of the variables RES, RPI5, and relative range
    result <- daily_data[symbol == ban_symbol, 
                         list(type="Banned", NRES=mean(mean_NRES), NQRES=mean(mean_NQRES), 
                              NRPI5=mean(mean_NRPI5), NQRPI5=mean(mean_NQRPI5), 
                              rel_range=mean(rel_range)), 
                         by="time,symbol"]
    
    result <- rbind(result, 
                    daily_data[symbol == matched, 
                               list(type="Matched", NRES=mean(mean_NRES), NQRES=mean(mean_NQRES), 
                                    NRPI5=mean(mean_NRPI5), NQRPI5=mean(mean_NQRPI5), 
                                    rel_range=mean(rel_range)), 
                               by="time,symbol"])
    
    
    # Calculate the shorting and total volume, which is used to calculate RELSS
    daily_volume <- daily_data[symbol == ban_symbol & ShortSale == TRUE,
                               list(type="Banned", short_vol=sum(sum_vol)), by="time,symbol"]
    
    daily_volume <- merge(daily_volume, 
                          daily_data[symbol == ban_symbol,
                                     list(total_vol=sum(sum_vol)), by="time"],
                          by="time")
    
    temp <- daily_data[symbol == matched & ShortSale == TRUE,
                       list(type="Matched", short_vol=sum(sum_vol)), by="time,symbol"]
    
    temp <- merge(temp, 
                  daily_data[symbol == matched, 
                             list(total_vol=sum(sum_vol)), by="time"], 
                  by="time")
    
    daily_volume <- rbind(daily_volume, temp)
    
    
    # Calculate RELSS, which is shortsale volume divided by the total trading volume
    daily_volume[, RELSS := (short_vol / total_vol)]
    setkey(daily_volume, time, symbol, type)
    
    
    # Calculate the effective spreads (RES) for short sales only
    shorting_RES <- daily_data[symbol == ban_symbol & ShortSale == TRUE,
                               list(type="Banned", NRES_short=mean(mean_NRES), NQRES_short=mean(mean_NQRES)), 
                               by="time,symbol"]
    
    shorting_RES <- rbind(shorting_RES, 
                          daily_data[symbol == matched & ShortSale == TRUE,
                                     list(type="Matched", NRES_short=mean(mean_NRES), NQRES_short=mean(mean_NQRES)), 
                                     by="time,symbol"])
    
    setkey(shorting_RES, time, symbol, type)
    
                          
    # Add RELSS and shorting RES to the result for the banned and matched symbol
    setkey(result, time, symbol, type)
    result <- merge(result, 
                    daily_volume[, list(RELSS), by="time,symbol,type"])
                          
    result <- merge(result, 
                    shorting_RES[, list(NRES_short, NQRES_short), by="time,symbol,type"])
    
    
    # Add results for current symbol to the list of results
    if (exists("results"))
    {
      results <- rbind(results, result)
    }
    else
    {
      results <- result
    }
    setkey(results, time, symbol, type)
  }
  
  # Return the mean of all stocks for each day
  results[, time := as.Date(time)]
  return(results[, list(NRES=mean(NRES), NQRES=mean(NQRES),
                        NRES_short=mean(NRES_short), NQRES_short=mean(NQRES_short),
                        NRPI5=mean(NRPI5), NQRPI5=mean(NQRPI5), 
                        rel_range=mean(rel_range), RELSS=mean(RELSS)), 
                 by="time,type"])
}




# The start and end of the ban period 
ban_date <- as.Date("2008-09-19")
exp_date <- as.Date("2008-10-09")


# Create the first dataset for the RELSS figures and plot the results
figure_dataset <- gen_figure_dataset(daily_results, final_matched)

# Display the RELSS results for all transactions, identify the ban period on the plot
p <- ggplot(figure_dataset) + geom_line(aes(x=time, y=RELSS, linetype=type), size=1)
rect <- data.table(xmin=ban_date, xmax=exp_date, ymin=-Inf, ymax=Inf)
p + scale_x_date(labels = date_format("%d-%b"), breaks = date_breaks("2 weeks")) + 
  geom_rect(data=rect, aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax), color="grey50", alpha=0.2, inherit.aes = FALSE) + 
  labs(x = "Date", y = "RELSS", title="Short-selling Activity (RELSS)", linetype="Type")



# Display the National RES results for all transacitons, identify the ban period on the plot
p <- ggplot(figure_dataset) + geom_line(aes(x=time, y=NRES, linetype=type), size=1)
rect <- data.table(xmin=ban_date, xmax=exp_date, ymin=-Inf, ymax=Inf)
p + scale_x_date(labels = date_format("%d-%b"), breaks = date_breaks("2 weeks")) + 
  geom_rect(data=rect, aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax), color="grey50", alpha=0.2, inherit.aes = FALSE) + 
  labs(x = "Date", y = "RES", title="National Effective Spreads (RES)", linetype="Type")

# Display the NASDAQ RES results for all transacitons, identify the ban period on the plot
p <- ggplot(figure_dataset) + geom_line(aes(x=time, y=NQRES, linetype=type), size=1)
rect <- data.table(xmin=ban_date, xmax=exp_date, ymin=-Inf, ymax=Inf)
p + scale_x_date(labels = date_format("%d-%b"), breaks = date_breaks("2 weeks")) + 
  geom_rect(data=rect, aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax), color="grey50", alpha=0.2, inherit.aes = FALSE) + 
  labs(x = "Date", y = "RES", title="NASDAQ Effective Spreads (RES)", linetype="Type")




# Display the National RPI5 results for all transacitons, identify the ban period on the plot
p <- ggplot(figure_dataset) + geom_line(aes(x=time, y=NRPI5, linetype=type), size=1)
rect <- data.table(xmin=ban_date, xmax=exp_date, ymin=-Inf, ymax=Inf)
p + scale_x_date(labels = date_format("%d-%b"), breaks = date_breaks("2 weeks")) + 
  geom_rect(data=rect, aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax), color="grey50", alpha=0.2, inherit.aes = FALSE) + 
  labs(x = "Date", y = "RPI5", title="National Price Impacts (RPI5)", linetype="Type")

# Display the NASDAQ RPI5 results for all transacitons, identify the ban period on the plot
p <- ggplot(figure_dataset) + geom_line(aes(x=time, y=NQRPI5, linetype=type), size=1)
rect <- data.table(xmin=ban_date, xmax=exp_date, ymin=-Inf, ymax=Inf)
p + scale_x_date(labels = date_format("%d-%b"), breaks = date_breaks("2 weeks")) + 
  geom_rect(data=rect, aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax), color="grey50", alpha=0.2, inherit.aes = FALSE) + 
  labs(x = "Date", y = "RPI5", title="NASDAQ Price Impacts (RPI5)", linetype="Type")



# Display the relative range (RVOL)
p <- ggplot(figure_dataset) + geom_line(aes(x=time, y=rel_range, linetype=type), size=1)
rect <- data.table(xmin=ban_date, xmax=exp_date, ymin=-Inf, ymax=Inf)
p + scale_x_date(labels = date_format("%d-%b"), breaks = date_breaks("2 weeks")) + 
  geom_rect(data=rect, aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax), color="grey50", alpha=0.2, inherit.aes = FALSE) + 
  labs(x = "Date", y = "RVOL", title="Proportional Trading Range (RVOL)", linetype="Type")




# Display the National effective spread for short sales only
p <- ggplot(figure_dataset) + geom_line(aes(x=time, y=NRES_short, linetype=type), size=1)
rect <- data.table(xmin=ban_date, xmax=exp_date, ymin=-Inf, ymax=Inf)
p + scale_x_date(labels = date_format("%d-%b"), breaks = date_breaks("2 weeks")) + 
  geom_rect(data=rect, aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax), color="grey50", alpha=0.2, inherit.aes = FALSE) + 
  labs(x = "Date", y = "RES", title="National Effective Spreads for Short Sales Only (RES)", linetype="Type")

# Display the NASDAQ effective spread for short sales only
p <- ggplot(figure_dataset) + geom_line(aes(x=time, y=NQRES_short, linetype=type), size=1)
rect <- data.table(xmin=ban_date, xmax=exp_date, ymin=-Inf, ymax=Inf)
p + scale_x_date(labels = date_format("%d-%b"), breaks = date_breaks("2 weeks")) + 
  geom_rect(data=rect, aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax), color="grey50", alpha=0.2, inherit.aes = FALSE) + 
  labs(x = "Date", y = "RES", title="NASDAQ Effective Spreads for Short Sales Only (RES)", linetype="Type")

