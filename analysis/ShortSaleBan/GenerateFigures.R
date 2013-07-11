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

# Create the dataset for the RELSS figure, RELSS is calculated for each stock each day 
# as the shortsale volume divided by the total trading volume. The mean of the results
# of all stocks for each day is then used for the final RELSS dataset.
gen_RELSS_dataset <- function(daily_data, matches)
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
  
  # For each banned symbol get the match and calculate RELSS
  for (ban_symbol in banned)
  {
    matched <- matches[ban_symbol]$match
    
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
    
    
    if (exists("results"))
    {
      results <- rbind(results, daily_volume[, list(time, symbol, type, RELSS)])
    }
    else
    {
      results <- daily_volume[, list(time, symbol, type, RELSS)]
    }
    setkey(results, time, symbol, type)
  }
  
  # Return the mean RELSS of all stocks for each day
  results[, time := as.Date(time)]
  return(results[, list(RELSS=mean(RELSS)), by="time,type"])
}




# The start and end of the ban period 
ban_date <- as.Date("2008-09-19")
exp_date <- as.Date("2008-10-09")


# Create the first dataset for the RELSS figures and plot the results
RELSS_dataset <- gen_RELSS_dataset(daily_results, final_matched)

# Display the RELSS results for all types of transacitons, identify the ban period on the plot
p <- ggplot(RELSS_dataset) + geom_line(aes(x=time, y=RELSS, linetype=type), size=1)
rect <- data.table(xmin=ban_date, xmax=exp_date, ymin=-Inf, ymax=Inf)
p + scale_x_date(labels = date_format("%d-%b"), breaks = date_breaks("2 weeks")) + 
  geom_rect(data=rect, aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax), color="grey50", alpha=0.2, inherit.aes = FALSE) + 
  labs(x = "Date", y = "RELSS", title="Short-selling Activity (RELSS)", linetype="Type")