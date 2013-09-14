###############################################################################
#
# Interfaces with a local MySQL database to simplify the process of storing and
# retrieving results that are stored in a database.
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
library(RMySQL)


# Connect to the local database
con <- dbConnect(MySQL(),
                 user="jon", password="test123",
                 dbname="research_test", host="localhost")
on.exit(dbDisconnect(con))


# Insert each of the final matched entries
setkey(figure_matched, symbol)
for (row in 1:NROW(figure_matched))
{
  entry <- figure_matched[row, ]

  sql <- sprintf("INSERT INTO matching VALUES
                 (NULL, '%s', '%s', %d, 
                 %f, %f, %f, %f, '%s', '%s');",
                 entry$symbol, entry$match, entry$frequency, entry$sum_price,
                 entry$sum_vol, entry$sum_cap, entry$sum_mean, entry$AddDate, entry$ExpDate)
  rs <- dbSendQuery(con, sql)
  dbClearResult(rs)
}


# Insert each of the market data values for the matching dataset
Sys.time()
for (row in 1:NROW(matching_dataset))
{
  entry <- matching_dataset[row, ]
  
  id <- dbGetQuery(con, 
                   sprintf("SELECT match_id FROM matching WHERE symbol = '%s';", entry$symbol))$match_id
  
  if (is.numeric(id))
  {
    sql <- sprintf("INSERT INTO market_data VALUES
                   ('%s', '%s', %d, '%s', %d, 
                     %d, %d, %f, %f, %d, 
                     %f, %f, %f, %f,
                     %f, %f, %f, %f,
                     %f, %f);",
                   entry$time, entry$symbol, id, entry$Q, entry$transactions, 
                   entry$sum_shares, entry$min_shares, entry$mean_shares, entry$med_shares, entry$max_shares,
                   entry$min_price, entry$mean_price, entry$med_price, entry$max_price,
                   entry$min_vol, entry$mean_vol, entry$med_vol, entry$max_vol,
                   entry$total_vol, entry$market_cap)
  }
  else
  {
    sql <- sprintf("INSERT INTO market_data VALUES
                   ('%s', '%s', NULL, '%s', %d, 
                     %d, %d, %f, %f, %d, 
                     %f, %f, %f, %f,
                     %f, %f, %f, %f,
                     %f, %f);",
                   entry$time, entry$symbol, entry$Q, entry$transactions, 
                   entry$sum_shares, entry$min_shares, entry$mean_shares, entry$med_shares, entry$max_shares,
                   entry$min_price, entry$mean_price, entry$med_price, entry$max_price,
                   entry$min_vol, entry$mean_vol, entry$med_vol, entry$max_vol,
                   entry$total_vol, entry$market_cap)
  }
  
  rs <- dbSendQuery(con, sql)
  dbClearResult(rs)
}
Sys.time()



id <- dbGetQuery(con, "select last_insert_id();")[1,1]

return(id)