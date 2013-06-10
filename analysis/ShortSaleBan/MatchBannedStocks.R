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


# Get a list of the banned symbols that having missing days during the preban period and the
# number of active trading days that the symbol is missing from
miss_days_preban <- function(banned_symbols, symbols_list)
{
  missing_preban <- data.table()
  
  for (entry in banned_symbols)
  {
    start_date <- symbols_list[symbol == entry, time][1]
    ban_date <- symbols_list[symbol == entry, AddDate][1]
    ban_days <- NROW(symbols_list[symbol == entry & time >= start_date & time < ban_date])
    
    # List of all trading days during the ban period
    all_ban_days <- NROW(symbols_list[time >= start_date & time < ban_date, list(time=unique(time))])
    
    if (ban_days < all_ban_days)
    {
      # Add the symbol to the list of symbols that do not trade during the ban period
      missing_days <- all_ban_days - ban_days
      
      if(NROW(missing_preban) > 0)
      {
        missing_preban <- rbind(missing_preban, data.table(symbol=entry, missing_days=missing_days))
      }
      else
      {
        missing_preban <- data.table(symbol=entry, missing_days=missing_days)
      }
    }
  }
  
  return(missing_preban)
}


# Get a list of the banned symbols that having missing days at all during the time the symbol
# was first listed and the last date the sybmol was listed
miss_days_all <- function(banned_symbols, symbols_list)
{
  missing_all <- data.table()
  
  for (entry in banned_symbols)
  {
    start_date <- symbols_list[symbol == entry, time][1]
    end_date <- last(symbols_list[symbol == entry, time])
    symbol_days <- NROW(symbols_list[symbol == entry & time %between% c(start_date,end_date)])
    
    # List of all trading days during the ban period
    all_days <- NROW(symbol_means[time %between% c(start_date,end_date), list(time=unique(time))])
    
    if (symbol_days < all_days)
    {
      # Add the symbol to the list of symbols that do not trade during the ban period
      missing_days <- all_days - symbol_days
      
      if(NROW(missing_all) > 0)
      {
        missing_all <- rbind(missing_all, data.table(symbol=entry, missing_days=missing_days))
      }
      else
      {
        missing_all <- data.table(symbol=entry, missing_days=missing_days)
      }
    }
  }
  
  return(missing_all)
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


# Add columns to final results for identifying which stocks were banned, and the start and end period
final_results[, banned := FALSE]

# Mark each banned stock as TRUE, with the add and expiry date
for (ticker in ban_list$Ticker)
{
  final_results[ticker, banned := TRUE]
  final_results[ticker, AddDate := ban_list[Ticker == ticker, AddDate]]
  final_results[ticker, ExpDate := ban_list[Ticker == ticker, ExpDate]]
}

# Update the key indexes to include banned
setkey(final_results, time, symbol, buysell, type, ShortSale, banned)


# Create a new data table containing the mean price and volume for each symbol for all types
symbol_means <- final_results[,list(mean_price=mean(mean_price), mean_vol=mean(mean_vol), matched=FALSE), by="time,symbol,banned,AddDate"]
setkey(symbol_means,time,banned,matched,symbol)

# Get the list of banned symbols
banned <- symbol_means[banned == TRUE, list(symbol=unique(symbol))]$symbol

# Set N, the number or random permutations of the banned symbols list to generate, currently 2 * number banned symbols
N = 2 * NROW(banned)

# For each random permutation of the banned stocks keep a record of the best match for each banned symbol
for (i in 1:N)
{
  permutation <- sample(banned)

  # Find a matching un-banned stock based on pre-ban price and volume for each symbol of the permutation
  for (ban_symbol in permutation)
  {
    # The date the symbol was first listed
    start_date <- symbol_means[symbol == ban_symbol, time][1]
    
    # The date the symbol was banned
    ban_date <- symbol_means[symbol == ban_symbol, AddDate][1]
    
    # The number of days the symbol was banned (based on the days it was listed)
    days_banned <- NROW(symbol_means[symbol == ban_symbol & time >= start_date & time < ban_date])
    
    # Get the list of dates the symbol was listed as banned
    dates <- symbol_means[symbol == ban_symbol & time >= start_date & time < ban_date, time]
    
    # Create a temporary list of candidate stocks that exist on the same days during the pre-ban period
    for (ticker in unique(symbol_means[J(dates,FALSE,FALSE), nomatch=0]$symbol))
    {
      # IF the stock exists during the pre-ban period on all of the same days add it as a candidate
      if (NROW(symbol_means[J(dates,FALSE,FALSE,ticker), nomatch=0]) == days_banned)
      {
        if (exists("candidates"))
        {
          candidates <- c(candidates, ticker)
        }
        else
        {
          candidates <- ticker
        }
      }
    }
        
    # Store the candidates in the data table that will contain the differences
    total_differences <- symbol_means[time %in% dates & symbol %in% candidates, list(time, symbol, mean_price, mean_vol)]
    setkey(total_differences,symbol)
    
    # Get a vector of the mean price and volume for the banned symbol for each day
    ban_prices <- symbol_means[J(dates,TRUE,FALSE,ban_symbol)]$mean_price  
    ban_vols <- symbol_means[J(dates,TRUE,FALSE,ban_symbol)]$mean_vol
    
    # Calculate the differences between the banned stock and each candidate stock
    total_differences[, num_price := (mean_price - ban_prices), by="symbol"]
    total_differences[, per_price := Delt(ban_prices, mean_price, type='arithmetic'), by="symbol"]
    total_differences[, num_vol := (mean_vol - ban_vols), by="symbol"]
    total_differences[, per_vol := Delt(ban_vols, mean_vol, type='arithmetic'), by="symbol"]
    
    # Calculate the sum of absolute % differences
    sum_differences <- total_differences[, list(sum_price=sum(abs(per_price)), sum_vol=sum(abs(per_vol))), by="symbol"]
    
    # Calculate the mean of the % price and volume sums
    sum_differences[, sum_mean := rowMeans(.SD), by="symbol"]
    
    # The best match is the symbol with the lowest average min price and volume
    setkey(sum_differences, sum_mean)
    best_match <- sum_differences[1,symbol]
    
    results <- data.table(ban_symbol = ban_symbol, best_match = best_match, frequency = 1)
    
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
      setkey(matched_hist, ban_symbol, best_match)
    }
    
    # Mark the best matched symbol used as matched
    symbol_means[symbol == best_match, matched := TRUE]
    
    # Clean up temporary data.tables
    rm(candidates, total_differences, sum_differences, results)
  }
  
  # Reset the list of matched symbols for the next permutation of the banned symbols
}