#!/usr/bin/env python2
###############################################################################
#
# A helpful script for aligning quotes entries with the corresponding trades
# entries and producing a resultant CSV containing the trades with the 
# NBBx calculations, as well as the NASDAQ information.
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
import sys
import os
import glob
import csv
import time
import datetime
import math
import logging
from collections import OrderedDict

def order_trades(dir):
    """Analyzes each of the dates in the trades csv files and returns an ordered
    list of the files to execute from earliest to latest date and the corresponding dates 
    that are covered in each file.
    """
    files = {}
    ordered_files = OrderedDict({})

    for file in glob.glob(dir + "*.csv"):
        with open(file, 'rb') as csvfile:
            files[file] = []
            reader = csv.DictReader(csvfile)

            #print file
            for line in reader:
                date = int(float(line['date']))
                if not date in files[file]:
                    #print file, line['date']
                    files[file].append(date)

    # Sort the files store them in an ordered dictionary
    for file in sorted(files, key=files.get):
        ordered_files[file] = files[file]

    return ordered_files
    #print "RESULTS"
    #for file in ordered_files:
    #    print file, ordered_files[file]

def order_quotes(dir):
    """Analyzes each of the dates in the quotes csv files and returns an ordered
    list of the files to execute from earliest to latest date and the corresponding 
    dates and the stock tickers, with their line counts, that are covered in each file.
    """
    files = OrderedDict({})
    ordered_files = OrderedDict({})

    for file in glob.glob(dir + "*.csv"):
        with open(file, 'rb') as csvfile:
            files[file] = {}
            reader = csv.DictReader(csvfile)

            #print file
            for line in reader:
                date = int(float(line['date']))
                if not date in files[file]:
                    #print file, line['date']
                    files[file][date] = {line['SYMBOL'] : reader.line_num}
                elif not line['SYMBOL'] in files[file][date]:
                    files[file][date][line['SYMBOL']] = reader.line_num

    # Sort the files store them in an ordered dictionary
    for file in sorted(files, key=files[file].get):
        ordered_files[file] = files[file]

    return ordered_files
    #print "RESULTS"
    #for file in ordered_files:
    #    print file, ordered_files[file]


def convert_mil(ms):
    """Converts a time in milliseconds from midnight format into a 
    compatible time format of HH:MM:SS, currently the time provided
    in milliseconds is floored to avoid having times in the future.
    """
    # Floor the results to avoid rounding errors for seconds entries
    ms = int(float(ms))
    hour = (ms / 3600000) % 24
    min = (ms / 60000) % 60
    sec = (ms / 1000) % 60

    return datetime.time(hour, min, sec)

def convert_sec(sec):
    """Converts a time in seconds from midnight format into compatible
    time format of HH:MM:SS
    """

    # Ensure the value is an integer
    sec = int(float(sec))
    hour = (sec / 3600) % 24
    min = (sec / 60) % 60
    sec = sec % 60

    return datetime.time(hour, min, sec)


trades_dir = ""
quotes_dir = ""
results_file = ""

# Process command line arguments
if len(sys.argv) < 4:
    sys.stderr.write("Invalid arguments given\n")
    sys.stderr.write("Usage: align_quotes.py {trades directory} {quotes directory} {output file}\n")
    sys.stderr.write("Example: " + sys.argv[0] + "trades_2012/ quotes_2012/ taq_2012.csv\n")
    sys.exit()

if os.path.isdir(sys.argv[1]) and os.path.isdir(sys.argv[2]):
    trades_dir, quotes_dir, results_file = sys.argv[1:4]
else:
     sys.stderr.write("Trades or quotes directory does not exist, check arguments and try again!")


# Configure the logging
logging.basicConfig(filename='errors.log',level=logging.DEBUG)


# Parse each trade/quotes file and determine the order in which to process the files
trades = order_trades(trades_dir)
quotes = order_quotes(quotes_dir)

quotes_file = None
quotes_reader = None


# An array of the results written to the final output
results = list()

# For each trades file iterate through the quotes files and produce a combined file
for file in trades:
    # Find the next quotes file which includes the date needed for the trades file
    if not quotes_file or not trades[file] in quotes[quotes_file]:
        # Check if the date exists in each quotes file
        for key in quotes:
            print trades[file][0], quotes[key]
            if trades[file][0] in quotes[key]:
                # Open the quotes file and read the first entry
                quotes_file = key
                quotes_csv = open(quotes_file, 'rb')
                quotes_reader = csv.DictReader(quotes_csv)
                quote = quotes_reader.next()

    # If the trades file date was not found in the quotes file, error, skip file
    if not quotes_file or not trades[file][0] in quotes[quotes_file]:
        logging.warning(file + " : No quotes entry found for the trades file date, skipping file!")
        continue

    # Open the trades file and find each corresponding entry in the quotes file
    trades_csv = open(file, 'rb')
    trades_reader = csv.DictReader(trades_csv)

    # Used to simplify processing duplicate trades file entries
    prev_trade_date = None
    prev_trade_time = None
    exchanges = {}

    # Find a matching quotes entry for each line in trades file
    
    for trade in trades_reader:
        trade_date = int(float(trade['date']))
        trade_time = convert_mil(trade['time'])

        # If the current date and time is the same, use the previous results
        if prev_trade_date == trade_date and prev_trade_time == trade_time:
            # TODO add handling
            continue
        else:
            prev_trade_date = trade_date
            prev_trade_time = trade_time
            # Reset the exchanges parsed
            exchanges = {}

        # Get the exchange data for each entry in the quotes file that has a matching time and date
        while (True):
            try:
                quote_date = int(float(quote['date']))
                quote_time = convert_sec(quote['time'])

                # For each entry with the same symbol, date and time add it to the list of exchanges
                if trade['symbol'] == quote['SYMBOL'] and trade_date == quote_date and trade_time == quote_time:
                    # Store the bid, ofr, bidsiz, ofrsiz for the exchange
                    exchanges[quote['EX']] = {}
                    exchanges[quote['EX']]['BID'] = quote['BID']
                    exchanges[quote['EX']]['OFR'] = quote['OFR']
                    exchanges[quote['EX']]['BIDSIZ'] = quote['BIDSIZ']
                    exchanges[quote['EX']]['OFRSIZ'] = quote['OFRSIZ']
                # Error if NO matching entries in the quotes file has been found
                # TODO add support to take the data from the previous time entry (within 1 second)
                elif not exchanges and trade['symbol'] == quote['SYMBOL'] and trade_date < quote_date or trade_time < quote_time:
                    logging.warning(file + " : No quotes entry found for the following trade at time: " + trade['symbol'] 
                        + ", " + trade_time.isoformat())
                    break
                # Break when the last matching quote entry for the time has been parsed
                elif exchanges and trade['symbol'] != quote['SYMBOL'] or trade_date < quote_date or trade_time < quote_time: 
                    break
                quote = quotes_reader.next()
            # Error the quotes file should not stop be finished before trades file
            except StopIteration:
                logging.warning(quotes_file + " : Ended before trades file at following trade: "
                    + trade['symbol'] + ", " + trade_time.isoformat())
                break

        # Print the results in exchanges
        print exchanges


    #with open(file, 'rb') as csvfile:
    #    reader = csv.DictReader(csvfile)
    #    for line in reader:
    #        date = int(float(line['time']))
    #        print  line['SYMBOL'], date, convert_sec(date).isoformat(), line['BID'], line['OFR'], line['BIDSIZ'], line['OFRSIZ'], line['EX']

