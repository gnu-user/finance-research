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
            files[file] = None
            reader = csv.DictReader(csvfile)

            #print file
            for line in reader:
                date = int(float(line['date']))
                if not files[file]:
                    #print file, line['date']
                    files[file] = date

    # Sort the files store them in an ordered dictionary
    for file in sorted(files, key=files.get):
        ordered_files[file] = files[file]

    #print "RESULTS"
    #or file in ordered_files:
    #    print file, ordered_files[file]

    return ordered_files


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

    
    #print "RESULTS"
    #for file in ordered_files:
    #    print file, ordered_files[file]

    return ordered_files


def calculate_NBBO(exchanges):
    """Calculates the national best bid and best offer and returns the 
    corresponding bid size and offer size for each.

    Accepts a dictionary containing the exchanges and the corresponding
    BID, OFR, BIDSIZ, and OFRSIZ, for each.

    example: { 'N'  =>  { 'BID'     => 50.0,
                          'OFR'     => 48.00,
                          'BIDSIZ'  => 10,
                          'OFRSIZ'  => 5}}
 
                    exchanges[quote['EX']]['BID'] = quote['BID']
                    exchanges[quote['EX']]['OFR'] = quote['OFR']
                    exchanges[quote['EX']]['BIDSIZ'] = quote['BIDSIZ']
                    exchanges[quote['EX']]['OFRSIZ'] = quote['OFRSIZ']
    """
    NBBO = {'BID': 0, 'OFR': sys.maxint, 'BIDSIZ': 0, 'OFRSIZ': 0}

    # Get the best bid and offer
    for exchange in exchanges:
        if exchanges[exchange]['BID'] >=  NBBO['BID']:
            NBBO['BID'] = exchanges[exchange]['BID']
            NBBO['BIDSIZ'] = exchanges[exchange]['BIDSIZ']
        if exchanges[exchange]['OFR'] <= NBBO['OFR']:
            NBBO['OFR'] = exchanges[exchange]['OFR']
            NBBO['OFRSIZ'] = exchanges[exchange]['OFRSIZ']

    return NBBO


def add_taq_entry(taq_output, trade, NBBO, exchanges):
    """Creates a new TAQ entry and appends it to the TAQ output,
    the format of the new entry added to the TAQ output buffer is
    as follows:

    <All trade data columns> <BESTBID, BESTOFR, BESTBIDSIZ, BESTOFRSIZ> <NSDQBID, NSDQOFR, NSDQBIDSIZ, NSDQOFRSIZ>

    If there is no available TAQ entry for NASDAQ then the columns in the TAQ output
    will contain 0 for each NSDQ entry.
    """
    taq_entry = OrderedDict({})

    # Add all trade data columns
    taq_entry['time']    = trade['time'] 
    taq_entry['symbol']  = trade['symbol']
    taq_entry['shares']  = trade['shares']
    taq_entry['buysell'] = trade['buysell'] 
    taq_entry['price']   = trade['price'] 
    taq_entry['type']    = trade['type']
    taq_entry['date']    = trade['date']

    # Add all NBBO columns
    taq_entry['BESTBID'] = NBBO['BID']
    taq_entry['BESTOFR'] = NBBO['OFR']
    taq_entry['BESTBIDSIZ'] = NBBO['BIDSIZ']
    taq_entry['BESTOFRSIZ'] = NBBO['OFRSIZ']

    # Add all the NASDAQ entries if they exist
    if exchanges.has_key('T'):
        taq_entry['NSDQBID'] = exchanges['T']['BID']
        taq_entry['NSDQOFR'] = exchanges['T']['OFR']
        taq_entry['NSDQBIDSIZ'] = exchanges['T']['BIDSIZ']
        taq_entry['NSDQOFRSIZ'] = exchanges['T']['OFRSIZ']
    else:
        taq_entry['NSDQBID'] = 0
        taq_entry['NSDQOFR'] = 0
        taq_entry['NSDQBIDSIZ'] = 0
        taq_entry['NSDQOFRSIZ'] = 0

    # Append the TAQ entry to the TAQ output buffer
    taq_output.append(taq_entry)


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


def float_to_int(value):
    """Converts a string representation of a float value to an int 
    FLOORING the value (e.g. 2.99 becomes 2)
    """

    return int(float(value))

def market_hours(time):
    """Determines if the time provide is within the market operating hours, 
    which are usually between 9:30 and 16:00.

    :param time: A datetime.time object of the time to check
    """
    open = datetime.time(9,30,00)
    close = datetime.time(16,00,00)

    if time < open or time > close:
        return False

    return True



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
quote = None

# An buffer of the final taq data to be written to file, which is periodically flushed to disk
taq_output = list()

# For each trades file iterate through the quotes files and produce a combined file
for file in trades:
    # Find the next quotes file which includes the date needed for the trades file
    if not quotes_file or not trades[file] in quotes[quotes_file]:
        # Check if the date exists in each quotes file
        for key in quotes:
            print trades[file], quotes[key]
            if trades[file] in quotes[key]:
                # Open the quotes file and read the first entry
                quotes_file = key
                quotes_csv = open(quotes_file, 'rb')
                quotes_reader = csv.DictReader(quotes_csv)
                quote = quotes_reader.next()

    # If the trades file date was not found in the quotes file, error, skip file
    if not quotes_file or not trades[file] in quotes[quotes_file]:
        logging.warning(file + " : No quotes entry found for the trades file date, skipping file!")
        continue

    # Open the trades file and find each corresponding entry in the quotes file
    trades_csv = open(file, 'rb')
    trades_reader = csv.DictReader(trades_csv)

    # Used to simplify processing duplicate trades file entries
    symbol = None
    prev_trade_date = None
    prev_trade_time = None
    exchanges = {}
    NBBO = {}

    # Find a matching quotes entry for each line in trades file
    for trade in trades_reader:
        symbol = trade['symbol']
        trade_date = float_to_int(trade['date'])
        trade_time = convert_mil(trade['time'])

        # Ignore the trade if it occurs is when the markets are closed
        if not market_hours(trade_time):
            continue

        # If the current date and time is the same, use the previous results
        if prev_trade_date == trade_date and prev_trade_time == trade_time:
            # TODO add handling
            # use the data for the current trade entry, duplicate the previous NBBx calculations 
            continue
        else:
            prev_trade_date = trade_date
            prev_trade_time = trade_time
            # Initialize new exchanges, NBBO calculations
            exchanges = {}
            NBBO = {}

        # Iterate through the quotes file to the matching symbol entry and date
        if symbol != quote['SYMBOL']:
            while quotes_reader.line_num < quotes[quotes_file][trade_date][symbol]:
                quotes_reader.next()

        # Get the data for each entry in the quotes file that has a matching time and date
        while (True):
            try:
                quote_date = float_to_int(quote['date'])
                quote_time = convert_sec(quote['time'])

                # Ignore any quotes that occur when the markets are closed
                if not market_hours(quote_time):
                    # Read the next line of the quotes file
                    quote = quotes_reader.next()
                    continue

                # For each entry with the same symbol, date and time add it to the list of exchanges
                if trade['symbol'] == quote['SYMBOL'] and trade_date == quote_date and trade_time == quote_time:
                    # Store bid, ofr, bidsiz, ofrsiz for the exchange, for multiple entries for the same
                    # exchange the LAST value added is used
                    exchanges[quote['EX']] = {}
                    exchanges[quote['EX']]['BID'] = quote['BID']
                    exchanges[quote['EX']]['OFR'] = quote['OFR']
                    exchanges[quote['EX']]['BIDSIZ'] = quote['BIDSIZ']
                    exchanges[quote['EX']]['OFRSIZ'] = quote['OFRSIZ']
                # Error if NO matching entries in the quotes file has been found
                # TODO add support to take the data from the previous time entry (within delta seconds),
                # if there is no data for the current time
                elif not exchanges and trade['symbol'] == quote['SYMBOL'] and (trade_date < quote_date or trade_time < quote_time):
                    logging.warning(file + " : No quotes entry found for the following trade: " + trade['symbol'] 
                        + ", " + trade_time.isoformat())
                    break
                # Break when the last matching quote entry for the time has been parsed
                elif exchanges and (trade['symbol'] != quote['SYMBOL'] or trade_date < quote_date or trade_time < quote_time): 
                    break

                # Read the next line of the quotes file
                quote = quotes_reader.next()

            # Error should not have reached the end of quotes file before trades file
            # TODO add support for the case where a single trades file spans across more than one quotes file
            except StopIteration:
                logging.warning(quotes_file + " : Ended before trades file :" + file
                    + " at following trade: " + trade['symbol'] + ", " + trade_time.isoformat())
                break

        # Print the results in exchanges
        #print trade_date, trade_time, exchanges

        # Calculate the NBBO, add the results as a new entry in the taq output buffer
        NBBO = calculate_NBBO(exchanges)
        add_taq_entry(taq_output, trade, NBBO, exchanges)

    #with open(file, 'rb') as csvfile:
    #    reader = csv.DictReader(csvfile)
    #    for line in reader:
    #        date = int(float(line['time']))
    #        print  line['SYMBOL'], date, convert_sec(date).isoformat(), line['BID'], line['OFR'], line['BIDSIZ'], line['OFRSIZ'], line['EX']

