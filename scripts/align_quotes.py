#!/usr/bin/env pypy
###############################################################################
#
# A helpful script for aligning quotes entries with the corresponding trades
# entries and producing a resultant CSV containing the trades with the
# NBBO calculations, as well as the NASDAQ information.
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
import logging
from collections import OrderedDict
from util import *


def trade_dates(file):
    """Analyzes a trades csv file and returns a list of the corresponding dates
    that are covered in the file.
    """
    dates = []

    # Open the trades file and get each date
    with open(file, 'rb') as csvfile:
        reader = csv.DictReader(csvfile)

        for line in reader:
            date = int(float(line['date']))
            if not date in dates:
                dates.append(date)

    return dates


def order_quotes(dir):
    """Analyzes each of the dates in the quotes csv files and returns an ordered
    list of the files to execute from earliest to latest date and the corresponding
    dates and the stock tickers, with their line counts, that are covered in each file.
    """
    files = OrderedDict({})

    for file in glob.glob(os.path.join(os.path.normpath(dir), '*.csv')):
        with open(file, 'rb') as csvfile:
            files[file] = {}
            reader = csv.DictReader(csvfile)

            for line in reader:
                date = int(float(line['date']))
                if not date in files[file]:
                    files[file][date] = {line['SYMBOL']: reader.line_num}
                elif not line['SYMBOL'] in files[file][date]:
                    files[file][date][line['SYMBOL']] = reader.line_num

    return files


def write_taq(taq_output, file):
    """Writes the data in the the TAQ buffer to file, appending the TAQ buffer data
    to any existing entries.
    """
    # Write the headers of the file if the file does not exist
    if not os.path.isfile(file):
        with open(file, 'wb') as csvfile:
            writer = csv.DictWriter(csvfile, delimiter=',', fieldnames=taq_output[0].keys())
            writer.writeheader()

    with open(file, 'ab') as csvfile:
        writer = csv.DictWriter(csvfile, delimiter=',', fieldnames=taq_output[0].keys())
        writer.writerows(taq_output)


def calculate_NBBO(exchanges):
    """Calculates the national best bid and best offer of all exchanges
    and uses the corresponding bid size and offer size for each.

    Accepts a dictionary containing the exchanges and the corresponding
    BID, OFR, BIDSIZ, and OFRSIZ, for each.

    example: { 'N'  =>  { 'BID'     => 50.0,
                          'OFR'     => 48.00,
                          'BIDSIZ'  => 10,
                          'OFRSIZ'  => 5}
               ... }
    """
    # The current NBBO out of all exchanges
    NBBO = {'BID': 0, 'OFR': sys.maxint, 'BIDSIZ': 0, 'OFRSIZ': 0}

    # Get the best bid and offer
    for exchange in exchanges:
        # Ignore any bids or offers less than 1 USD (error in quotes file)
        if exchanges[exchange]['BID'] >= NBBO['BID'] and exchanges[exchange]['BID'] >= 1.0:
            NBBO['BID'] = exchanges[exchange]['BID']
            NBBO['BIDSIZ'] = exchanges[exchange]['BIDSIZ']
        if exchanges[exchange]['OFR'] <= NBBO['OFR'] and exchanges[exchange]['OFR'] >= 1.0:
            NBBO['OFR'] = exchanges[exchange]['OFR']
            NBBO['OFRSIZ'] = exchanges[exchange]['OFRSIZ']

    # If there is no valid OFR for the current NBBO set it to 0
    if NBBO['OFR'] == sys.maxint:
        NBBO['OFR'] = 0

    return NBBO


def add_taq_entry(taq_output, trade, NBBO, exchanges):
    """Creates a new TAQ entry and appends it to the TAQ output,
    the format of the new entry added to the TAQ output buffer is
    as follows:

    <All trade columns>, NBB, NBO, NBBSIZ, NBOSIZ, NQBB, NQBO, NQBBSIZ, NQBOSIZ

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
    if all(k in NBBO for k in ('BID', 'OFR', 'BIDSIZ', 'OFRSIZ')):
        taq_entry['NBB'] = NBBO['BID']
        taq_entry['NBO'] = NBBO['OFR']
        taq_entry['NBBSIZ'] = NBBO['BIDSIZ']
        taq_entry['NBOSIZ'] = NBBO['OFRSIZ']
    else:
        taq_entry['NBB'] = 0
        taq_entry['NBO'] = 0
        taq_entry['NBBSIZ'] = 0
        taq_entry['NBOSIZ'] = 0

    # Add the NASDAQ NBBO entries if they exist
    if 'T' in exchanges:
        taq_entry['NQBB'] = exchanges['T']['BID']
        taq_entry['NQBO'] = exchanges['T']['OFR']
        taq_entry['NQBBSIZ'] = exchanges['T']['BIDSIZ']
        taq_entry['NQBOSIZ'] = exchanges['T']['OFRSIZ']
    else:
        taq_entry['NQBB'] = 0
        taq_entry['NQBO'] = 0
        taq_entry['NQBBSIZ'] = 0
        taq_entry['NQBOSIZ'] = 0

    # Append the TAQ entry to the TAQ output buffer
    taq_output.append(taq_entry)


# The trades file, quotes directory, and resultant taq file
trades_file = ''
quotes_file = ''
taq_file = ''
log_file = ''


# Process command line arguments
if len(sys.argv) < 4:
    sys.stderr.write("Invalid arguments given\n")
    sys.stderr.write("Usage: align_quotes.py {trades file} {quotes file} {output file}\n")
    sys.stderr.write("Example: " + sys.argv[0] + "trades_08oct2012.csv quotes_08oct2012.csv taq_08oct2012.csv\n")
    sys.exit(1)

if os.path.isfile(sys.argv[1]) and os.path.isfile(sys.argv[2]):
    # Error if output file is directory
    if os.path.isdir(sys.argv[3]):
        sys.stderr.write("The output file cannot be a directory, please specify an output file!\n")
        sys.exit(1)

    # Error if trying to overwrite an existing output file
    if os.path.isfile(sys.argv[3]):
        sys.stderr.write("The output file already exists, please specify a different output file!\n")
        sys.exit(1)
    trades_file, quotes_file, taq_file = sys.argv[1:4]
    log_file = 'errors_' + os.path.basename(taq_file)
else:
    sys.stderr.write("Trades or quotes file does not exist, check arguments and try again!\n")
    sys.exit(1)

# Configure the logging
logging.basicConfig(format='', filename=log_file, level=logging.DEBUG)


# A buffer of the final taq data to be written to file, which is periodically flushed to disk
taq_output = list()


# For each entry in the trades file, iterate through the quotes files and produce a combined file
with open(trades_file, 'rb') as trades_csv:
    trades_reader = csv.DictReader(trades_csv)

    # Open the quotes file and read the first entry
    quotes_csv = open(quotes_file, 'rb')
    quotes_reader = csv.DictReader(quotes_csv)
    quote = quotes_reader.next()

    # Used to simplify processing duplicate trades file entries
    prev_symbol = None
    prev_trade_date = None
    prev_trade_time = None

    # The list of exchanges, each exchange has its own NBBO
    exchanges = {}
    NBBO = {}

    # Calculate current NBBO/NASDAQ BBO in the quotes file for each line in trades file
    for trade in trades_reader:
        symbol = trade['symbol']
        trade_date = float_to_int(trade['date'])
        trade_time = convert_mil(trade['time'])

        # Ignore the trade if it occurs when the markets are closed
        if not market_hours(trade_time):
            continue

        # If the current date and time is the same, use the previous results
        if prev_symbol == symbol and prev_trade_date == trade_date and prev_trade_time == trade_time:
            # Use the data for the current trade, duplicate the previous NBBO calculations
            add_taq_entry(taq_output, trade, NBBO, exchanges)
            continue
        else:
            prev_trade_date = trade_date
            prev_trade_time = trade_time

        # Reset the exchanges and NBBO for the new symbol
        if prev_symbol and (prev_symbol != symbol):
            exchanges.clear()
            NBBO.clear()

        # Set the previous symbol to the current
        prev_symbol = symbol

        # Iterate through the quotes file to the matching symbol entry
        while symbol > quote['SYMBOL']:
            try:
                quote = quotes_reader.next()
            except StopIteration:
                logging.warning(quotes_file + " : Ended before trades file :" + trades_file
                    + " at following trade: " + trade['symbol'] + ", " + trade_time.isoformat())
                break

        # If no matching entry in the quotes file add the most recent results, the NBBx results can
        # be all 0 in the case where there is no matching symbol in the quotes file
        if symbol < quote['SYMBOL']:
            add_taq_entry(taq_output, trade, NBBO, exchanges)
            continue

        # Get the NBBO for each exchange entry in the quotes file
        while (symbol == quote['SYMBOL']):
            try:
                quote_date = float_to_int(quote['date'])
                quote_time = convert_sec(quote['time'])

                # Ignore any quotes that occur when the markets are closed
                if not market_hours(quote_time):
                    quote = quotes_reader.next()
                    continue

                # Get the current NBBO for each exchange in the quotes file using the most recent entry
                if trade_date == quote_date and trade_time >= quote_time:
                    # Parse the latest NBBO bid, ofr, bidsiz, ofrsiz for each exchange
                    if not quote['EX'] in exchanges and quote['EX'] != 'D':
                        exchanges[quote['EX']] = {}

                    if quote['EX'] != 'D':
                        exchanges[quote['EX']]['BID'] = float(quote['BID'])
                        exchanges[quote['EX']]['OFR'] = float(quote['OFR'])
                        exchanges[quote['EX']]['BIDSIZ'] = float(quote['BIDSIZ'])
                        exchanges[quote['EX']]['OFRSIZ'] = float(quote['OFRSIZ'])

                # Error if NO matching entries in the quotes file has been found
                elif not exchanges and (trade_date < quote_date or trade_time < quote_time):
                    logging.warning(trades_file + " : No quotes entry found for the following trade: " + trade['symbol']
                        + ", " + trade_time.isoformat())
                    break
                # Break when the last matching quote entry for the time has been parsed
                elif exchanges and (symbol != quote['SYMBOL'] or trade_date < quote_date or trade_time < quote_time):
                    break

                # Read the next line of the quotes file
                quote = quotes_reader.next()

            # Error should not have reached the end of quotes file before trades file
            # TODO add support for the case where a single trades file spans across more than one quotes file
            except StopIteration:
                logging.warning(quotes_file + " : Ended before trades file :" + trades_file
                    + " at following trade: " + trade['symbol'] + ", " + trade_time.isoformat())
                break

        # Calculate the current NBBO out of all exchanges, add the results as a new entry in the taq output buffer
        NBBO = calculate_NBBO(exchanges)
        add_taq_entry(taq_output, trade, NBBO, exchanges)

        # If the TAQ buffer is > 250K lines, flush the buffer to disk
        if taq_output.__len__() >= 250000:
            write_taq(taq_output, taq_file)
            # Clear the buffer in memory
            del taq_output[:]

    # Close the open quotes files
    quotes_csv.close()


# Write any remaining content in the TAQ buffer to disk
if taq_output:
    write_taq(taq_output, taq_file)
