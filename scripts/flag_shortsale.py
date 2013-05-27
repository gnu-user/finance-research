#!/usr/bin/env pypy
###############################################################################
#
# Processes the aligned taq files and flags entries that are short sales using the
# data from files containing the list of short sale trades that occurred.
# Trades in the aligned taq files that are identified as short sales are flagged
# with 1 and the corresponding short sale size and short sale type are recorded.
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
import csv
import logging
from collections import OrderedDict
from util import *


def write_buffer(buffer, file):
    """Writes the data in the the buffer to file, appending the buffer data
    to any existing entries.
    """
    # Write the headers of the file if the file does not exist
    if not os.path.isfile(file):
        with open(file, 'wb') as csvfile:
            writer = csv.DictWriter(csvfile, delimiter=',', fieldnames=buffer[0].keys())
            writer.writeheader()

    with open(file, 'ab') as csvfile:
        writer = csv.DictWriter(csvfile, delimiter=',', fieldnames=buffer[0].keys())
        writer.writerows(buffer)


def add_entry(buffer, trade, shortsale):
    """Adds an entry from the aligned taq file to the buffer and
    flags sets the status of the trade as a shortsale if it is one.
    """
    taq_entry = OrderedDict({})

    # Add the entries from the trade file in order
    taq_entry['time']    = trade['time']
    taq_entry['symbol']  = trade['symbol']
    taq_entry['shares']  = trade['shares']
    taq_entry['buysell'] = trade['buysell']
    taq_entry['price']   = trade['price']
    taq_entry['type']    = trade['type']
    taq_entry['date']    = trade['date']

    taq_entry['NBB'] = trade['NBB']
    taq_entry['NBO'] = trade['NBO']
    taq_entry['NBBSIZ'] = trade['NBBSIZ']
    taq_entry['NBOSIZ'] = trade['NBOSIZ']

    taq_entry['NQBB'] = trade['NQBB']
    taq_entry['NQBO'] = trade['NQBO']
    taq_entry['NQBBSIZ'] = trade['NQBBSIZ']
    taq_entry['NQBOSIZ'] = trade['NQBOSIZ']


    # Flag the trade as a shortsale if it is
    if 'ShortType' in shortsale and 'ShortSize' in shortsale:
        taq_entry['ShortSale'] = 1
        taq_entry['ShortType'] = shortsale['ShortType']
        taq_entry['ShortSize'] = shortsale['ShortSize']
    else:
        taq_entry['ShortSale'] = 0
        taq_entry['ShortType'] = ''
        taq_entry['ShortSize'] = 0

    # Append the TAQ entry to the TAQ output buffer
    buffer.append(taq_entry)


def matching_entries(trades, shortsales):
    """Matches the trades to shortsales with the same volume (size) or shares
    and returns a tuple containing the trade and matching shortsale. If there
    is no matching shortsale for the corresponding trade an empty dictionary
    is returned for the shortsale.
    """
    unmatched_trades = list(trades)
    unmatched_shortsales = list(shortsales)

    # Yield all matching trades and shortsale entries
    for i in range(len(trades)):
        for j in range(len(shortsales)):
            if float_to_int(trades[i]['shares']) == float_to_int(shortsales[j]['ShortSize']):
                unmatched_trades.remove(trades[i])
                unmatched_shortsales.remove(shortsales[j])
                yield (trades[i], shortsales.pop(j))
                break

    # Log any unmatched shortsales as errors
    for shortsale in unmatched_shortsales:
        logging.warning(shortsale_file + " : No matching trade in: " + taq_file
            + " for following shortsale: " + shortsale['Symbol'] + ", " + shortsale['Time']
            + ", " + shortsale['Price'] + ", " + shortsale['ShortSize'])

    # Return any unmatched trades with empty shortsales
    for trade in unmatched_trades:
        yield (trade, {})


# The trades file, quotes directory, and resultant taq file
taq_file = ''
shortsale_file = ''
output_file = ''
log_dir = 'errors/flag_shortsale'
log_file = ''


# Process command line arguments
if len(sys.argv) < 4:
    sys.stderr.write("Invalid arguments given\n")
    sys.stderr.write("Usage: flag_shortsale.py {aligned taq file} {shortsale file} {output file}\n")
    sys.stderr.write("Example: flag_shortsale.py taq_08oct2012.csv NSDQsh_08oct2008.txt taq_reg_sho/taq_08oct2012.csv\n")
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

    taq_file, shortsale_file, output_file = sys.argv[1:4]
    log_file = os.path.join(os.path.normpath(log_dir), 'errors_' + os.path.basename(taq_file).replace('.csv', '.log'))
else:
    sys.stderr.write("Aligned TAQ or shortsale file does not exist, check arguments and try again!\n")
    sys.exit(1)

# Configure the logging
logging.basicConfig(format='', filename=log_file, level=logging.DEBUG)


# A buffer of the final data to be written to file, which is periodically flushed to disk
buffer = list()

# A list of trades and shortsales that occur at the same time, which are then matched
trades = list()
shortsales = list()

# For each entry in the aligned taq file iterate through the shortsale file and flag shortsale trades
with open(taq_file, 'rb') as taq_csv:
    taq_reader = csv.DictReader(taq_csv)

    # Open the shortsale file and read the first entry
    shortsale_csv = open(shortsale_file, 'rb')
    shortsale_reader = csv.DictReader(shortsale_csv, delimiter='|')
    shortsale = shortsale_reader.next()

    # Initialize the previous time to 1 second prior to the opening time
    prev_trade_time = datetime.time(9, 29, 59)

    # Flag each matched trade in the aligned taq file that is a shortsale
    for trade in taq_reader:
        symbol = trade['symbol']
        trade_time = convert_mil(trade['time'])

        # Ignore the trade if it occurs when the markets are closed
        if not market_hours(trade_time):
            continue

        # Iterate through the shortsale file to the matching symbol entry
        while symbol > shortsale['Symbol']:
            try:
                shortsale = shortsale_reader.next()
            except StopIteration:
                logging.warning(shortsale_file + " : Ended before :" + taq_file
                    + " at following trade: " + symbol + ", " + trade_time.isoformat())
                break

        # If no matching entry in the shortsale file then mark the trade as not a shortsale
        if symbol < shortsale['Symbol']:
            add_entry(buffer, trade, {})
            continue

        # If the current symbol or trade time differs, match and add any outstanding trades
        if (symbol != shortsale['Symbol'] or prev_trade_time < trade_time) and trades:
            for (trade_entry, shortsale_entry) in matching_entries(trades, shortsales):
                add_entry(buffer, trade_entry, shortsale_entry)
            # Clear the lists
            del trades[:]
            del shortsales[:]

        prev_trade_time = trade_time

        # Flag any trades in the taq file as shortsales that are matched in the shortsales file
        while (True):
            try:
                shortsale_time = convert_str(shortsale['Time'])

                # Ignore any shortsales that occur when the markets are closed
                if not market_hours(shortsale_time):
                    shortsale = shortsale_reader.next()
                    continue

                # Flag any trades that have an exact match in the shortsale file
                if symbol == shortsale['Symbol'] and trade_time == shortsale_time:
                    trades.append(trade)
                    shortsales.append(shortsale)
                    shortsale = shortsale_reader.next()
                    break
                # Break symbol has changed or the shortsale time is greater, trade is NOT a shortsale
                elif symbol != shortsale['Symbol'] or trade_time < shortsale_time:
                    trades.append(trade)
                    break

                # Read the next line of the shortsale file
                shortsale = shortsale_reader.next()

            # Should not have reached the end of shortsale file before TAQ file
            except StopIteration:
                logging.warning(shortsale_file + " : Ended before :" + taq_file
                    + " at following trade: " + symbol + ", " + trade_time.isoformat())
                break

        # If the buffer is > 250K lines, flush the buffer to disk
        if buffer.__len__() >= 250000:
            write_buffer(buffer, output_file)
            # Clear the buffer in memory
            del buffer[:]

    # Close the open files
    shortsale_csv.close()


# Add any outstanding trades to the buffer
if trades:
    for (trade_entry, shortsale_entry) in matching_entries(trades, shortsales):
        add_entry(buffer, trade_entry, shortsale_entry)

# Write any remaining content in the buffer to disk
if buffer:
    write_buffer(buffer, output_file)
