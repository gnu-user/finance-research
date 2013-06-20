#!/usr/bin/env pypy
###############################################################################
#
# Processes the aligned taq files and create a time-weighted output for each
# file containing the NBBO and NQBBO.
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


def add_entry(buffer, trade, trade_time, RQHS):
    """Adds an entry from the aligned taq file to the buffer
    """
    taq_entry = OrderedDict({})

    # Add the entries from the trade file in order
    taq_entry['time']    = trade_time.isoformat()
    taq_entry['symbol']  = trade['symbol']
    taq_entry['NBB'] = trade['NBB']
    taq_entry['NBO'] = trade['NBO']
    #taq_entry['NBBSIZ'] = trade['NBBSIZ']
    #taq_entry['NBOSIZ'] = trade['NBOSIZ']
    taq_entry['NQBB'] = trade['NQBB']
    taq_entry['NQBO'] = trade['NQBO']
    #taq_entry['NQBBSIZ'] = trade['NQBBSIZ']
    #taq_entry['NQBOSIZ'] = trade['NQBOSIZ']

    # Add the RQS calculation for both National and NSDQ
    if 'NRQHS' in RQHS and 'NQRQHS' in RQHS:
        taq_entry['NRQHS'] = RQHS['NRQHS']
        taq_entry['NQRQHS'] = RQHS['NQRQHS']
    else:
        logging.warning(trade['time'] + ',' + trade['symbol'] + ' Adding: ' 
            + trade_time.isoformat() + ' No RQHS data found!')

    # Append the TAQ entry to the TAQ output buffer
    buffer.append(taq_entry)


def calc_RQHS(trade):
    """Calculates the RQHS for both the National BBO and NASDAQ BBO and returns
    a dictionary containing the results for each. The RQHS is calculated using the
    following:
                RQS = (At - Bt) / Qt    , Qt = (At + Bt) / 2
                RQHS = RQS / 2
    """
    RQHS = {}

    try:
        # Calculate RQHS for National BBO
        A = float(trade['NBO'])
        B = float(trade['NBB'])
        RQHS['NRQHS'] = round(((A - B) / ((A + B) / 2.0)) / 2.0, 6)
    except ZeroDivisionError:
        RQHS['NRQHS'] = 0.0

    # Calculate RQHS for NASDAQ BBO
    try:
        A = float(trade['NQBO'])
        B = float(trade['NQBB'])
        RQHS['NQRQHS'] = round(((A - B) / ((A + B) / 2.0)) / 2.0, 6)
    except ZeroDivisionError:
        RQHS['NQRQHS'] = 0.0

    return RQHS


def add_midpoint(buffer):
    """Calculate the midpoint (Q) and midpoint in 5 minutes (Q_5min) for each trade in the buffer
    for both the National BBO and NASDAQ BBO and store the results in the original buffer. The 
    Midpoint is calculated using the following:

                Qt = (At + Bt) / 2
    """ 
    for i in xrange(0, len(buffer)):
        # Set the 5 minute offset to the last value if within < 5 closing
        if i >= (len(buffer) - 301):
            offset = (len(buffer) - 1) - i
        else:
            offset = 300

        # Calculate midpoint now for National BBO
        A = float(buffer[i]['NBO'])
        B = float(buffer[i]['NBB'])
        Q = (A + B) / 2.0

        # Calculate the midpoint in 5 minutes for National BBO
        A = float(buffer[i + offset]['NBO'])
        B = float(buffer[i + offset]['NBB'])
        Q_5min = (A + B) / 2.0

        # Set the national midpoint in the buffer
        buffer[i]['NMID'] = round(Q, 6)
        buffer[i]['NMID5'] = round(Q_5min, 6)

        # Calculate midpoint now for NASDAQ BBO
        A = float(buffer[i]['NQBO'])
        B = float(buffer[i]['NQBB'])
        Q = (A + B) / 2.0

        # Calculate the midpoint in 5 minutes for NASDAQ BBO
        A = float(buffer[i + offset]['NQBO'])
        B = float(buffer[i + offset]['NQBB'])
        Q_5min = (A + B) / 2.0

        # Set the NASDAQ midpoint in the buffer
        buffer[i]['NQMID'] = round(Q, 6)
        buffer[i]['NQMID5'] = round(Q_5min, 6)


def add_RPI5(buffer):
    """Calculates the relative price impact in 5 minutes (RPI5) for each trade
    in the buffer for both the National BBO and NASDAQ BBO and stores the
    results in the original buffer. The RPI5 is calculated using the
    following:
                (Q_5min - Q) / Q

    For entries that occur within < 5 minutes of closing, the last trade of the
    day is used.
    """
    for i in xrange(0, len(buffer)):
        # Set the 5 minute offset to the last value if within < 5 closing
        if i >= (len(buffer) - 301):
            offset = (len(buffer) - 1) - i
        else:
            offset = 300

        # Calculate the midpoint in 5 minutes for National BBO
        A = float(buffer[i + offset]['NBO'])
        B = float(buffer[i + offset]['NBB'])
        Q_5min = (A + B) / 2.0

        # Calculate midpoint now for National BBO
        A = float(buffer[i]['NBO'])
        B = float(buffer[i]['NBB'])
        Q = (A + B) / 2.0

        # Calculate the National RPI5
        try:
            buffer[i]['NRPI5'] = round((Q_5min - Q) / Q, 6)
        except ZeroDivisionError:
            buffer[i]['NRPI5'] = 0.0

        # Calculate the midpoint in 5 minutes for NASDAQ BBO
        A = float(buffer[i + offset]['NQBO'])
        B = float(buffer[i + offset]['NQBB'])
        Q_5min = (A + B) / 2.0

        # Calculate midpoint now for NASDAQ BBO
        A = float(buffer[i]['NQBO'])
        B = float(buffer[i]['NQBB'])
        Q = (A + B) / 2.0

        # Calculate the National RPI5
        try:
            buffer[i]['NQRPI5'] = round((Q_5min - Q) / Q, 6)
        except ZeroDivisionError:
            buffer[i]['NQRPI5'] = 0.0


# The trades file, quotes directory, and resultant taq file
taq_file = ''
output_file = ''
log_dir = 'errors/time_weighted'
log_file = ''


# Process command line arguments
if len(sys.argv) < 3:
    sys.stderr.write("Invalid arguments given\n")
    sys.stderr.write("Usage: time_weighted.py {aligned taq file} {output file}\n")
    sys.stderr.write("Example: time_weighted.py taq_08oct2012.csv taq_time_08oct2012.csv\n")
    sys.exit(1)

if os.path.isfile(sys.argv[1]):
    # Error if output file is directory
    if os.path.isdir(sys.argv[2]):
        sys.stderr.write("The output file cannot be a directory, please specify an output file!\n")
        sys.exit(1)

    # Error if trying to overwrite an existing output file
    if os.path.isfile(sys.argv[2]):
        sys.stderr.write("The output file already exists, please specify a different output file!\n")
        sys.exit(1)

    taq_file, output_file = sys.argv[1:3]

    if not os.path.exists(log_dir):
        os.makedirs(log_dir)

    log_file = os.path.join(os.path.normpath(log_dir), 'errors_' + os.path.basename(taq_file).replace('.csv', '.log'))
else:
    sys.stderr.write("Aligned TAQ or shortsale file does not exist, check arguments and try again!\n")
    sys.exit(1)

# Configure the logging
logging.basicConfig(format='', filename=log_file, level=logging.DEBUG)


# The market start and end time boundaries
start = datetime.time(9, 30, 00)
end = datetime.time(16, 00, 00)

# A buffer of the final data to be written to file, which is periodically flushed to disk
buffer = list()


# For each entry in the aligned taq file iterate through the shortsale file and flag shortsale trades
with open(taq_file, 'rb') as taq_csv:
    taq_reader = csv.DictReader(taq_csv)

    # Read the first entry
    trade = taq_reader.next()
    cur_trade = trade
    cur_trade_time = convert_mil(trade['time'])

    # First entry for the current symbol
    first_entry = True

    # Go through each trade in the taq file and create a time-weighted entry
    for trade in taq_reader:
        # If the previous symbol and trade time is the same get the last entry
        while cur_trade['symbol'] == trade['symbol'] and cur_trade_time == convert_mil(trade['time']):
            try:
                # Save the current trade information
                cur_trade = trade
                cur_trade_time = convert_mil(trade['time'])

                # Read the next trade
                trade = taq_reader.next()
            except StopIteration:
                logging.warning(taq_file + " : Ended at following trade: " 
                    + trade['symbol'] + ", " + convert_mil(trade['time']).isoformat())
                break

        # Ignore the trade if it occurs when the markets are closed
        if not market_hours(cur_trade_time):
            # Save the current trade information
            cur_trade = trade
            cur_trade_time = convert_mil(trade['time'])
            continue

        # Calculate the RQHS for the current trade
        RQHS = calc_RQHS(cur_trade)

        # If the entry for the symbol is the first for the start of the day, repeat the
        # entry from the market start time up to the current trade time
        if first_entry:
            for seconds in xrange(0, time_delta(start, cur_trade_time)):
                trade_time = add_seconds(start, seconds)
                add_entry(buffer, cur_trade, trade_time, RQHS)
            first_entry = False

        # If the next symbol is different, repeat the entry up to and including the end of the market period
        if cur_trade['symbol'] != trade['symbol']:
            for seconds in xrange(0, time_delta(cur_trade_time, end) + 1):
                trade_time = add_seconds(cur_trade_time, seconds)
                add_entry(buffer, cur_trade, trade_time, RQHS)
            # Next symbol will be the first entry
            first_entry = True

        # Repeat the current trade entry up to the next trade time
        if cur_trade['symbol'] == trade['symbol']:
            for seconds in xrange(0, time_delta(cur_trade_time, convert_mil(trade['time']))):
                trade_time = add_seconds(cur_trade_time, seconds)
                add_entry(buffer, cur_trade, trade_time, RQHS)

        # Save the next trade as the current trade
        cur_trade = trade
        cur_trade_time = convert_mil(trade['time'])

        # Calculate midpoints and flush buffer to disk when it contains all time-weighted trades for one day
        if buffer.__len__() >= 23401:
            # Calculate the midpoints and add it to the buffer
            add_midpoint(buffer)
            write_buffer(buffer, output_file)
            # Clear the buffer in memory
            del buffer[:]

    # For the very last trade, repeat the entry up to and including the end of the market period
    if cur_trade_time <= end:
        RQHS = calc_RQHS(cur_trade)
        for seconds in xrange(0, time_delta(cur_trade_time, end) + 1):
            trade_time = add_seconds(cur_trade_time, seconds)
            add_entry(buffer, cur_trade, trade_time, RQHS)


# Write any remaining content in the buffer to disk
if buffer:
    add_midpoint(buffer)
    write_buffer(buffer, output_file)
