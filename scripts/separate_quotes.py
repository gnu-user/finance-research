#!/usr/bin/env pypy
###############################################################################
#
# Processes the trades files and quotes files and seperates the larger quotes
# files into individual quotes files for each trades file date.
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


def process_trades(dir):
    """Analyzes each of the dates in the trades csv files and returns an ordered
    list of the dates found in the trades file and the corresponding name for the
    resultant separate quotes output file that should be created.
    """
    dates_files = {}
    ordered_dates = OrderedDict({})

    for file in glob.glob(os.path.join(os.path.normpath(dir), '*.csv')):
        with open(file, 'rb') as csvfile:
            reader = csv.DictReader(csvfile)
            line = reader.next()

            # Get the trades filename and make the output file have the
            # same name, but with quotes_<date>.csv instead of trades_<date>.csv
            date = int(float(line['date']))
            file_name = os.path.basename(file)
            dates_files[date] = file_name.replace('trades', 'quotes')

    # Sort the files by date add them in an ordered dictionary
    for date in sorted(dates_files):
        ordered_dates[date] = dates_files[date]

    return ordered_dates


def write_quotes(quotes_output, dir, file):
    """Writes the data in the the quotes output buffer to file, appending the
    quotes buffer data to any existing entries.
    """
    output_file = os.path.join(os.path.normpath(dir), file)

    # Write the headers of the file if the file does not exist
    if not os.path.isfile(output_file):
        with open(output_file, 'wb') as csvfile:
            writer = csv.DictWriter(csvfile, delimiter=',', fieldnames=quotes_output[0].keys())
            writer.writeheader()

    with open(output_file, 'ab') as csvfile:
        writer = csv.DictWriter(csvfile, delimiter=',', fieldnames=quotes_output[0].keys())
        writer.writerows(quotes_output)


trades_dir = ''
quotes_file = ''
output_dir = ''

# Process command line arguments
if len(sys.argv) < 4:
    sys.stderr.write("Invalid arguments given\n")
    sys.stderr.write("Usage: align_quotes.py {trades directory} {quotes file} {output directory}\n")
    sys.stderr.write("Example: " + sys.argv[0] + "trades_2012/ quotes_2012.csv separate_quotes_2012/\n")
    sys.exit(1)

if os.path.isdir(sys.argv[1]) and os.path.isfile(sys.argv[2]) and os.path.isdir(sys.argv[3]):
    trades_dir, quotes_file, output_dir = sys.argv[1:4]
else:
    sys.stderr.write("A file or directory(s) does not exist, check arguments and try again!\n")
    sys.exit(1)

# Configure the logging
logging.basicConfig(filename='seperate_quotes_errors.log', level=logging.DEBUG)


# Process the trades file, map the trade file date to the corresponding separate quotes output file
trade_dates = process_trades(trades_dir)

# A buffer containing the output file and data to write to disk
quotes_buffer = {}

# Initialize the quotes buffer to have each output file mapping to an empty list
for date in trade_dates:
    output_file = trade_dates[date]
    quotes_buffer[output_file] = list()


# Process each entry in the quotes file and write the entries for each date to the
# corresponding output file
with open(quotes_file, 'rb') as csvfile:
    reader = csv.DictReader(csvfile)

    for quote in reader:
        date = int(float(quote['date']))
        output_file = trade_dates[date]

        # Append the quotes entry to the corresponding output file buffer
        quotes_buffer[output_file].append(quote)

        # If the quotes buffer has >= 100K lines, flush the buffer to disk
        if quotes_buffer[output_file].__len__() >= 100000:
            write_quotes(quotes_buffer[output_file], output_dir, output_file)
            quotes_buffer[output_file] = list()


# Write any remaining content in the quotes buffer to disk
for output_file in quotes_buffer:
    if quotes_buffer[output_file].__len__() > 0:
        write_quotes(quotes_buffer[output_file], output_dir, output_file)
