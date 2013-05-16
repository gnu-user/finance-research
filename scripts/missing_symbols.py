#!/usr/bin/env pypy
###############################################################################
#
# Script to analyze the quotes files to ensure that there are no missing symbols
# for any day.
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


def get_symbols(file):
    """Analyzes a reference file containing the list of symbols and returns
    a set containing the symbols
    """
    symbols = set()

    # Open the trades file and get each date
    with open(file, 'rb') as csvfile:
        reader = csv.DictReader(csvfile)

        for line in reader:
            symbols.add(line['SYMBOL'])

    return symbols


def get_dates_symbols(file):
    """Analyzes each of the dates and symbols in the quotes file and returns an
    ordered dictionary containing each date in the file and all of the symbols
    found for that date.
    """
    dates_symbols = OrderedDict({})

    with open(file, 'rb') as csvfile:
        reader = csv.DictReader(csvfile)

        for line in reader:
            date = int(float(line['date']))
            if not date in dates_symbols:
                dates_symbols[date] = set()
            dates_symbols[date].add(line['SYMBOL'])

    return dates_symbols


# Symbols file containing a list of symbols and quotes file to check
symbols_file = ''
quotes_file = ''
errors_file = ''


# Process command line arguments
if len(sys.argv) < 3:
    sys.stderr.write("Invalid arguments given\n")
    sys.stderr.write("Usage: align_quotes.py {symbols file} {quotes file} {output file}\n")
    sys.stderr.write("Example: " + sys.argv[0] + "symbols.csv quotes_08oct2012.csv errors_08oct2012.log\n")
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
    symbols_file, quotes_file, errors_file = sys.argv[1:4]
else:
    sys.stderr.write("Symbols or quotes file does not exist, check arguments and try again!\n")
    sys.exit(1)

# Configure the logging
logging.basicConfig(filename=errors_file)


# Get the list of symbols and search for the symbols
symbols = get_symbols(symbols_file)
dates_symbols = get_dates_symbols(quotes_file)

# For each date in the quotes file report any missing symbols
for date in dates_symbols:
    for symbol in sorted(symbols.difference(dates_symbols[date])):
        logging.warning("For date: " + str(date) + ", missing symbol: " + symbol)
