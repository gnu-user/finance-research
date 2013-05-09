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
from collections import OrderedDict

def order_files(dir):
    """Analyzes each of the dates in the trades/quotes csv files and returns an ordered
    list of the files to execute from earliest to latest date and the corresponding dates 
    that are covered in each file.
    """
    files = {}
    ordered_files = OrderedDict({})

    for file in glob.glob(dir + "*.csv"):
        with open(file, 'rb') as csvfile:
            files[file] = []
            reader = csv.DictReader(csvfile)

            print file
            for line in reader:
                date = int(float(line['date']))
                if not date in files[file]:
                    print file, line['date']
                    files[file].append(date)

    # Sort the files store them in an ordered dictionary
    for file in sorted(files, key=files.get):
        ordered_files[file] = files[file]

    print "RESULTS"
    for file in ordered_files:
        print file, ordered_files[file]


# TODO add helper for converting the time formats in the CSV files into datetimes


# TODO add helper for processing the files and opening them in the correct order
# such that they can easily be iterated through, should return a data structure
# mapping the file to the date's it covers


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


# Parse each trade file and determine the order in which to process the trade
order_files(quotes_dir)