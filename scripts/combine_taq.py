#!/usr/bin/env pypy
###############################################################################
#
# Combines the resultant taq files from align_quotes.py into a single output file
#
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


def process_taq(dir):
    """Analyzes each of the dates in the trades and quotes csv files and returns an ordered
    list of the dates found in the file and the corresponding name for the resultant file.
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
            dates_files[date] = file

    # Sort the files by date add them in an ordered dictionary
    for date in sorted(dates_files):
        ordered_dates[date] = dates_files[date]

    return ordered_dates

taq_dir = ''
output_file = ''

# Process command line arguments
if len(sys.argv) < 3:
    sys.stderr.write("Invalid arguments given\n")
    sys.stderr.write("Usage: align_quotes.py {aligned taq directory} {output file}\n")
    sys.stderr.write("Example: " + sys.argv[0] + " aligned_taq_2008/ aligned_taq_2008/taq_aug01_nov30.csv\n")
    sys.exit(1)

if os.path.isdir(sys.argv[1]):
    # Error if output file is directory
    if os.path.isdir(sys.argv[2]):
        sys.stderr.write("The output file cannot be a directory, please specify an output file!\n")
        sys.exit(1)
    # Error if trying to overwrite an existing output file
    if os.path.isfile(sys.argv[2]):
        sys.stderr.write("The output file already exists, please specify a different output file!\n")
        sys.exit(1)
    taq_dir, output_file = sys.argv[1:3]
else:
    sys.stderr.write("TAQ directory provided does not exist!\n")
    sys.exit(1)


# Process the TAQ files, return an ordered list of files to combine
taq_dates = process_taq(taq_dir)

# Write the headers from the first file to the output file
with open(taq_dates[taq_dates.keys()[0]], 'rb') as inputcsvfile:
    reader = csv.DictReader(inputcsvfile)

    with open(output_file, 'wb') as outputcsvfile:
        writer = csv.DictWriter(outputcsvfile, delimiter=',', lineterminator='\n', fieldnames=reader.fieldnames)
        writer.writeheader()


# Use the shell awk and cat commands to concatenate each of the files quickly
for date in taq_dates:
    os.system("awk FNR-1 %s >> %s" % (taq_dates[date], output_file))
