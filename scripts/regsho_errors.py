#!/usr/bin/env pypy
###############################################################################
#
# Identifies if there are any errors in the RegSho data before any further
# processing of the files.
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

regsho_file = ''
regsho_filename = ''

# Process command line arguments
if len(sys.argv) < 2:
    sys.stderr.write("Invalid arguments given\n")
    sys.stderr.write("Usage: regsho_errors.py {regsho file}\n")
    sys.stderr.write("Example: regsho_errors.py regsho_20080801.txt\n")
    sys.exit(1)

if os.path.isfile(sys.argv[1]):
    regsho_file = sys.argv[1]
else:
    sys.stderr.write("File does not exist, check arguments and try again!\n")
    sys.exit(1)

# Configure the logging
regsho_filename = os.path.basename(regsho_file).replace('.txt', '')
logging.basicConfig(format='', filename=regsho_filename + '_errors.log', level=logging.DEBUG)


# Process each entry in the regsho file and determine if there are any errors in the data
with open(regsho_file, 'rb') as csvfile:
    reader = csv.DictReader(csvfile, delimiter='|')

    for entry in reader:
        # If the size and shortsize or shorttype not 'S', report error
        if entry['Size'] != entry['ShortSize']:
            logging.warning(regsho_file + " : " + entry['Symbol'] + ", " + entry['Date'] +
                ", " + entry['Time'] + " : Size = " + entry['Size'] + ", ShortSize = " +
                entry['ShortSize'])
        if entry['ShortType'] != 'S':
            logging.warning(regsho_file + " : " + entry['Symbol'] + ", " + entry['Date'] +
                ", " + entry['Time'] + " : ShortType = " + entry['ShortType'])
