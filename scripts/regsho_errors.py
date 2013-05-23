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
output_dir = ''
output_file  = ''

# Process command line arguments
if len(sys.argv) < 3:
    sys.stderr.write("Invalid arguments given\n")
    sys.stderr.write("Usage: regsho_errors.py {regsho file} {error log directory}\n")
    sys.stderr.write("Example: regsho_errors.py regsho_20080801.txt errors/regsho/\n")
    sys.exit(1)

if os.path.isfile(sys.argv[1]) and os.path.isdir(sys.argv[2]):
    regsho_file, output_dir = sys.argv[1:3]
else:
    sys.stderr.write("File or directory does not exist, check arguments and try again!\n")
    sys.exit(1)

# Configure the logging
regsho_filename = os.path.basename(regsho_file).replace('.txt', '') + '_errors.log'
output_file = os.path.join(os.path.normpath(output_dir), regsho_filename)
logging.basicConfig(format='', filename=output_file, level=logging.DEBUG)


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
