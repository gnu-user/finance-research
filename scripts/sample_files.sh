#!/bin/bash
###############################################################################
#
# A helpful script for getting a sample of very large csv files for the purpose
# of doing development. Simply run the script and provide the directory 
# containing the large CSV files and the number of lines from the head of each
# file you want, the resultant files will all be named _sample.csv
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
DIR=""
NUM_LINES=25000

# Make sure the user provided the path
if [[ -z "$1" ]]
then
    echo "No path provided" 1>&2
    echo "Usage: $0 {directory with csv files} {# line to sample (optional)}" 1>&2
    echo "Example: $0 /work/user/TAQ_DATA/"
    exit 1
fi

# Make sure the directory exists
if [[ -d "$1" ]]
then
    DIR="$1"
else
    echo "Directory provided does not exist!" 1>&2
fi

# Set the number of lines to sample if specified
if [[ "$2" ]]
then
    NUM_LINES="$2"
fi

# Change to dir, process csv files
cd "${DIR}"

for file in *.csv
do
    file_name=${file%.*}   # Filename without extension

    # Get only the first 25,000 lines of the file
    head -n${NUM_LINES} $file &> ${file_name}_sample.csv
done
