#!/bin/bash
###############################################################################
#
# A helpful script that is a runner for executing a series of sharcnet jobs for
# each trades file. For each trades file in the trades directory specified the
# script executes a new sharcnet job to execute the align_quotes.py script for
# that trades file.
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

TRADES_DIR=""
QUOTES_DIR=""
OUTPUT_DIR=""
LOG_DIR="log/align_quotes/"

# Make sure the user provided the path
if [[ -z "$1" ]]
then
    echo "No path provided" 1>&2
    echo "Usage: $0 {trades directory} {quotes directory} {output directory}" 1>&2
    echo "Example: $0 trades_2012/ quotes_2012/ taq_2012/"
    exit 1
fi

# Make sure the directories exist
if [[ -d "$1" ]] && [[ -d "$2" ]] && [[ -d "$3" ]]
then
    TRADES_DIR="$1"
    QUOTES_DIR="$2"
    OUTPUT_DIR="$3"
else
    echo "Directory(s) provided do not exist!" 1>&2
    exit 1
fi

# Make sure the output directory has no files in it
if [[ "$(ls -A $OUTPUT_DIR)" ]]
then
     echo "Output directory contains files, choose a different directory!" 1>&2
     exit 1
fi

# Make a log directory for the sharcnet job execution logs
if [[ ! -d "${LOG_DIR}" ]]
then
    mkdir -p $LOG_DIR
fi


# Execute a job for each trade and matching quotes file
for file in ${TRADES_DIR}/*.csv
do
    # Get the trade and quote filename, set the output file
    trade_file=$(basename $file)
    trade_file=${trade_file%.*}
    quote_file=quotes_${trade_file/trades_/}.csv
    output_file=taq_${trade_file/trades_/}.csv

    # Process the trades file with align_quotes.py as a job on SHARCNET
    sqsub -r 7d -q serial --memperproc=4G -o ${LOG_DIR}/${trade_file}.log align_quotes.py ${file} ${QUOTES_DIR}/${quote_file} ${OUTPUT_DIR}/${output_file}
done

