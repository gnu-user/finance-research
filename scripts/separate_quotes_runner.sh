#!/bin/bash
###############################################################################
#
# A helpful script that is a runner for executing a series of sharcnet jobs for
# separating the large quotes files into a smaller quotes file for each day.
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
LOG_DIR="log/separate_quotes/"

# Make sure the user provided the path
if [[ -z "$1" ]]
then
    echo "No path provided" 1>&2
    echo "Usage: $0 {directory with trades files} {directory with quotes files} {output directory for results}" 1>&2
    echo "Example: $0 trades_2012/ quotes_2012/ separate_quotes_2012/"
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

# Make a log directory for the sharnet job execution logs
if [[ ! -d "${LOG_DIR}" ]]
then
    mkdir -p $LOG_DIR
fi


# Execute a job for each trades file
for file in ${QUOTES_DIR}/*.csv
do
    # Get the filename, set the output file
    file_name=$(basename $file)
    file_name=${file_name%.*} 

    # Process the trades file with align_quotes.py as a job on SHARCNET
    sqsub -r 7d -q serial --memperproc=8G -o ${LOG_DIR}/${file_name}.log separate_quotes.py ${TRADES_DIR} ${file} ${OUTPUT_DIR}
done
