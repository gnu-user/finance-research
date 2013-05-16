#!/bin/bash
###############################################################################
#
# A helpful script that is a runner for executing a series of scripts to
# determine if there are any missing symbols in the quotes file
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

SYMBOLS_FILE=""
QUOTES_DIR=""
OUTPUT_DIR=""
LOG_DIR="log/missing_symbols/"

# Make sure the user provided the path
if [[ -z "$1" ]]
then
    echo "Usage: $0 {symbols file} {quotes directory} {output directory}" 1>&2
    echo "Example: $0 symbols.csv quotes_2012/ missing_symbols/"
    exit 1
fi

# Make sure the directories exist
if [[ -e "$1" ]] && [[ -d "$2" ]] && [[ -d "$3" ]]
then
    SYMBOLS_FILE="$1"
    QUOTES_DIR="$2"
    OUTPUT_DIR="$3"
else
    echo "File or directory(s) provided do not exist!" 1>&2
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
for file in ${QUOTES_DIR}/*.csv
do
    # Get the quote filename, set the output file
    quote_file=$(basename $file)
    quote_file=${quote_file%.*}
    output_file=errors_${quote_file/quotes_/}.log

    # Process the trades file with align_quotes.py as a job on SHARCNET
    sqsub -r 3d -q serial --memperproc=4G -o ${LOG_DIR}/${quote_file}.log missing_symbols.py ${SYMBOLS_FILE} ${file} ${OUTPUT_DIR}/${output_file}
done

