#!/bin/bash
###############################################################################
#
# A helpful script that is a runner for executing a series of sharcnet jobs for
# the time_weighted.py script, which creates time-weighed output files for TAQ
# data.
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

TAQ_DIR=""
OUTPUT_DIR=""
LOG_DIR="log/time_weighted/"

# Make sure the user provided the path
if [[ -z "$1" ]]
then
    echo "Usage: $0 {aligned taq directory} {output directory}" 1>&2
    echo "Example: $0 aligned_taq_2012/ aligned_taq_time_2012/" 1>&2
    exit 1
fi

# Make sure the directories exist
if [[ -d "$1" ]] && [[ -d "$2" ]]
then
    TAQ_DIR="$1"
    OUTPUT_DIR="$2"
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
for file in ${TAQ_DIR}/*.csv
do
    # Get the taq filename, set the output file
    taq_file=$(basename $file)
    taq_file=${taq_file%.*}
    output_file=taq_time_${taq_file/taq_/}.csv

    # Process the taq file with time_weighted.py as a job on SHARCNET
    sqsub -r 3h -q serial --mpp=2G -o ${LOG_DIR}/${taq_file}.log time_weighted.py ${file} ${OUTPUT_DIR}/${output_file}
done

