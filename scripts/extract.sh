#!/bin/bash
###############################################################################
#
# A helpful script for concurrently extracting many files at once, to use with
# SHARCNET you can use the following command:
# 
# sqsub -q threaded -r <time> -n <# cpu> --memperproc=<amount of mem> -o 
# output.log ./extract.sh <dir to extract> <# threads (same as #cpu)>
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
set -m

N_THREADS=8

# Make sure the user provided the path
if [[ -z "$1" ]]
then
    echo "No path provided" 1>&2
    echo "Usage: $0 {directory with zip files} {number of threads (optional)}" 1>&2
    echo "Example: $0 /work/user/TAQ_DATA/ 2"
    exit 1
fi

# Make sure the directory exists
if [[ -d "$1" ]]
then
    DIR="$1"
else
    echo "Directory provided does not exist!" 1>&2
fi

# Use number of threads if specified
if [[ "$2" ]]
then
    N_THREADS="$2"
fi


# Change to the directory and process the sas files
cd "${DIR}"

for file in *.zip
do
    # Do not execute more than 8 threads simultaneously
    while [ `jobs | wc -l` -ge ${N_THREADS} ]
    do
        sleep 10
    done
    
    # Fork the conversion process
    unzip $file &> ${file}.log &
done

# Wait for all parallel jobs to finish
while [ 1 ]
do
    fg &> /dev/null; [ $? == 1 ] && break; 
done
