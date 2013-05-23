#!/bin/bash
###############################################################################
#
# A helpful script that is a runner for executing a series of sharcnet jobs for
# the regwho_errors.py script
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

FILES_DIR=""
OUTPUT_DIR=""
LOG_DIR="log/regsho_errors/"

all_subdir()
{
  # save current directory then cd to "$1"
  pushd "$1" >/dev/null
  # for each non-hidden (i.e. not starting with .) file/directory...
  for file in * ; do
    # print file/direcotry name if it really exists...
    if [[ -f "$file" ]]
    then
        DIR="$(pwd | tr -d '\n')"
        echo "${DIR}/${file}"
    fi
    # if directory, go down and list directory contents too
    if [[ -d "$file" ]]
    then
        all_subdir "$file" "$2  "
    fi
  done
  # restore directory
  popd >/dev/null
}

# Make sure the user provided the path
if [[ -z "$1" ]]
then
    echo "No path provided" 1>&2
    echo "Usage: $0 {reg_sho directory} {output directory}" 1>&2
    echo "Example: $0 reg_sho_2012/ errors/reg_sho/"
    exit 1
fi

# Make sure the directories exist
if [[ -d "$1" ]] && [[ -d "$2" ]]
then
    FILES_DIR="$1"
    OUTPUT_DIR="$2"
else
    echo "Directory(s) provided do not exist!" 1>&2
    exit 1
fi

# Make a log directory for the sharcnet job execution logs
if [[ ! -d "${LOG_DIR}" ]]
then
    mkdir -p $LOG_DIR
fi


# Execute a job for each file
for file in $(all_subdir ${FILES_DIR})
do
    # Get the trade and quote filename, set the output file
    file_name=$(basename $file)
    file_name=${file_name%.*}
    output_file=errors_${file_name}.log

    # Process the trades file with align_quotes.py as a job on SHARCNET
    sqsub -r 4h -q serial --memperproc=4G -o ${LOG_DIR}/${file_name}.log regsho_errors.py ${file} ${OUTPUT_DIR}
done

