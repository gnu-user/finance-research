#!/usr/bin/perl -w
###############################################################################
#
# A quick script to consistently rename files to a common format, this makes
# it much easier to process the files and have script to automatically queue
# jobs on SHARCNET. 
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
use File::Find;
use strict;
use warnings;
use Cwd;

# Mapping of months to days
my %months = ("07" => "jul",
              "08" => "aug",
              "09" => "sep",
              "10" => "oct",
              "11" => "nov",
              "12" => "dec");

my $dir = $ARGV[0];

# Rename the files to a consistent naming scheme used everywhere else
sub rename_files
{
    my $file = $_;
    my $newfile = "";
    my $cwd = cwd() . "/";

    # Rename the file to have a consistent naming scheme
    if ($file =~ /(^.*?)(\d{4})(\d{2})(\d{2}).*?(\.[^~]+$)/)
    {
        $newfile = $1 . "_" . $4 . $months{$3} . $2 . $5;
        system("mv -vf " . $cwd . $file . " " . $cwd . $newfile )
    }
}


find(\&rename_files, $dir);
exit 0;
