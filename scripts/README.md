README
========================================

Source File Overview
----------------------------------------

The following is a summary of the scripts used to process the data and produce
the combined output containing the NBBx calculations. There are several other
scripts as well but many of them were used to perform sanity checks on the data
(e.g. regsho_errors.py).

For each script there is also a corresponding job runner for launching all of 
the jobs on SHARCNET (e.g. align_quotes_runner.sh), to run the script with the
dataset you will want to run the job runner, which will have the same prefix as
the script to execute with "_runner.sh" appended.



Align Quotes (align_quotes.py)
----------------------------------------

Used to align the trade and quotes file and calculate the National and NASDAQ 
BBO data and produces a resultant CSV file containing the combined data with the
NASDAQ and National BBx. The align quotes script takes as its arguments a trade
and quotes file and produces a corresponding output file with the combined trade
and quote data and BBx.

To execute the align quotes script for each trade and quotes file execute the
job runner, align_quotes_runner.sh and for the arguments pass the trades directory
quotes directory and the output directory for the combined results file.



Flag Short Sale (flag_shortsale.py)
----------------------------------------

This script is used to process the aligned TAQ files and flag entries that are 
short sales using the NASDAQ data identifying short sales that occurred. Entries 
in the aligned taq files that are identified as short sales are flagged with 1 
and the corresponding short sale size and short sale type are recorded.

To execute the flag shortsale script for each aligned TAQ file execute the
job runner, flag_shortsale_runner.sh and for the arguments pass the aligned
TAQ  directory, the directory containing the NASDAQ data identifying short sales,
and the output directory to store the results.



Time Weighted (time_weighted.py)
----------------------------------------

This script is used to process the aligned TAQ files and create a time-weighted
output as well as calculate the midpoints, RQHS, and RPI5.

To create a time weighted output for each aligned TAQ file, execute the job runner, 
time_weighted_runner.sh and for the arguments pass the aligned TAQ directory, and 
the output directory to store the results.



Combine TAQ Files (combine_taq.py)
----------------------------------------

In order for jobs to be executed on SHARCNET and complete their execution without
taking more than 7 days (or running out of memory!) the data is separated into 
individual files for each day. To combine the results files for each day into 
a single output file use the combin_taq.py script.

To execute the combine_taq.py script pass as the first argument the directory
containing the separate TAQ files and the name of the resultant file to be
created, (e.g. taq_aug01_nov30.csv).



Summary Statistics (ShortSaleAnalysis.R)
----------------------------------------

In order to calculate the summary statistics for the results, execute the
short sale analysis script which is used to process the entire resultant
dataset and calculate the means, medians, etc. for each symbol for each day. Edit
the script and configure the parameters to specify the taq and time directories
as well as the output directories. If you want to calculate the trimmed results
set TRIM_RESULTS to TRUE.




Data Processing Workflow
----------------------------------------

1.  Start by executing the align quotes job runner on SHARCNET for the trades
    and quotes directory containing the separate trades and quotes file for
    each day. Wait for the jobs to finish executing, performance on SHARCNET varies
    widely, there have been instances where some jobs have finished quickly
    whereas others have not finished within 7 days. After consulting with SHARCNET
    advisors, the reason given is due to the poor performance of disk operations 
    (reading/writing) large files on SHARCNET. Keep an eye on jobs, if there 
    are jobs that do not finish in time you will have to restart them manually,
    see the log/script name/ directory to view the results.


2.  After the aligned TAQ files have been created the next step is to then
    flag the short sale entries. Execute the flag shortsale job runner, this
    will create a job for each aligned TAQ file with the corresponding NASDAQ
    file identifying short sales on that date. Again keep an eye on the jobs!


3.  Now that you have the aligned TAQ files with short sales idenfitied the
    next step is to create the time weighted results and calculate the midpoints, 
    RQHS, and RPI5. Exceute the time weighted results job runner, this will 
    create a job for each aligned TAQ file and save the resultant file to
    the output directory specifed. Keep an eye on your jobs, as this job can
    tend to take the longest as there is a lot more data being written to disk
    in order to create the time weighted files.


4.  If you want a single combined file containing the results then execute the
    combine_taq.py script and provide it the directory to the separate files
    and the name of the resultant combined output file.


5.  Lastly, to generate summary statistics for the raw TAQ data edit the analysis
    script ShortSaleAnalysis.R and set the following variables accordingly and
    manually execute the R script as an individual job on SHARCNET. Due to the
    disk reading and writing this job often takes a while, make sure you give 
    the job the maximum amount of memory as it usually requires 12GB of RAM.

    TRIM_RESULTS
    taq_dir
    time_dir
    market_cap_file
    daily_results_file
    time_weight_results_file
    



FAQ
----------------------------------------

1.  Why are there so many separate scripts? Why not just have one?  

    The reason there are separate scripts for each process is because all of the
    jobs combined, or in some cases even two of the tasks combined into one script
    will not finish execution within the maximum time limit for a job on SHARCNET,
    which is 7 days. As well having separate scripts means that if a single job
    is not finished within time (due to the inconsistent performance on SHARCNET)
    you only have to restart that one job manually rather than redoing everything.


2.  Why are there separate files?  

    It is impossible for any task processing the entire dataset at once to complete
    the execution within 7 days, not only would it take months or years to process 
    the entire dataset as one job, there is also no system with enough RAM available
    (roughly 300GB+!).


3.  Where are the log files?  

    The log files are created within your current directory, a sub directory is
    create for log files related to each type of script/task you are executing,
    (e.g. log/aling_quotes/ is the log file for running align quotes scripts).


4.  Help, it's not working!  

    Make sure that you are executing the job runners instead of the script individually,
    make sure that you are giving the correct arguments and the full path to the
    directories or files (e.g. use /work/jgillett/trades/ instead of just trades/),
    if it is still not working take a look at the log/ directory for the corresponding
    script and see what the error message says.


5.  My jobs are not finishing in time!  

    This was an issue I ran into initially, while python is fast and works well,
    its performance can be improved. Try looking into using [PyPy](http://pypy.org/)
    it is an optimized version of python created by researchers trying to get
    extra performance, it markets itself as being 2-3x faster than the standard
    python.
