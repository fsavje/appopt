*** 1. Initialization

Change "SCRATCHDIR" in "envir.sh" to the path (absolute or relative) to a folder
that can handle high disk IO.


*** 2. Generate batches

Run:

> Rscript genBatches.R

This creates folders "b4G", "b25G", "running", "done" in SCRATCHDIR. "b4G" and "b25G"
will be populated with all simulation batches (5000 for each specification).


*** 2. Block all batches

The batches are divided into those with high ("b25G") and low ("b4G") memory use.

For the low memory batches, run:

> ./runbatches.sh $ID $CORES b4G "Rscript batch.R"

where $ID is an arbitrary identifier number and $CORES instructs how many cores should
be used. Each core will need approximately 4 GB of RAM, so ensure that the machine this is
run on has at least 4 * $CORES of RAM.

For the high memory batches, run:

> ./runbatches.sh $ID $CORES b25G "Rscript batch.R"

Normally these batches need approximately 15 GB of RAM per core, but
20-25 GB is preferable if possible.

Let these commands run until "b4G", "b25G" and "running" are empty and all
batches (50 000) are in "done".


*** 3. Collect batches

The batches are now in individual RData files, run:

> Rscript collect.R

to collect them into one RData file for each specification. Folders
"b4G", "b25G", "running", "done" (and their content) can be deleted 
after this step is done.


*** 4. Prepare batches for deriving statistics of interest

Currently the batches only contain covariates and blockings.
To get the statistics of interest, we first prepare the batches
by extracting the methods of interest and generate outcome data.
Do so by running:

> Rscript prepest.R
> Rscript prepstats.R


*** 5. Get results

Running:

> ./runbatches.sh $ID $CORES estsims "Rscript simest.R"
> ./runbatches.sh $ID $CORES statsims "Rscript simstats.R"

will derive the results. These are stored in folders "estresults"
and "statresults" in the current folder (not on SCRATCHDIR).
These folders are already populated in this version.


*** Generate tables

Run:

> Rscript esttable.R
> Rscript stattable.R

to generate tables. Both ASCII formatted and LaTeX tables are created.
ASCII is printed to std-in while LaTeX is saved as file in the folder
"latex". These table are already produced in this version. 

