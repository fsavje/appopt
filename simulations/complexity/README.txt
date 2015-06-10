*** 1. Initialization

Change "SCRATCHDIR" in "envir.sh" to the path (absolute or relative) to a folder
that can handle high disk IO.

Change "TIMEPROG" to the path to an executable of GNU time program, at least of
version 1.7. (see http://ftp.gnu.org/gnu/time/).

This is the latest version, but it contain a bug when reporting memory use. See, e.g.,
https://groups.google.com/forum/#!topic/gnu.utils.help/u1MOsHL4bhg
This can be patched by changing line 395:

FROM:
	fprintf (fp, "%lu", ptok ((UL) resp->ru.ru_maxrss));
TO:
	fprintf (fp, "%lu", (UL) resp->ru.ru_maxrss);

and re-compiling the program.


*** 2. Generate batches

Run:

> Rscript genbatches.R

This creates folders "b4G", "b6G", "b25G", "b32G", "running", "tmpdata" in SCRATCHDIR.
"b4G", "b6G", "b25G" and "b32G" will be populated with all simulation batches (250 for
each specification).


*** 3. Block the batches

The batches are divided into folders by different memory use.

"b4G" uses approximately 4 GB per core.
"b6G" uses approximately 6 GB per core.
"b25G" less than 25 GB per core.
"b32G" uses approximately 32 GB per core, but can have spikes to 48 GB.

To do the blockings, run:

> ./runbatches.sh $ID $CORES b4G "Rscript batch.R"
> ./runbatches.sh $ID $CORES b6G "Rscript batch.R"
> ./runbatches.sh $ID $CORES b25G "Rscript batch.R"
> ./runbatches.sh $ID $CORES b32G "Rscript batch.R"

where $ID is an arbitrary identifier number and $CORES instructs how many cores should
be used. Note that memory requirement are per core, so ensure that the machine this is
run on has at least MEMORY * $CORES of RAM.

Let these commands run until "b4G", "b6G", "b25G", "b32G", "running", "tmpdata" are empty.

The results are collected as files in the folder "results" (already populated in this version).


*** 4. Collect batches

Collect the run time and memory use information into a single RData file:

> Rscript collect.R

The result is saved as "results.RData" which already exists in this version.


*** 5. Generate tables and figures

Run:

> Rscript make_figure.R
> Rscript make_tables.R

Tables and figure are saved in the folder "latex".
These are already produced in this version.
