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


*** 2. Run tests

Run:

> ./runall.sh

Ensure that the machine this is run on has at least 48 GB of accessible RAM.

The results will be collected in the folder "result" (already populated in this
version) where each file contains the result from "time".

The results are summarized in the file "summary.txt", where "OK" indicates
no problem, "TIME" indicate a run time of more than 3 hours and "MEM" a
memory use of more than 48 GB.
