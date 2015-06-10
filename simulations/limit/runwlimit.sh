#!/bin/bash

source envir.sh

ulimit -St 10800 -Sv 50331648

$TIMEPROG -v Rscript algorithms/$1 "${SCRATCHDIR}$2" 2>&1

if [ "$?" == "0" ]; then
	echo "all well"
fi
