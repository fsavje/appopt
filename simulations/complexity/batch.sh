#!/bin/bash

source envir.sh

BATCHFILE="$1"
source $BATCHFILE
OUTFILE="results/$2"

DATAFILE="$(Rscript gendata.R $BATCHSET)"

if [ "$?" == "0" ]; then
	./time.sh Rscript algorithms/dry.R $DATAFILE >> $OUTFILE

	if [ "$TORUN" == "all" ] || [ "$TORUN" == "appopt" ]; then
		./time.sh Rscript algorithms/aoDLAV.R $DATAFILE >> $OUTFILE
		./time.sh Rscript algorithms/aoULAV.R $DATAFILE >> $OUTFILE
		./time.sh Rscript algorithms/aoD2AG.R $DATAFILE >> $OUTFILE
		./time.sh Rscript algorithms/aoD2SG.R $DATAFILE >> $OUTFILE
	fi
	if [ "$TORUN" == "all" ]; then
		./time.sh Rscript algorithms/moore.R $DATAFILE >> $OUTFILE
		./time.sh Rscript algorithms/nbpm.R $DATAFILE >> $OUTFILE
		./time.sh Rscript algorithms/greedy.R $DATAFILE >> $OUTFILE
	fi

	rm $DATAFILE
	rm $BATCHFILE
fi
