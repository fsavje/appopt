#!/bin/bash

source envir.sh

JOBID="$1"
CORES="$2"
BATCHDIR="${SCRATCHDIR}$3"
COMMAND="$4"

RUNDIR="${SCRATCHDIR}running"

echo "Starting simulations."
for c in $(seq 1 $CORES); do
    (
		echo "Starting core ${c}."
		while true; do
			BATCHFILE="$(ls -1 $BATCHDIR | sort --random-sort | head -1)"
			if [ "$?" != "0" ] || [ "$BATCHFILE" == "" ]; then
				break
			fi
			echo "Running $BATCHFILE."
			mv $BATCHDIR/$BATCHFILE $RUNDIR/$BATCHFILE
			if [ "$?" == "0" ]; then
				$COMMAND $RUNDIR/$BATCHFILE "${JOBID}-${c}"
			fi
		done
		echo "Finished core ${c}."
	) &
	sleep 10
done

wait
echo "All simulations done."
