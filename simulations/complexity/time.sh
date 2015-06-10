#!/bin/bash

source envir.sh

$TIMEPROG -f "%S %U %M" $@ 2>&1
