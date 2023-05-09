#!/bin/bash
START=$(date +%s%N)
./RUN
END=$(date +%s%N)
seconds=$(echo "scale=3;($END - $START)/(1*10^09)" | bc)

echo "CPU Time:  $seconds (s)" >> Vortex_Results.txt
echo " " >> Vortex_Results.txt
