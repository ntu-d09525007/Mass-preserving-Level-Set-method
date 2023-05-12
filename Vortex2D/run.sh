#!/bin/bash

rm -rf Vortex_Results.txt

for r in 1.0 0.1 0.01 0.001 
do
	sed -i -e "50 c $r\t0.01" input/default.txt
	for dx in 32 64 128 256
	do
		sed -i -e "4 c Vortex_$r_$dx" input/default.txt
		sed -i -e "14 c $dx" input/default.txt
		START=$(date +%s%N)
		./RUN
		END=$(date +%s%N)
		seconds=$(echo "scale=3;($END - $START)/(1*10^09)" | bc)

		echo "CPU Time:  $seconds (s)" >> Vortex_Results.txt
		echo " " >> Vortex_Results.txt

	done
done
