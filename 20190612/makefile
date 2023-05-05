all:DATA_MOD.o
	ifort MAIN.F90 DATA_MOD.o -qopenmp -heap-arrays -O3 -o RUN
DATA_MOD.o:
	ifort -c DATA_MOD.F90
clean:
	rm DATA_MOD.o *.mod *.PLT *.dat -rf
