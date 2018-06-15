
main: *.f90
	h5fc -c io.f90
	h5fc io.o main.f90 -o main


clean:
	rm -f *.o *.mod main data.h5

.PHONY: clean