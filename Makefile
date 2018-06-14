
main: *.f90
	h5fc system.f90 io.f90 main.f90 -o main


clean:
	rm -f *.o *.mod main data.h5

.PHONY: clean