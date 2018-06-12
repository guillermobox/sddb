
main: *.f90
	h5fc system.f90 io.f90 main.f90 -o main
	./main
	h5dump data.h5

clean:
	rm -f *.o *.mod main data.h5

.PHONY: clean