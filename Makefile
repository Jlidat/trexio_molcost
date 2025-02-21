FC=gfortran

DEBUG_FLAGS=-fcheck=all -Wconversion -Wuninitialized -Wall -g -Wno-tabs -Wline-truncation -Wreal-q-constant -fbacktrace -finit-real=nan -ffpe-trap=zero
FCFLAGS=$(shell pkg-config --cflags trexio) $(DEBUG_FLAGS)
LIBS=$(shell pkg-config --libs trexio)

test_trexio: test.o trexio_module.o
	$(FC) -o $@ $(FCFLAGS) $^ $(LIBS)

%.o: %.F90 
	$(FC) -c $< 

%.o: %.f90 trexio_module.o
	$(FC) -c $< $(FCFLAGS) 

.PHONY: clean

clean:
	rm *.o
