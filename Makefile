FC=gfortran

FCFLAGS=$(shell pkg-config --cflags trexio)
LIBS=$(shell pkg-config --libs trexio)

test_trexio: test.o trexio_module.o
	$(FC) -o $@ $(FCFLAGS) $^ $(LIBS)

%.o: %.F90 
	$(FC) -c $<

%.o: %.f90 trexio_module.o
	$(FC) -c $<

.PHONY: clean

clean:
	rm *.o
