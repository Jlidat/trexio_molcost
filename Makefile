FC=gfortran

DEBUG_FLAGS=-fcheck=all -Wconversion -Wuninitialized -Wall -g -Wno-tabs -Wline-truncation -Wreal-q-constant -fbacktrace -finit-real=nan -ffpe-trap=zero
FCFLAGS=$(shell pkg-config --cflags trexio) $(DEBUG_FLAGS)

LIBS=$(shell pkg-config --libs trexio)



trexioMain_5d: trexioMain_5d.o trexio_module.o trexio2info_5d.o trexio2inporb.o trexio2mono.o script.o ecriS.o 
	$(FC) -o $@ $(FCFLAGS) $^ $(LIBS)

trexio2info_5d: trexio2info_5d.o  trexio_module.o 
	$(FC) -o $@ $(FCFLAGS) $^ $(LIBS)

trexio2inporb:trexio2inporb.o trexio_module.o
	$(FC) -o $@ $(FCFLAGS) $^ $(LIBS)

trexio2mono:trexio2mono.o ecriS.o script.o trexio_module.o
	$(FC) -o $@ $(FCFLAGS) $^ $(LIBS)


locorb2trexio_5d: locorb2trexio_5d.o trexio_module.o
	$(FC) -o $@ $(FCFLAGS) $^ $(LIBS)

#locorb2trexio: locorb2trexio.o trexio_module.o
#	$(FC) -o $@ $(FCFLAGS) $^ $(LIBS)

#trexioMain: trexioMain.o trexio2info.o trexio2inporb.o trexio2mono.o trexio_module.o script.o ecriS.o 
#	$(FC) -o $@ $(FCFLAGS) $^ $(LIBS)

#trexio2info: trexio2info.o trexio_module.o 
#	$(FC) -o $@ $(FCFLAGS) $^ $(LIBS)


%.o: %.F90 
		$(FC) -c $< 

%.o: %.f trexio_module.o
	$(FC) -c $(FCFLAGS) $< -std=legacy

%.o: %.f90 trexio_module.o
	$(FC) -c $< $(FCFLAGS) 

#%.o: %.F90
#	$(FC) -c $< 

.PHONY: clean

clean:
	rm *.o
