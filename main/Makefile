FC=gfortran

DEBUG_FLAGS=-fcheck=all -Wconversion -Wuninitialized -Wall -g -Wno-tabs -Wline-truncation -Wreal-q-constant -fbacktrace -finit-real=nan -ffpe-trap=zero
FCFLAGS=$(shell pkg-config --cflags trexio) $(DEBUG_FLAGS)

LIBS=$(shell pkg-config --libs trexio)

trexio2info: trexio2info.o trexio_module.o
	$(FC) -o $@ $(FCFLAGS) $^ $(LIBS)


trexio2dolo: trexio2dolo.o trexio_module.o t_Info.o t_Inporb.o t_Mono.o script.o ecriS.o 
	$(FC) -o $@ $(FCFLAGS) $^ $(LIBS)

t_Info: t_Info.o  trexio_module.o 
	$(FC) -o $@ $(FCFLAGS) $^ $(LIBS)

t_Inporb:t_Inporb.o trexio_module.o
	$(FC) -o $@ $(FCFLAGS) $^ $(LIBS)

t_Mono:t_Mono.o ecriS.o script.o trexio_module.o
	$(FC) -o $@ $(FCFLAGS) $^ $(LIBS)


locorb2trexio: locorb2trexio.o trexio_module.o
	$(FC) -o $@ $(FCFLAGS) $^ $(LIBS)


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
