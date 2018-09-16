#
#  $Header:$
#


fflags = -O2 -Wall
fc = gfortran
RM := rm -rf

include config.mk

.SUFFIXES:
.SUFFIXES: .f90 .F .o

.90.o:
	

OBJS = lunar_reflectance.f90  make_lunar_file.f90  readh5dataset.f90 lunar_irradiance.f90
all: dnb_tools

dnb_tools: 
	@echo 'Building target: $@'
	@echo 'Invoking: Intel(R) Fortran Linker'
	$(fc)  -c ${fflags} readh5dataset.f90  ${hdf5libs} $(hdflibs) ${hdf5links}
	$(fc) -c  ${fflags} lunar_reflectance.f90   ${hdf5libs} $(hdflibs) ${hdf5links}
	$(fc) -c  ${fflags} lunar_irradiance.f90   ${hdf5libs} $(hdflibs) ${hdf5links}
	$(fc) ${fflags}  -o "../dnb_start" $(OBJS) $(LIBS) ${hdf5libs} $(hdflibs) ${hdf5links}
	@echo 'Finished building target: $@'
	@echo ' '	

clean:
	rm -f *.o *.mod
	rm -f dnb_start
	-@echo ' '
	
.PHONY: all clean dependents

make_lunar_file.o: make_lunar_file.f90 readh5dataset.o lunar_reflectance.o lunar_irradiance.o
