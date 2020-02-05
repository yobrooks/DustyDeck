# Makefile to build lbstime.a library and driver routines 
#
# Andrew J. Pounds, Ph.D.
# Departments of Chemistry and Computer Science
# Mercer University
# Spring 2007
#

F90 = gfortran    
CC  = gcc 
CFLAGS = -O3

TIMINGLIBS =  -L./ -llbstime 
CLIBS = -lm

OBJS = cputime.o walltime.o  

all: dusty lib

cputime.o : cputime.cc   
	$(CC) $(CFLAGS) -c cputime.cc  

walltime.o : walltime.cc   
	$(CC) $(CFLAGS) -c walltime.cc  

dusty.o : dusty.f90  
	$(F90) -c dusty.f90   

# Don't forget the -lstdc++
dusty : dusty.o lib  $(OBJS) 
	$(F90) -o dusty dusty.o  $(TIMINGLIBS) -lstdc++  

# Default Targets for Cleaning up the Environment
clean :
	rm *.o
	rm *.a

pristine :
	rm *.o
	rm *.a
	touch *.c *.f90  

ctags :
	ctags  *.c *.f90

# Target for making the library

lib: $(OBJS) 
	ar -rc liblbstime.a $(OBJS) 
	ranlib liblbstime.a
