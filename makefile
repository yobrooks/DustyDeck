# Makefile to build lbstime.a library and driver routines 
#
# Andrew J. Pounds, Ph.D.
# Departments of Chemistry and Computer Science
# Mercer University
# Spring 2007
#

FC = gfortran    
CC  = gcc 
CFLAGS = -O3
OPTFLAGS = -O1 -fthread-jumps

TIMINGLIBS =  -L./ -llbstime 
CLIBS = -lm
OBJS = cputime.o walltime.o  

# Three different targets for all three dusty versions
all: lib dusty dustyC dustyF

# Timing files
cputime.o : cputime.cc   
	$(CC) $(CFLAGS) -c cputime.cc  

walltime.o : walltime.cc   
	$(CC) $(CFLAGS) -c walltime.cc  

#Original F77 dusty file
dusty.o : dusty.f  
	$(FC) $(OPTFLAGS) -c dusty.f

dusty : dusty.o lib  $(OBJS)
	$(FC) $(OPTFLAGS) -o dusty dusty.o  $(TIMINGLIBS) -lstdc++ 

# C++ dusty file
dustyC.o : dustyC.cpp
	$(CC) $(CLIBS) $(OPTFLAGS) -c dustyC.cpp

dustyC : dustyC.o lib  $(OBJS)
	$(CC) $(CLIBS) $(OPTFLAGS) -o dustyC dustyC.o  $(TIMINGLIBS) -lstdc++ 

# F90 dusty files
mymod.o : mymod.f90
	$(FC) $(OPTFLAGS) -c mymod.f90

dustyF.o : dustyF.f90 mymod.o
	$(FC) $(OPTFLAGS) -c mymod.f90 dustyF.f90

dustyF : dustyF.o mymod.o lib  $(OBJS) 
	$(FC) $(OPTFLAGS) -o dustyF dustyF.o mymod.o  $(TIMINGLIBS) -lstdc++


# Default Targets for Cleaning up the Environment
clean :
	rm *.o
	rm *.a

pristine :
	rm *.o
	rm *.a
	touch *.c *.f *.f90

ctags :
	ctags  *.c *.f *.f90

# Target for making the library

lib: $(OBJS) 
	ar -rc liblbstime.a $(OBJS) 
	ranlib liblbstime.a
