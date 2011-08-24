
PROGRAM = g2vis5d


# === C-COMPILER ---

# DEC
#CC = cc
#CFLAGS = -c -DLITTLE -DUNDERSCORE
#
# SGI
#CC = cc
#CFLAGS = -c -cckr -DUNDERSCORE
#
# GCC
# OPTIMIZATION OF V5D IS NOT WORKING WITH gcc!!!
#CC = gcc
#CFLAGS = -c -O0 -DUNDERSCORE
#
# Intel
CC = gcc
CFLAGS = -c -O0 -DUNDERSCORE -Wall -g -fbounds-check

# === FORTRAN90 COMPILER ---

FFLAGS = -c -O0 -Wall  -g -fbounds-check 
#FLOAD = -O0 -Wl,--rpath -Wl,/opt/NECcomp/compiler90/ia64/lib
FLOAD = -O0
F77 = gfortran
LIBS = -lm

OBJECTS = \
	g2vis5d.o \
	grads.o \
	vis5d.o \
	binio.o \
	v5d.o \
	Time.o \
	Constants.o \
	String.o \
	getch.o


$(PROGRAM): $(OBJECTS)
	$(F77) $(FLOAD) $(OBJECTS) $(LIBS) -o $@

Time.o: Time.f90
	$(F77) $(FFLAGS) Time.f90

String.o: String.f90
	$(F77) $(FFLAGS) String.f90

Constants.o: Constants.f90
	$(F77) $(FFLAGS) Constants.f90

grads.o: grads.f90 String.o Time.o Constants.o
	$(F77) $(FFLAGS) grads.f90

vis5d.o: vis5d.f90 grads.o
	$(F77) $(FFLAGS) vis5d.f90

g2vis5d.o: g2vis5d.f90 grads.o vis5d.o
	$(F77) $(FFLAGS) g2vis5d.f90

getch.o:  getch.c
	$(CC) $(CFLAGS) getch.c 

# hmjb os arquivos a seguir foram copiados do vis5d 

binio.o:  binio.c
	$(CC) $(CFLAGS) binio.c -o binio.o

v5d.o:  v5d.c
	$(CC) $(CFLAGS) v5d.c -o v5d.o

clean: 
	rm -f $(OBJECTS) $(PROGRAM) *.mod *~
