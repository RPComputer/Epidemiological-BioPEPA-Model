#*****************************************************************************
#*  FILE:    Makefile
#*
#*  AUTHOR:  Stephen Gilmore
#*
#*  CREATED: February 3, 2008
#*
#*****************************************************************************

CSE_CPP_HOME = ../../..
include ../../../makefile.config

all:
	rm -f stochkit
	${CPP} -c ProblemDefinition.cpp ${CPP_OPTIONS}
	${CPP} -c stochkit.cpp ${CPP_OPTIONS}	
	${CPP} -o stochkit stochkit.o ProblemDefinition.o ${LINK_OPTIONS} 

clean : 
	rm -f ${OBJS} *.o core* *~
