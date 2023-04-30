CC = ghc
II = ghci
FLAGS = -threaded -g2
OBJECTS = Main.hs Dibujo.hs FloatingPic.hs Grilla.hs Interp.hs Pred.hs
EXAMPLES =  Dibujos/Ejemplo.hs Dibujos/Feo.hs Dibujos/Grilla_cord.hs
OBJECTIVE = Main

all: $(OBJECTS)
	$(CC) $(FLAGS) $(OBJECTIVE).hs

clean: 
	rm -f *.o *.hi $(OBJECTS.:.hs=.o) $(OBJECTS.:.hs=.hi) $(OBJECTIVE)
	rm -f Dibujos/*.o Dibujos/*.hi

execute:
	make clean
	make

interp: $(OBJECTS)
	$(II) $(FLAGS) $(OBJECTIVE).hs
