CC=gcc
CFLAGS=-I .
DEPS = graph_matrix.h arr.h string.h
OBJ = graph_matrix.o  arr.o string.o exercises.o

%.o: %.c $(DEPS)
	$(CC) -c -o $@ $< $(CFLAGS)

gm: $(OBJ)
	$(CC) -o $@ $^ $(CFLAGS) 
