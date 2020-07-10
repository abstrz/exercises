CC=gcc
CFLAGS=-I .
DEPS = graph_matrix.h arr.h
OBJ = graph_matrix.o exercises.o arr.o

%.o: %.c $(DEPS)
	$(CC) -c -o $@ $< $(CFLAGS)

gm: $(OBJ)
	$(CC) -o $@ $^ $(CFLAGS) 
