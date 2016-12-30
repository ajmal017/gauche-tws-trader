TARGET=tcp-echo-server
GAUCHEDIR=../Gauche
GAUCHELIBDIR=../Gauche/src
CFLAGS=-I$(GAUCHEDIR)/src -I$(GAUCHEDIR)/gc/include
LFLAGS=-L/usr/local/lib -luv -L$(GAUCHELIBDIR) -lgauche-0.9

build: $(TARGET)

run: $(TARGET)
	./$(TARGET)

$(TARGET): main.c
	$(CC) -g -I/usr/local/include -o $(TARGET) main.c $(CFLAGS) $(LFLAGS)
