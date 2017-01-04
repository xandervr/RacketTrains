TARGET=main.rkt
COLLECTIONPATH="."

all:
	clear; racket -S $(COLLECTIONPATH) $(TARGET)
