TARGET=main.rkt
COLLECTIONPATH="."
TESTS=Tests/Tests.rkt

all:
	clear; racket -S $(COLLECTIONPATH) $(TARGET)

test:
	clear; racket -S $(COLLECTIONPATH) $(TESTS)

