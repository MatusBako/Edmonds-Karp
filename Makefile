CFLAGS = -W -Wall -Werror -Wwarn
CC = ghc
BIN = ford-fulkerson
MAIN = Ford
TEST = UnitTests
OTHER = Args Dimacs GraphData

all: $(MAIN) $(TEST)

$(MAIN): 
	$(CC) $(CFLAGS) $(MAIN).hs -o $(BIN)

$(TEST): 
	$(CC) $(CFLAGS) -main-is $(TEST) $(TEST).hs

clean: 
	rm -f *.hi *.o $(BIN) $(TEST)

test: $(TEST)
	./$(TEST)

run: $(BIN)
	./$(BIN)

zip:
	zip -r flp-fun-xbakom01.zip TestInputs *.hs *.sh Makefile README.md
