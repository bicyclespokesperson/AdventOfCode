COMPILER := ghc-9.2.5
SRCS := $(wildcard *.hs)

Runner: $(SRCS)
	$(COMPILER) -o $@ $(SRCS)

.PHONY: run
run: Runner
	./$<

.PHONY: clean
clean:
	rm -f *.o *.hi Runner
