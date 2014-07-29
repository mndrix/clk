.PHONY: all

all: bin/clk

bin/clk: bin/clk.pl
	swipl --toplevel=main -o bin/clk -c bin/clk.pl

