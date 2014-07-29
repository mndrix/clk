.PHONY: all

all:
	swipl --toplevel=main -o bin/clk -c bin/clk.pl

