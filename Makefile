.PHONY: all tags

all:
	swipl --toplevel=main -o bin/clk -c bin/clk.pl

tags:
	ctags -R .
