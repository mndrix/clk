HTML_DOCS = docs/clk-in.html docs/clk-entry-search.html
POD2HTML_TMPS = *.tmp

doc: $(HTML_DOCS)

clean:
	$(RM) $(HTML_DOCS) $(POD2HTML_TMPS)

test:
	prove -lr t

# build HTML documentation for each command
$(HTML_DOCS) : docs/%.html : %
	pod2html --podroot=$(PWD) --podpath=. --infile=$< --outfile=$@
