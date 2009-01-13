HTML_DOCS = docs/clk-in.html docs/clk-out.html docs/clk-entry-search.html \
			docs/clk-cat-entry.html \
			docs/clk-list.html
POD_DOCS = docs/specs.html docs/environment.html
POD2HTML_TMPS = *.tmp

doc: $(HTML_DOCS) $(POD_DOCS)

clean:
	$(RM) $(HTML_DOCS) $(POD2HTML_TMPS)

test:
	prove -lr t

# build HTML documentation for each command
$(POD_DOCS) : docs/%.html : docs/%.pod
	./pod2html $< > $@
$(HTML_DOCS) : docs/%.html : %
	./pod2html $< > $@
