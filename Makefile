# Makefile

# This file is part of Glosara.

# By Marcos Cruz (programandala.net), 2020.

# Last modified: 202012241728.
# See change log at the end of the file.

# ==============================================================
# Requirements {{{1

# Asciidoctor (by Dan Allen, Sarah White et al.)
#   http://asciidoctor.org

# ==============================================================
# Main {{{1

.PHONY: all
all: doc

.PHONY: doc
doc: wwwdoc

.PHONY: clean
clean: cleanwww

# ==============================================================
# Online documentation {{{2

# Online documentation displayed on the Fossil repository.

.PHONY: wwwdoc
wwwdoc: wwwreadme

.PHONY: cleanwww
cleanwww:
	rm -f \
		doc/www/* \
		tmp/README.*

.PHONY: wwwreadme
wwwreadme: doc/www/README.html

doc/www/README.html: tmp/README.html
	echo "<div class='fossil-doc' data-title='README'>" > $@;\
	cat $< >> $@;\
	echo "</div>" >> $@

tmp/README.html: README.adoc
	asciidoctor \
		--embedded \
		--out-file=$@ $<

# ==============================================================
# Change log {{{1

# 2020-12-24: Start. Build an online version of the README file, for the Fossil
# repository.
