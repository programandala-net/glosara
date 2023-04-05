# Makefile

# This file is part of Glosara.

# By Marcos Cruz (programandala.net), 2020, 2023.

# Last modified: 20230405T1336+0200.
# See change log at the end of the file.

# ==============================================================
# Requirements {{{1

# Asciidoctor (by Dan Allen, Sarah White et al.)
#   http://asciidoctor.org

# ==============================================================
# Main {{{1

.PHONY: all
all: doc

# ==============================================================
# Change log {{{1

# 2020-12-24: Start. Build an online version of the README file, for the Fossil
# repository.
#
# 2023-04-05: Remove the online documentation rule, after migrating from Fossil
# to Mercurial.
