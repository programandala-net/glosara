# Glosara

## Description

Glosara is a glosser for Forth sources, though it could be used with
other programming languages as well. Its goal is to extract the glossary
and modules documentation out of the sources and build the documents in
[Asciidoctor](http://asciidoctor.org) format, with cross references.

## Current status

Glosara is very stable. It’s being used to build the documentation of
[Solo Forth](http://programandala.net/en.program.solo_forth.html) ([Solo
Forth in SourceHut](https://hg.sr.ht/~programandala_net/solo_forth)),
[Galope](http://programandala.net/en.program.galope.html) ([Galope in
SourceHut](https://hg.sr.ht/~programandala_net/galope)) and other Forth
projects of the author, and it’s being completed and improved during the
process.

## Requirements

Glosara uses some modules from the following libraries:

- [Forth Foundation Library](http://irdvo.github.io/ffl/)

- [Galope](http://programandala.net/en.program.galope.html) ([Galope in
  SourceHut](https://hg.sr.ht/~programandala_net/galope))

## Usage

    Usage: glosara  { [option...] { <input-file> | <-i file> } [option...] ...}

    Note: both `input-file` and the `-i` option start the program
    immediately with the currently parsed options.

      -?, --help     Show this help.
      -V, --version  Show version info.
      -i, --input    Set a file containing a list of input files (one per line).
                     This option, as well as an explicit input file, starts the
                     program immediately, using only the options that were
                     specified before it.
      -l, --level    Set the headings level (1..6).
      -o, --output   Set the output file.
      -u, --unique   Flag: reject the duplicated entries.
      -v, --verbose  Flag: activate the verbose mode.
      -m, --markers  Set the glossary entry markers used in the source code;
                     default: "doc{ }doc".
      -a, --annex    Flag: set the annex mode, i.e. just convert the links of the
                     given file, but don't extract glossary entries from it.
      -s, --sections Flag: add a section heading before every glossary entry that
                     starts with a new initial.
      -d, --dir      Set the temp directory; default: /tmp/glosara/.

## Brief history of the code and its repository

- 2015-09: The code was started as part of [Solo
  Forth](http://programandala.net/en.program.solo_forth.html) ([Solo
  Forth in SourceHut](https://hg.sr.ht/~programandala_net/solo_forth))
  in order to build its documentation, but a few months later it became
  an independent project.

- 2016-04-28: A Git repository was created out of the development
  backups and uploaded to
  [GitHub](http://github.com/programandala-net/glosara) in order to
  preserve the evolution of the code from the start and resume its
  development.

- 2020-12-03: The Git repository was converted to
  [Fossil](https://fossil-scm.org), keeping GitHub as a mirror.

- 2023-04-05: The Fossil repository was converted to
  [Mercurial](https://mercurial-scm.org), enabling a better interaction
  with GitHub.

- 2025-02-23: The Mercurial repository was uploaded to
  [SourceHut](https://hg.sr.ht/~programandala_net/glosara), keeping
  GitHub as a mirror.
