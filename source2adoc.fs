#! /usr/bin/env gforth

\ source2adoc.fs

\ This file is part of Solo Forth
\ http://programandala.net/en.program.solo_forth.html

s" A-00-20150925" 2constant version

\ ==============================================================
\ Description

\ source2adoc is a command line tool that extracts the glossary
\ documentation from the Solo Forth sources, and creates a glossary
\ document en the Asciidoctor format.

\ ==============================================================
\ Author and license

\ Copyright (C) 2015 Marcos Cruz (programandala.net)

\ You may do whatever you want with this work, so long as you retain
\ the copyright notice(s) and this license in all redistributed copies
\ and derived works. There is no warranty.

\ ==============================================================
\ Acknowledgements

\ pbm2scr is written in Forth with Gforth 0.7.3 (by Anton Ertl,
\ Bernd Paysan et al.):
\   http://gnu.org/software/gforth

\ ==============================================================
\ History

\ 2015-09-25: Start. Version A-00.

\ ==============================================================
\ Requirements

forth definitions

\ From the Galope library
\ (http://programandala.net/en.program.galope.html)

require galope/unslurp-file.fs
require galope/minus-extension.fs

\ ==============================================================
\ Source interpreter

wordlist constant extract-wordlist
  \ it holds the only word recognized during the interpretation of
  \ a source file

wordlist constant entry-wordlist
  \ it holds the words recognized during the interpretation of
  \ a glossary entry

extract-wordlist set-current

: doc{  ( -- )
  \ Start of glossary entry.
  ;

entry-wordlist set-current

: }doc  ( -- )
  \ End of glossary entry.
  ;

: \  ( "text<eol>"  -- )
  ;

: ;  ( "text<eol>"  -- )
  ;

: (parse-source)  ( -- )
  \ Parse the current source.
  begin   parse-name ?dup
  while   find-name ?dup if  name>int execute  then
  repeat  drop
  ;
: parse-source  ( ca len -- )
  ['] (parse-source) execute-parsing
  ;

  \ XXX TODO

: parse-source?  ( ca len -- wf )
  \ Parse a give source.
  \ ca len = source
  \ wf = no error?
  \
  extract-wordlist seal
  ['] parse-source catch
  dup if  nip nip  then  \ fix the stack
  dup ?wrong 0=
  \ restore_vocabularies
  no_parsing_error_left? and
  ;


\ ==============================================================
\ File converter

forth definitions

: working-dir  ( -- ca len )
  \ Current working directory.
  s" PWD" getenv
  ;
: working-dir+  ( ca1 len1 -- ca2 len2 )
  \ Add the current working directory to a file name.
  working-dir s" /" s+ 2swap s+
  ;
: save-scr  ( ca len -- )
  \ Save the SCR buffer to the output file.
  \ ca len = input file name
  -extension s" .scr" s+ zxscr /zxscr 2swap unslurp-file
  ;
: (source>glossary)  ( ca len -- )
  \ ca len = input file name
  2>r  get-order
  init-zxscr adoc-wordlist >order seal
  2r@ working-dir+ included
  set-order  2r> save-scr
  ;
: about  ( -- )
  ." source>glossary" cr
  ." Forth source to glossary document converter" cr
  ." Version " version type cr
  ." http://programandala.net/en.program.solo_forth.html" cr cr
  ." Copyright (C) 2015 Marcos Cruz (programandala.net)" cr cr
  ." Usage:" cr
  ."   source2glossary input_file" cr
  ." Or (depending on the installation method):" cr
  ."   source2glossary.fs input_file" cr cr
  ." The input file may be a Forth source or a Z80 source." cr
  ;
: input-files  ( -- n )
  \ Number of input files in the command line.
  argc @ 1-
  ;
: source>glossary  ( -- )
  input-files ?dup
  if    0 do  i 1+ arg (source>glossary)  loop
  else  about  then
  ;

source>glossary bye
