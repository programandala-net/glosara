#! /usr/bin/env gforth

\ glosser.fs

\ XXX UNDER DEVELOPMENT

\ This file is part of Solo Forth
\ http://programandala.net/en.program.solo_forth.html

s" 20151125" 2constant version

\ ==============================================================
\ Description

\ glosser is a command line tool that extracts the glossary
\ documentation from the Solo Forth sources, and creates a glossary
\ document in the Asciidoctor format.

\ ==============================================================
\ Author and license

\ Copyright (C) 2015 Marcos Cruz (programandala.net)

\ You may do whatever you want with this work, so long as you retain
\ the copyright notice(s) and this license in all redistributed copies
\ and derived works. There is no warranty.

\ ==============================================================
\ Acknowledgements

\ glosser is written in Forth with Gforth 0.7.3 (by Anton Ertl,
\ Bernd Paysan et al.):
\   http://gnu.org/software/gforth

\ ==============================================================
\ History

\ 2015-09-25: Start.
\
\ 2015-11-04: Renamed to "glosser". Some changes. Deferred
\ vocabularies for parsing.
\
\ 2015-11-25: First draft.

\ ==============================================================
\ Requirements

forth-wordlist set-current

\ From the Galope library
\ (http://programandala.net/en.program.galope.html)

require galope/unslurp-file.fs
require galope/minus-extension.fs
require galope/string-suffix-question.fs

\ ==============================================================
\ Source interpreter

0 value extract-wordlist  ( -- wid )
  \ words recognized during the interpretation of
  \ a source file

0 value entry-wordlist  ( -- wid )
  \ words recognized during the interpretation of
  \ a glossary entry in a source file

wordlist constant forth-extract-wordlist  ( -- wid )
  \ words recognized during the interpretation of
  \ a Forth source file

wordlist constant z80-extract-wordlist  ( -- wid )
  \ words recognized during the interpretation of
  \ a Z80 source file

wordlist constant forth-entry-wordlist  ( -- wid )
  \ words recognized during the interpretation of
  \ a glossary entry in a Forth source file

wordlist constant z80-entry-wordlist  ( -- wid )
  \ words recognized during the interpretation of
  \ a glossary entry in a Z80 source file

: end-of-glossary-entry?  ( ca len -- f )
  cr ." End of entry? " 2dup type key drop  \ XXX INFORMER
  s" }doc" str=  ;
  \ is the given string the end of a glossary entry?

: end-of-glossary-entry  ( -- )
  cr ." End of entry -- press any key" key drop  \ XXX INFORMER
  0 parse 2drop  \ discard the rest of the line
  extract-wordlist >order seal  ;

: glossary-line  ( "text<eol>" -- )
  parse-name 2dup end-of-glossary-entry?
  if    2drop end-of-glossary-entry
  else  type space 0 parse type
  then  ;
  \ parse and type a glossary line

: (doc{)  ( -- )
  entry-wordlist >order seal  ;
  \ start a glossary entry

\ ----------------------------------------------

forth-extract-wordlist set-current

: doc{  ( -- )
  \ cr ." Entry found -- press any key" key drop  \ XXX INFORMER
  (doc{)  ;
  \ start a glossary entry

\ ----------------------------------------------

z80-extract-wordlist set-current

: doc{  ( -- )
  cr ." Entry found -- press any key" key drop  \ XXX INFORMER
  (doc{)  ;
  \ start a glossary entry

\ ----------------------------------------------

forth-entry-wordlist set-current

: \  ( "text<eol>"  -- )  glossary-line  ;
  \ start of glossary line

\ ----------------------------------------------

z80-entry-wordlist set-current

: ;  ( "text<eol>"  -- )  glossary-line  ;
  \ Start of line of glossary entry.

\ ==============================================================
\ File converter

forth-wordlist set-current

: z80-source-file?  ( ca len -- f )
  2dup s" .z80s" string-suffix? if  2drop true exit  then
       s" .asm" string-suffix? if  true exit  then
  false  ;
  \ is the filename _ca len_ a Z80 source?
  \ if not, it's supposed to be a Forth source

: (parse-source)  ( -- )
  begin   parse-name
          \ 2dup type space  \ XXX INFORMER
          dup
  while   find-name ?dup if  name>int execute  then
  repeat  2drop  ;
  \ parse the current source

: restore-order  ( -- )
  only forth  ;

: parse-source  ( ca len -- )
  extract-wordlist >order seal
  ['] (parse-source) execute-parsing
  restore-order  ;

: get-wordlists  ( ca len -- wid1 wid2 )
  z80-source-file?
  if    z80-extract-wordlist z80-entry-wordlist
  else  forth-extract-wordlist forth-entry-wordlist
  then  ;
  \ return the proper wordlist for the source file _ca len_

: set-wordlists  ( ca len -- )
  get-wordlists  to entry-wordlist  to extract-wordlist  ;
  \ set the proper wordlist for the source file _ca len_

: parse-source-file  ( ca len -- )
  2dup set-wordlists  slurp-file parse-source  ;
  \ get the contents of the source file _ca len_, extract the
  \ glossary information and print it to standard output

: glosser  ( ca len -- )
  \ ." About to parse " 2dup type cr  \ XXX INFORMER
  \ ." Press any key" key drop  \ XXX INFORMER
  2>r  get-order  2r> parse-source-file  set-order  ;
  \ extract the glossary information from the source file
  \ _ca len_ and print it to standard output

: about  ( -- )
  ." glosser" cr
  ." Forth source to glossary converter" cr
  ." Version " version type cr
  ." http://programandala.net/en.program.solo_forth.html" cr cr
  ." Copyright (C) 2015 Marcos Cruz (programandala.net)" cr cr
  ." Usage:" cr
  ."   glosser input_file" cr
  ." Or (depending on the installation method):" cr
  ."   glosser.fs input_file" cr cr
  ." The input file may be a Forth source or a Z80 source." cr  ;

: input-files  ( -- n )
  argc @ 1-  ;
  \ number of input files in the command line

: run  ( -- )
  input-files ?dup
  if    0 do  i 1+ arg glosser  loop
  else  about  then  ;

run

\ vim: textwidth:64
