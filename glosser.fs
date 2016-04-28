#! /usr/bin/env gforth

\ glosser.fs

\ XXX UNDER DEVELOPMENT

\ This file is part of Solo Forth
\ http://programandala.net/en.program.solo_forth.html

s" 201511261904" 2constant version

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
\
\ 2015-11-26: First working version: the glossary entries are
\ extracted from the sources of Solo Forth, ordered and joined
\ into a glossary file.

\ ==============================================================
\ Requirements

forth-wordlist set-current

\ From the Galope library
\ (http://programandala.net/en.program.galope.html)

require galope/unslurp-file.fs
require galope/minus-extension.fs
require galope/string-suffix-question.fs
require galope/string-slash.fs

\ ==============================================================
\ Config

s" /tmp/" 2constant temp-directory
s" .glossary_entry" 2constant entry-filename-extension

\ ==============================================================
\ Source interpreter

: set-normal-order  ( -- )
  only forth  ;

0 value extract-wordlist  ( -- wid )
  \ words recognized during the interpretation of
  \ a source file

0 value entry-wordlist  ( -- wid )
  \ words recognized during the interpretation of
  \ a glossary entry in a source file

: set-source-order  ( -- )
  extract-wordlist >order seal  ;
  \ set the search order needed to parse the source

: set-entry-order  ( -- )
  entry-wordlist >order seal  ;
  \ set the search order needed to parse a glossary entry

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
  \ cr ." End of entry? " 2dup type key drop  \ XXX INFORMER
  s" }doc" str=  ;
  \ is the given string the end of a glossary entry?

0 value entry-fid

: create-entry-file  ( ca len -- )
  w/o create-file throw  to entry-fid  ;

: close-entry-file  ( -- )
  entry-fid close-file throw  0 to entry-fid  ;

: >entry-file  ( ca len -- )
  entry-fid write-file throw  ;

: >entry-file-line  ( ca len -- )
  entry-fid ?dup if  write-line throw  else  2drop  then  ;

: end-of-glossary-entry  ( -- )
  \ cr ." End of entry -- press any key" key drop  \ XXX INFORMER
  0 parse 2drop  \ discard the rest of the line
  close-entry-file set-source-order  ;
  \ end of the current glossary entry

  \ XXX TODO
  \
: c>hex  ( c -- ca len )
  base @ >r hex  s>d <# # # #>  r> base !  ;
  \ convert a character to a 2-char string with its hex value

: string>hex  ( ca1 len1 -- ca2 len2 )
  s" " 2swap bounds do  i c@ c>hex s+  loop  ;
  \ _ca2 len2_ consists of the hex values of the characters of
  \ _ca1 len1_ (one 2-digit hex number per original character)

: entry-files-pattern  ( -- ca len )
  temp-directory s" *" s+ entry-filename-extension s+  ;
  \ wildcard patter for all temporary entry files

: delete-temp-files  ( -- )
  entry-files-pattern  s" rm -f " 2swap s+ system  ;
  \ delete all temporary entry files

: entryname>filename  ( ca1 len1 -- ca2 len2 )
  s" 000000000000000000000000000000000000000000000000000000000000"
  2swap string>hex s+ 62 string/
  entry-filename-extension s+
  temp-directory 2swap s+  ;
  \ convert the name of a glossary entry (a Forth word) to its
  \ temporary filename. the base filename consists of 31 8-bit hex
  \ numbers that represent the characters of the entry name.

: new-entry  ( ca len -- )
  entryname>filename create-entry-file  
  s" == " >entry-file  ;
  \ start a new entry, creating its output file

: get-entry-line  ( "text<eol>" -- )
  \ ." valid-entry-line: " 2dup type ~~ cr key drop  \ XXX INFORMER
  0 parse >entry-file-line cr  ;
  \ parse a line of the current entry and write it to its file

: entry-line  ( "text<eol>" -- )
  save-input parse-name
  \ ~~ ." parsed: " 2dup type cr key drop  \ XXX INFORMER
  2dup end-of-glossary-entry?
  if  2drop restore-input end-of-glossary-entry exit  then
  dup 0<> entry-fid 0= and
  if  new-entry  else  2drop  then
  restore-input throw  get-entry-line  ;
  \ parse and type a line of glossary entry

\ ----------------------------------------------

forth-extract-wordlist set-current
: doc{  ( -- )  set-entry-order  ;
  \ start a glossary entry

z80-extract-wordlist set-current
: doc{  ( -- )  set-entry-order  ;
  \ start a glossary entry

forth-entry-wordlist set-current
: \  ( "text<eol>"  -- )  entry-line  ;
  \ start of glossary entry line in a Forth source

z80-entry-wordlist set-current
: ;  ( "text<eol>"  -- )  entry-line  ;
  \ start of glossary entry line in a Z80 source

\ ==============================================================
\ File converter

forth-wordlist set-current

: z80-source-file?  ( ca len -- f )
  2dup s" .z80s" string-suffix? if  2drop true exit  then
       s" .asm" string-suffix? if  true exit  then
  false  ;
  \ is the filename _ca len_ a Z80 source?
  \ if not, it's supposed to be a Forth source

: parse-current-source  ( -- )
  begin  refill  while
    begin  parse-name dup  while
      \ 2dup ." <" type ." > " \ key drop  \ XXX INFORMER
      find-name ?dup if  name>int execute  then
    repeat  2drop
  repeat  ;
  \ parse the current source file

: parse-file  ( fid -- )
  set-source-order
  ['] parse-current-source execute-parsing-file
  set-normal-order  ;
  \ parse the file _fid_

: get-wordlists  ( ca len -- wid1 wid2 )
  z80-source-file?
  if    z80-extract-wordlist z80-entry-wordlist
  else  forth-extract-wordlist forth-entry-wordlist
  then  ;
  \ return the proper word lists for the source file _ca len_

: set-wordlists  ( ca len -- )
  get-wordlists  to entry-wordlist  to extract-wordlist  ;
  \ set the proper word list for the source file _ca len_

: parse-source-file  ( ca len -- )
  2dup set-wordlists  r/o open-file throw  parse-file  ;
  \ extract the glossary information from file _ca len_ and
  \ print it to standard output

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

: parse-source-files  ( n -- )
  0 do  i 1+ arg parse-source-file  loop  argc off  ;

: join-entries  ( -- )
  s" cat  " entry-files-pattern  s+ 
  s"  > /tmp/glossary.adoc" s+ system  ;
  \ XXX TMP
  \ XXX TODO

: create-glossary-file  ( -- )
  \ XXX TODO header
  join-entries  ;

: glossary  ( n -- )
  delete-temp-files parse-source-files create-glossary-file  ;
  \ create a glossary of entries extracted from _n_ parameter files

: run  ( -- )
  input-files ?dup
  if  glossary  else  about  then  set-normal-order  ;

run bye

\ vim: textwidth=64
