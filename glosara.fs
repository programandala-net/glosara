#! /usr/bin/env gforth

\ glosara.fs

\ XXX UNDER DEVELOPMENT

: version  s" 0.1.0+201701262203" ;

\ ==============================================================
\ Description

\ Glosara is a command line tool that extracts the glossary
\ documentation out of Forth sources, and creates a glossary
\ document in Asciidoctor format.

\ Glosara is written in Forth with Gforth
\ (http://gnu.org/software/gforth).

\ ==============================================================
\ Author and license

\ Copyright (C) 2015, 2016 Marcos Cruz (programandala.net)

\ You may do whatever you want with this work, so long as you
\ retain the copyright notices and this license in all
\ redistributed copies and derived works. There is no warranty.

\ ==============================================================
\ History

\ See at the end of the file.

\ ==============================================================
\ Requirements

forth-wordlist set-current

\ From Forth Foundation Library
\ (http://irdvo.github.io/ffl/)

include ffl/arg.fs  \ argument parser

\ From the Galope library
\ (http://programandala.net/en.program.galope.html)

require galope/unslurp-file.fs
require galope/minus-extension.fs
require galope/string-suffix-question.fs
require galope/string-slash.fs

\ ==============================================================
\ Config

variable verbose      \ flag: verbose mode? \ XXX not used yet
variable options      \ counter: valid options on the command line

s" /tmp/" 2constant temp-directory
s" .glossary_entry" 2constant entry-filename-extension

\ ==============================================================
\ Misc

: echo  ( ca len -- )
  verbose @ if  type cr  else  2drop  then  ;
  \ Print the string _ca len_ if `verbose` is on.

\ ==============================================================
\ Word lists

: set-normal-order  ( -- )
  only forth  ;

0 value extract-wordlist  ( -- wid )
  \ Words recognized during the interpretation of a source file.

0 value entry-wordlist  ( -- wid )
  \ Words recognized during the interpretation of
  \ a glossary entry in a source file.

: set-source-order  ( -- )
  extract-wordlist >order seal  ;
  \ Set the search order needed to parse the source.

: set-entry-order  ( -- )
  entry-wordlist >order seal  ;
  \ Set the search order needed to parse a glossary entry

wordlist constant forth-extract-wordlist  ( -- wid )
  \ Words recognized during the interpretation of
  \ a Forth source file

wordlist constant z80-extract-wordlist  ( -- wid )
  \ Words recognized during the interpretation of
  \ a Z80 source file.

wordlist constant forth-entry-wordlist  ( -- wid )
  \ Words recognized during the interpretation of
  \ a glossary entry in a Forth source file.

wordlist constant z80-entry-wordlist  ( -- wid )
  \ Words recognized during the interpretation of
  \ a glossary entry in a Z80 source file.

\ ==============================================================
\ Files

0 value entry-fid
2variable output-filename  \ filename

: create-entry-file  ( ca len -- )
  w/o create-file throw  to entry-fid  ;

: close-entry-file  ( -- )
  entry-fid close-file throw  0 to entry-fid  ;

: >entry-file  ( ca len -- )
  entry-fid write-file throw  ;

: >entry-file-line  ( ca len -- )
  entry-fid ?dup if  write-line throw  else  2drop  then  ;

: c>hex  ( c -- ca len )
  base @ >r hex  s>d <# # # #>  r> base !  ;
  \ Convert a character to a 2-char string with its hex value.

: string>hex  ( ca1 len1 -- ca2 len2 )
  s" " 2swap bounds do  i c@ c>hex s+  loop  ;
  \ Return a string _ca2 len2_ which consists of the hex values
  \ of the characters of string _ca1 len1_ (one 2-digit hex
  \ number per original character).

: entry-files-pattern  ( -- ca len )
  temp-directory s" *" s+ entry-filename-extension s+  ;
  \ Wildcard pattern for all temporary entry files.

: delete-temp-files  ( -- )
  entry-files-pattern  s" rm -f " 2swap s+ system  ;
  \ Delete all temporary entry files.

: ruler  ( c len -- ca len )
  dup allocate throw swap 2dup 2>r rot fill 2r>  ;
  \ Return a string of _len_ characters _c_.

31 constant max-word-length

: entryname>filename  ( ca1 len1 -- ca2 len2 )
  '0' max-word-length ruler
  2swap string>hex s+ [ max-word-length 2 * ] literal string/
  entry-filename-extension s+
  temp-directory 2swap s+  ;
  \ Convert the name of a glossary entry _ca1 len1_ (a Forth
  \ word) to its temporary filename _ca2 len2_. The base
  \ filename consists of `max-word-length` 8-bit hex numbers
  \ that represent the characters of the entry name.

\ ==============================================================
\ Glossary entries

: end-of-glossary-entry?  ( ca len -- f )
  \ cr ." End of entry? " 2dup type key drop  \ XXX INFORMER
  s" }doc" str=  ;
  \ Is the given string the end of a glossary entry?

: end-of-glossary-entry  ( -- )
  \ cr ." End of entry -- press any key" key drop  \ XXX INFORMER
  0 parse 2drop  \ discard the rest of the line
  close-entry-file set-source-order  ;
  \ End of the current glossary entry.

: new-entry  ( ca len -- )
  entryname>filename create-entry-file
  s" == " >entry-file  ;
  \ Start a new entry, creating its output file.

: get-entry-line  ( "text<eol>" -- )
  \ ." valid-entry-line: " 2dup type ~~ cr key drop  \ XXX INFORMER
  0 parse >entry-file-line cr  ;
  \ Parse a line of the current entry and write it to its file.

: parse-line  ( -- ca len )  0 parse  ;

: entry-line  ( "text<eol>" -- )
  \ XXX FIXME -- why parse names instead of the whole line?
  \ XXX FIXME -- why `save-input`?
  save-input parse-line
\  ~~ ." parsed: " 2dup type cr  \ XXX INFORMER
  \ key drop  \ XXX INFORMER
  2dup end-of-glossary-entry?
  if  2drop restore-input throw end-of-glossary-entry exit  then
  dup 0<> entry-fid 0= and
  if  new-entry  else  2drop  then
  restore-input throw  get-entry-line  ;
  \ Parse and type a line of glossary entry.

\ ==============================================================
\ Interface

forth-extract-wordlist set-current
: doc{  ( -- )  set-entry-order  ;
  \ Start a glossary entry.

forth-entry-wordlist set-current
: \  ( "text<eol>"  -- )  entry-line  ;
  \ Start of glossary entry line in a Forth source.

z80-extract-wordlist set-current
: doc{  ( -- )
\  ~~  \ XXX INFORMER
  set-entry-order
\  ~~  \ XXX INFORMER
  ;
  \ Start a glossary entry.

z80-entry-wordlist set-current
: ;  ( "text<eol>"  -- )
  \ ~~  \ XXX INFORMER
  entry-line
  \ ~~  \ XXX INFORMER
  ;
  \ Start of glossary entry line in a Z80 source.

\ ==============================================================
\ Parser

forth-wordlist set-current

: z80-source-file?  ( ca len -- f )
  2dup s" .z80s" string-suffix? if  2drop true exit  then
       s" .asm"  string-suffix?  ;
  \ Is the filename _ca len_ a Z80 source?
  \ If not, it's supposed to be a Forth source.

: parse-current-source  ( -- )
  begin  refill  while
    begin  parse-name dup  while
    \  ~~
    2dup ." <" type ." > "  \ XXX INFORMER
      \ key drop  \ XXX INFORMER
      find-name ?dup if  name>int execute  then
    repeat  2drop
  repeat  ;
  \ Parse the current source file.

: parse-file  ( fid -- )
  set-source-order
  cr ." parse-file 1" \ XXX INFORMER
  ['] parse-current-source execute-parsing-file
  cr ." parse-file 2" \ XXX INFORMER
  set-normal-order  ;
  \ Parse the file _fid_.

: get-wordlists  ( ca len -- wid1 wid2 )
  z80-source-file?
  if    z80-extract-wordlist z80-entry-wordlist
  else  forth-extract-wordlist forth-entry-wordlist
  then  ;
  \ Return the proper word lists for the source file _ca len_.

: set-wordlists  ( ca len -- )
  get-wordlists  to entry-wordlist  to extract-wordlist  ;
  \ Set the proper word lists for the source file _ca len_.

: parse-source-file  ( ca len -- )
  \ cr ." parse-source-file " 2dup type \ XXX INFORMER
  2dup set-wordlists  r/o open-file throw
  \ cr ." parse-source-file open" \ XXX INFORMER
  dup parse-file
  close-file  \ XXX FIXME "double free or corruption" Gforth error
  throw  ;
  \ Extract the glossary information from file _ca len_ and
  \ print it to standard output.

\ ==============================================================
\ Argument parser

\ Create a new argument parser
s" Glosara"  \ name
s" [ OPTION | INPUT-FILE ] ..."  \ usage
version
s" Written in Forth by Marcos Cruz (programandala.net)" \ extra
arg-new constant arguments

\ Add the default options
arguments arg-add-help-option
arguments arg-add-version-option

\ Add the verbose option
4 constant arg.verbose-option
char v  \ short option
s" verbose"  \ long option
s" activate verbose mode"  \ description
true  \ switch type
arg.verbose-option arguments arg-add-option

\ Add the output option
5 constant arg.output-option
char o  \ short option
s" output"  \ long option
s" set the output file"  \ description
false  \ switch type
arg.output-option arguments arg-add-option

: help  ( -- )
  arguments arg-print-help  ;
  \ Show the help.

: aid  ( -- )
  options @ ?exit  help  ;
  \ Show the help if no option was specified.

: verbose-option  ( -- )
  verbose on  s" Verbose mode is on" echo  ;

: input-file  ( ca len -- )
  \ cr ." input-file " 2dup type \ XXX INFORMER
  s" Processing " 2over s+ echo  parse-source-file  ;

: output-option  ( ca len -- )
  \ cr ." output-option " 2dup type  \ XXX INFORMER
  output-filename @ 0<> abort" More than one output file specified"
  output-filename 2!  ;

: version-option  ( -- )
  arguments arg-print-version  ;

: option  ( n -- )
  1 options +!
  case
    arg.help-option       of  help              endof
    arg.version-option    of  version-option    endof
    arg.output-option     of  output-option     endof
    arg.verbose-option    of  verbose-option    endof
    arg.non-option        of  input-file        endof
  endcase  ;

: option?  ( -- n f )
  arguments arg-parse  dup arg.done <> over arg.error <> and  ;
  \ Parse the next option. Is it right?

\ ==============================================================
\ Boot

: init  ( -- )
  delete-temp-files
  argc off  options off  verbose off  ;

: run  ( -- )
  init  begin  option?  while  option  repeat  drop  aid  ;

run bye

\ ==============================================================
\ History

\ 2015-09-25: Start.
\
\ 2015-11-04: Renamed from "source2adoc" to "glosser". Some
\ changes. Deferred vocabularies for parsing.
\
\ 2015-11-25: First draft.
\
\ 2015-11-26: First working version: the glossary entries are
\ extracted from the sources of Solo Forth, ordered and joined
\ into a glossary file. Added an argument parser.
\
\ 2016-04-28: Create Git repository out of the development
\ backups.  Rename to "Glosara". Change the version numbering
\ after Semantic Versioning (http://semver.org).

\ vim: textwidth=64
