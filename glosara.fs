#! /usr/bin/env gforth

\ glosara.fs

\ XXX UNDER DEVELOPMENT

: version  s" 0.2.0+201702141520" ;

\ ==============================================================
\ Description

\ Glosara is a command line tool that extracts the glossary
\ documentation out of Forth sources, and creates a glossary
\ document in Asciidoctor format.

\ Glosara is written in Forth with Gforth
\ (http://gnu.org/software/gforth).

\ ==============================================================
\ Author and license

\ Copyright (C) 2015, 2016, 2017 Marcos Cruz (programandala.net)

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

include ffl/arg.fs \ argument parser

\ From the Galope library
\ (http://programandala.net/en.program.galope.html)

require galope/unslurp-file.fs
require galope/minus-extension.fs
require galope/string-suffix-question.fs
require galope/string-slash.fs
require galope/trim.fs
require galope/slash-name.fs

\ ==============================================================
\ Config

variable verbose verbose off \ flag: verbose mode?
variable options \ counter: valid options on the command line

s" /tmp/" 2constant temp-directory
s" .glossary_entry" 2constant entry-filename-extension

\ ==============================================================
\ Misc

: echo ( ca len -- )
  verbose @ if  type cr  else  2drop  then ;
  \ Print the string _ca len_ if `verbose` is on.

\ ==============================================================
\ Files

2variable output-filename \ filename

0 [if] \ XXX TODO --

0 value entry-fid

: create-entry-file ( ca len -- )
  w/o create-file throw  to entry-fid ;

: close-entry-file ( -- )
  entry-fid close-file throw  0 to entry-fid ;

: >entry-file ( ca len -- )
  entry-fid write-file throw ;

: >entry-file-line ( ca len -- )
  entry-fid ?dup if  write-line throw  else  2drop  then ;

[then]

0 [if] \ XXX TODO --

: create-output-file ( ca len -- )
  w/o create-file throw  to output-fid ;

: close-output-file ( -- )
  output-fid close-file throw  0 to output-fid ;

: >output-file ( ca len -- )
  output-fid write-file throw ;

: >output-file-line ( ca len -- )
  output-fid ?dup if  write-line throw  else  2drop  then ;

[then]

: c>hex ( c -- ca len )
  base @ >r hex  s>d <# # # #>  r> base ! ;
  \ Convert a character to a 2-char string with its hex value.

: string>hex ( ca1 len1 -- ca2 len2 )
  s" " 2swap bounds do  i c@ c>hex s+  loop ;
  \ Return a string _ca2 len2_ which consists of the hex values
  \ of the characters of string _ca1 len1_ (one 2-digit hex
  \ number per original character).

: entry-files-pattern ( -- ca len )
  temp-directory s" *" s+ entry-filename-extension s+ ;
  \ Wildcard pattern for all temporary entry files.

: delete-temp-files ( -- )
  entry-files-pattern  s" rm -f " 2swap s+ system ;
  \ Delete all temporary entry files.

: ruler ( c len -- ca len )
  dup allocate throw swap 2dup 2>r rot fill 2r> ;
  \ Return a string of _len_ characters _c_.

31 constant max-word-length

: entryname>filename ( ca1 len1 -- ca2 len2 )
  '0' max-word-length ruler
  2swap string>hex s+ [ max-word-length 2 * ] literal string/
  entry-filename-extension s+
  temp-directory 2swap s+ ;
  \ Convert the name of a glossary entry _ca1 len1_ (a Forth
  \ word) to its temporary filename _ca2 len2_. The base
  \ filename consists of `max-word-length` 8-bit hex numbers
  \ that represent the characters of the entry name.

\ ==============================================================
\ Glossary entries

0 [if] \ XXX OLD

: parse-line ( -- ca len )  0 parse ;

: end-of-entry ( -- )
  ." }doc found! End of entry." \ XXX INFORMER
  \ cr ." End of entry -- press any key" key drop \ XXX INFORMER
  close-entry-file set-source-order ;
  \ End of the current glossary entry.

: new-entry ( ca len -- )
  entryname>filename create-entry-file
  s" == " >entry-file ;
  \ Start a new entry, creating its output file.

: get-entry-line ( "text<eol>" -- )
  \ ." valid-entry-line: " 2dup type ~~ cr key drop \ XXX INFORMER
  parse-line >entry-file-line cr ;
  \ Parse a line of the current entry and write it to its file.

: parse-entry-line ( "text<eol>" -- )
  parse-line
  2dup ." «" type ." »" \ XXX INFORMER
  \ key drop \ XXX INFORMER
  2dup end-of-entry? if   end-of-entry
                     else type then ;
  \ Parse and type a line of glossary entry.

[then]

\ ==============================================================
\ Source parser

255 constant /line-buffer

create line-buffer /line-buffer 2 + chars allot

variable processing-entry \ flag: processing a glossary entry?

: start-of-entry? ( ca len -- f )
  s" doc{" str= ;

: end-of-entry? ( ca len -- f )
  s" }doc" str= ;

: process-ordinary-line ( ca len -- )
  start-of-entry? processing-entry !  ;

: process-entry-line ( ca len -- )
  2dup end-of-entry? if   processing-entry off 2drop
                     else type cr then ;

: tidy-line  ( ca len -- ca' len' )
  /name 2nip trim ;
 
: process-line ( ca len -- )
  \ 2dup type cr \ XXX INFORMER
  tidy-line processing-entry @ if   process-entry-line
                               else process-ordinary-line then ;

: read-line? ( fid -- ca len f )
  >r line-buffer dup /line-buffer r> read-line throw ;

: parse-file ( fid -- )
  begin dup read-line? while process-line repeat 2drop ;
  \ Extract the glossary information from file _fid_ and
  \ print it to standard output.

: parse-input-file ( ca len -- )
  r/o open-file throw dup parse-file close-file throw ;
  \ Extract the glossary information from file _ca len_ and
  \ print it to standard output.

\ ==============================================================
\ Argument parser

\ Create a new argument parser
s" Glosara" \ name
s" [ OPTION | INPUT-FILE ] ..." \ usage
version
s" Written in Forth by Marcos Cruz (programandala.net)" \ extra
arg-new constant arguments

\ Add the default options
arguments arg-add-help-option
arguments arg-add-version-option

\ Add the verbose option
4 constant arg.verbose-option
char v \ short option
s" verbose" \ long option
s" activate verbose mode" \ description
true \ switch type
arg.verbose-option arguments arg-add-option

\ Add the output option
5 constant arg.output-option
char o \ short option
s" output" \ long option
s" set the output file" \ description
false \ switch type
arg.output-option arguments arg-add-option

: help ( -- )
  arguments arg-print-help ;
  \ Show the help.

: ?help ( -- )
  options @ ?exit  help ;
  \ Show the help if no option was specified.

: verbose-option ( -- )
  verbose on  s" Verbose mode is on" echo ;

: input-file ( ca len -- )
  \ cr ." input-file " 2dup type \ XXX INFORMER
  s" Processing " 2over s+ echo  parse-input-file ;

: output-option ( ca len -- )
  \ cr ." output-option " 2dup type \ XXX INFORMER
  output-filename @ 0<> abort" More than one output file specified"
  output-filename 2! ;

: version-option ( -- )
  arguments arg-print-version ;

: option ( n -- )
  1 options +!
  case
    arg.help-option       of  help              endof
    arg.version-option    of  version-option    endof
    arg.output-option     of  output-option     endof
    arg.verbose-option    of  verbose-option    endof
    arg.non-option        of  input-file        endof
  endcase ;

: option? ( -- n f )
  arguments arg-parse  dup arg.done <> over arg.error <> and ;
  \ Parse the next option. Is it right?

\ ==============================================================
\ Boot

: init-variables ( -- )
  processing-entry off ;

: init-arguments ( -- )
  argc off  options off  verbose off ;

: init ( -- )
  delete-temp-files init-arguments init-variables ;

: run ( -- )
  init  begin  option?  while  option  repeat  drop  ?help ;

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
\ 2016-04-28: Create a Git repository out of the development
\ backups.  Rename to "Glosara". Change the version numbering
\ after Semantic Versioning (http://semver.org).

\ vim: textwidth=64
