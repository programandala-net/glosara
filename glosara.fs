#! /usr/bin/env gforth

\ glosara.fs

\ XXX UNDER DEVELOPMENT

: version  s" 0.7.0+201702142227" ;

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
\ Requirements

forth-wordlist set-current

\ From Forth Foundation Library
\ (http://irdvo.github.io/ffl/)

include ffl/arg.fs \ argument parser

\ From the Galope library
\ (http://programandala.net/en.program.galope.html)

\ require galope/unslurp-file.fs
\ require galope/minus-extension.fs
\ require galope/string-slash.fs
\ require galope/trim.fs
require galope/slash-name.fs
require galope/first-name.fs
\ require galope/replaced.fs

require galope/tilde-tilde.fs \ XXX TMP -- for debugging

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

2variable output-filename
variable output-file \ output file identifier

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

: create-output-file ( -- )
  output-filename 2@ w/o create-file throw output-file ! ;

: close-output-file ( -- )
  output-file @ close-file throw ;

: output  ( -- fid )
  output-file @ ?dup 0= if stdout then ;

0 [if] \ XXX TODO --

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
[then]

\ ==============================================================
\ Strings

: c>hex ( c -- ca len )
  base @ >r hex  s>d <# # # #>  r> base ! ;
  \ Convert a character _c_ to a 2-character string _ca len_
  \ with its hex value.

: string>hex ( ca1 len1 -- ca2 len2 )
  s" " 2swap bounds do  i c@ c>hex s+  loop ;
  \ Return a string _ca2 len2_ which consists of the hex values
  \ of the characters of string _ca1 len1_ (one 2-digit hex
  \ number per original character).

\ ==============================================================
\ Source parser

255 constant /line-buffer

create line-buffer /line-buffer 2 + chars allot

variable entry          \ counter of non-empty lines (first line is 1)
variable entry-header   \ flag: processing an entry header?
variable header-status

: processing-header? ( -- f )
  header-status @ 1 = ;

: header-done? ( -- f )
  header-status @ 2 = ;

: start-of-entry? ( ca len -- f )
  s" doc{" str= ;

: end-of-entry? ( ca len -- f )
  s" }doc" str= ;

: new-entry ( -- )
  1 entry ! entry-header off header-status off ;

: process-code-line ( ca len -- )
  start-of-entry? if new-entry then ;
  \ Process input file line _ca len_.

: name>id ( ca1 len1 -- ca2 len2 )
  s" [#" 2swap string>hex s+ s" ]" s+ ;
  \ Convert word name _ca1 len1_ to an Asciidoctor attribute
  \ list _ca2 len2_ containing the corresponding id block
  \ attribute.

: heading ( ca len -- )
  2dup name>id type cr ." == " type cr ;

: code-block ( ca len -- )
  ." ----" cr type cr ;
  \ Output the markup to start or end a code block,
  \ followed by the given string.

: header-boundary ( ca len -- )
  code-block 1 header-status +! ;

: end-header ( ca len -- )
  header-boundary ;

: start-header ( ca len -- )
  1 entry +!
  2dup first-name heading cr header-boundary ;
  \ Start an entry header, whose first line is _ca len_.

: start-of-header?  ( -- f )
  entry @ 2 = ;

: end-of-header?  ( len -- f )
  0= processing-header? and ;

: update-line-count ( len -- )
  0<> abs entry +! ;

: (process-entry-line) ( ca len -- )
  dup update-line-count
  dup end-of-header?   if end-header   exit then
      start-of-header? if start-header exit then
  type cr ;
  \ Process input line _ca len_, which is part of the contents
  \ of a glossary entry.

: process-entry-line ( ca len -- )
  2dup end-of-entry? if   entry off 2drop
                     else (process-entry-line) then ;
  \ Process input line _ca len_, which is part of a glossary
  \ entry, maybe its end markup.

: tidy  ( ca len -- ca' len' )
  /name 2nip dup 0<> abs /string ;
  \ Remove the first name (a substring delimited by spaces) from
  \ _ca len_ and the first space after it.  also the The removed
  \ name is the line comment mark of the input source.

: (process-line) ( ca len -- )
  entry @ if   process-entry-line
          else process-code-line then ;
  \ Process the input line _ca len_.

: process-line ( ca len -- )
  tidy ['] (process-line) output outfile-execute ;
  \ Process the input line _ca len_, redirecting the
  \ output to the output file specified in the command line, if
  \ any.

: read-line? ( fid -- ca len f )
  >r line-buffer dup /line-buffer r> read-line throw ;

: init-output ( -- )
  output-filename @ if create-output-file then ;

: init-parser-flags ( -- )
  entry off entry-header off header-status off ;

: init-parser ( -- )
  init-parser-flags init-output ;

: parse-file ( fid -- )
  init-parser
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
s" Written by Marcos Cruz (programandala.net)" \ extra
arg-new constant arguments

\ Add the default options
arguments arg-add-help-option
arguments arg-add-version-option

\ Add the verbose option
4 constant arg.verbose-option
'v'                       \ short option
s" verbose"               \ long option
s" activate verbose mode" \ description
true                      \ switch type
arg.verbose-option arguments arg-add-option

\ Add the output option
5 constant arg.output-option
'o'                     \ short option
s" output"              \ long option
s" set the output file" \ description
false                   \ switch type
arg.output-option arguments arg-add-option

: help ( -- )
  arguments arg-print-help ;
  \ Show the help.

: ?help ( -- )
  options @ ?exit help ;
  \ Show the help if no option was specified.

: verbose-option ( -- )
  verbose on s" Verbose mode is on" echo ;

: input-file ( ca len -- )
  s" Processing " 2over s+ echo parse-input-file ;

: output-option ( ca len -- )
  output-filename @ abort" More than one output file specified"
  output-filename 2! ;

: version-option ( -- )
  arguments arg-print-version ;

: option ( n -- )
  1 options +!
  case
    arg.help-option    of help           endof
    arg.version-option of version-option endof
    arg.output-option  of output-option  endof
    arg.verbose-option of verbose-option endof
    arg.non-option     of input-file     endof
  endcase ;

: option? ( -- n f )
  arguments arg-parse  dup arg.done <> over arg.error <> and ;
  \ Parse the next option. Is it right?

\ ==============================================================
\ Boot

: init-arguments ( -- )
  output-filename off argc off options off verbose off ;

: init ( -- )
  init-arguments ;

: run ( -- )
  init begin option? while option repeat drop ?help ;

run bye

\ vim: filetype=gforth textwidth=64
