#! /usr/bin/env gforth

\ glosara.fs

\ XXX UNDER DEVELOPMENT

: version  s" 0.9.0+201702150033" ;

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
require galope/string-slash.fs
\ require galope/trim.fs
require galope/slash-name.fs
require galope/first-name.fs
\ require galope/replaced.fs

require galope/tilde-tilde.fs \ XXX TMP -- for debugging

\ ==============================================================
\ Misc

variable verbose verbose off \ flag: verbose mode?

: echo ( ca len -- )
  verbose @ if  type cr  else  2drop  then ;
  \ Print the string _ca len_ if `verbose` is on.

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
\ Files

s" /tmp/" 2constant temp-directory

s" .glosara.entry" 2constant entry-filename-extension

2variable output-filename

variable output-file \ output file identifier

: create-output-file ( -- fid )
  output-filename 2@ w/o create-file throw dup output-file ! ;

: close-output-file ( -- )
  output-file @ close-file throw ;

31 constant max-word-length
max-word-length 2 * constant /entry-filename

create entry-filename-template /entry-filename chars allot
  \ Entry filename.

: init-entry-filename-template ( -- )
  entry-filename-template /entry-filename '0' fill ;
  \ Erase the entry filename with characters '0'.

init-entry-filename-template

: entryname>filename ( ca1 len1 -- ca2 len2 )
  \ 2dup ." entryname=" type cr key drop \ XXX INFORMER
  entry-filename-template /entry-filename 2swap string>hex s+
  /entry-filename string/
  \ 2dup ." filename=" type cr key drop  \ XXX INFORMER
  entry-filename-extension s+
  temp-directory 2swap s+ ;
  \ Convert a glossary entry name _ca1 len1_ (a Forth
  \ word) to its temporary filename _ca2 len2_. The base
  \ filename consists of `max-word-length` 8-bit hex numbers
  \ that represent the characters of the entry name.

variable entry-file

: close-entry-file ( -- )
  entry-file @ close-file throw entry-file off ;
  \ Close the glossary entry file.

: create-entry-file ( ca len -- fid )
  entry-file @ if close-entry-file then
  entryname>filename w/o create-file throw dup entry-file ! ;
  \ Create a file for glossary entry name _ca len_.
  \ If a previous entry file is open, close it.

: entry-files-pattern ( -- ca len )
  temp-directory s" *" s+ entry-filename-extension s+ ;
  \ Wildcard pattern for all temporary entry files.

: delete-entry-files ( -- )
  s" rm -f " entry-files-pattern s+ system ;
  \ Delete all temporary entry files.

\ ==============================================================
\ Source parser

255 constant /line-buffer

create line-buffer /line-buffer 2 + chars allot

variable entry-line#    \ counter of non-empty lines (first line is 1)
variable entry-header   \ flag: processing an entry header?
variable header-status  \ 0=not found yet; 1=processing; 2=finished

: processing-header? ( -- f )
  header-status @ 1 = ;

: start-of-entry? ( ca len -- f )
  s" doc{" str= ;

: end-of-entry? ( ca len -- f )
  s" }doc" str= ;

: new-entry ( -- )
  1 entry-line# ! entry-header off header-status off ;

: process-code-line ( ca len -- )
  start-of-entry? if new-entry then ;
  \ Process input file line _ca len_.

: entryname>id ( ca1 len1 -- ca2 len2 )
  s" [#" 2swap string>hex s+ s" ]" s+ ;
  \ Convert word name _ca1 len1_ to an Asciidoctor attribute
  \ list _ca2 len2_ containing the corresponding id block
  \ attribute.

: heading ( ca len -- )
  2dup entryname>id type cr ." == " type cr ;

: code-block ( ca len -- )
  ." ----" cr type cr ;
  \ Output the markup to start or end a code block,
  \ followed by the given string.

: header-boundary ( ca len -- )
  code-block 1 header-status +! ;

: end-header ( ca len -- )
  header-boundary ;

: start-header ( ca len -- )
  1 entry-line# +!
  2dup first-name 2dup create-entry-file to outfile-id
                       heading cr
       header-boundary ;
  \ Start an entry header, whose first line is _ca len_.

: start-of-header?  ( -- f )
  entry-line# @ 2 = ;

: end-of-header?  ( len -- f )
  0= processing-header? and ;

: update-entry-line# ( len -- )
  0<> abs entry-line# +! ;
  \ If _len_ (the length of the current entry line)
  \ is not zero, increase the count of entry lines.

: (process-entry-line) ( ca len -- )
  dup update-entry-line#
  dup end-of-header?   if end-header   exit then
      start-of-header? if start-header exit then
  type cr ;
  \ Process input line _ca len_, which is part of the contents
  \ of a glossary entry.

: process-entry-line ( ca len -- )
  2dup end-of-entry? if   entry-line# off 2drop
                     else (process-entry-line) then ;
  \ Process input line _ca len_, which is part of a glossary
  \ entry, maybe its end markup.

: tidy  ( ca len -- ca' len' )
  /name 2nip dup 0<> abs /string ;
  \ Remove the first name (a substring delimited by spaces) from
  \ _ca len_ and the first space after it.  also the The removed
  \ name is the line comment mark of the input source.

: process-line ( ca len -- )
  tidy entry-line# @ if   process-entry-line
                     else process-code-line then ;
  \ Process the input line _ca len_.

: read-line? ( fid -- ca len f )
  >r line-buffer dup /line-buffer r> read-line throw ;

  \ XXX OLD
  \ : init-output ( -- )
  \ output-filename @ if create-output-file then ;

: begin-parsing ( -- )
  entry-line# off entry-header off header-status off ;
  \ Init the parser variables.

: end-parsing ( -- )
  stdout to outfile-id ;
  \ Set `emit` and `type` to standard output.

: parse-file ( fid -- )
  begin-parsing
  begin dup read-line? while process-line repeat 2drop
  end-parsing ;
  \ Extract the glossary information from file _fid_ and
  \ print it to standard output.

: parse-input-file ( ca len -- )
  r/o open-file throw dup parse-file close-file throw ;
  \ Extract the glossary information from file _ca len_ and
  \ print it to standard output.

\ ==============================================================
\ Glossary

  \ : output ( -- fid )
  \ output-filename @ if create-output-file else stdout then ;
  \ XXX OLD

: >file ( ca1 len1 -- ca1 len1 | ca2 len2 )
  output-filename @ if s"  > " s+ output-filename 2@ s+ then ;
  \ I an output file was specified in the command line, add its
  \ redirection to the given shell command _ca1 len1_, resulting
  \ _ca2 len2_. Otherwise do nothing.

: cat ( -- ca len )
  s" cat " entry-files-pattern s+ ;
  \ Return the shell `cat` command to concatenate and print all glossary
  \ entry files.

: glossary ( -- )
  cat >file system ;
  \ Print the final glossary to standard output or to the output
  \ file, if specified.

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

: verbose-option ( -- )
  verbose on s" Verbose mode is on" echo ;

variable input-files# \ counter

: input-file ( ca len -- )
  1 input-files# +!
  s" Processing " 2over s+ echo parse-input-file ;

: output-option ( ca len -- )
  output-filename @ abort" More than one output file specified"
  output-filename 2! ;

: version-option ( -- )
  arguments arg-print-version ;

: option ( n -- )
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

: init ( -- )
  delete-entry-files
  argc off verbose off output-filename off input-files# off ;

: options ( -- )
  begin option? while option repeat drop ;

: files ( -- n )
  init options input-files# @ ;

: run ( -- )
  files if glossary else help then ;

run bye

\ vim: filetype=gforth textwidth=64
