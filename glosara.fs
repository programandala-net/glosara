#! /usr/bin/env gforth
\ glosara.fs

\ ==============================================================
\ Glosara {{{1

: version s" 0.31.0-dev.7.0+202004130107" ;

\ ==============================================================
\ Description

\ Glosara is a command line tool that extracts the glossary
\ documentation out of Forth sources, and creates a glossary
\ document in Asciidoctor format.

\ Glosara is written in Forth (http://forth-standard.org) with
\ Gforth (http://gnu.org/software/gforth).

\ ==============================================================
\ Author and license

\ Copyright (C) 2015, 2016, 2017, 2018, 2020 Marcos Cruz
\ (programandala.net)

\ You may do whatever you want with this work, so long as you
\ retain all copyright, authorship and credit notices and this
\ license in all redistributed copies and derived works. There
\ is no warranty.

\ ==============================================================
\ Requirements

forth-wordlist set-current

warnings off

\ From Forth Foundation Library
\ (http://irdvo.github.io/ffl/)

require ffl/arg.fs \ argument parser
require ffl/rgx.fs \ regular expressions

\ From the Galope library
\ (http://programandala.net/en.program.galope.html)

require galope/c-to-str.fs        \ `c>str`
require galope/first-name.fs      \ `first-name`
require galope/minus-leading.fs   \ `-leading`
require galope/replaced.fs        \ `replaced`
require galope/s-s-plus.fs        \ `ss+`
require galope/slash-name.fs      \ `/name`
require galope/stringer.fs        \ `stringer`
require galope/trim.fs            \ `trim`
require galope/unslurp-file.fs    \ `unslurp-file`

\ ==============================================================
\ Dependencies

\ This program depends on the fact that Gforth allocates all
\ strings in the heap. This may be changed in a future version,
\ in order to make the code more portable.

\ ==============================================================
\ Debug

variable debugging

debugging off

: debug? ( -- f ) debugging @ ;

: (checkstack) ( ca len -- )
  cr type cr ."   " .s ;

: checkstack ( ca len -- )
  debug? if (checkstack) else 2drop then ;

: (checkstr) ( ca1 len1 ca2 len2 -- )
  (checkstack) cr ."   " 2dup type ;

: checkstr ( ca1 len1 ca2 len2 -- )
  debug? if (checkstr) else 2drop then ;

\ ==============================================================
\ Misc

variable verbose
  \ Flag: verbose mode?

variable unique
  \ Flag: unique mode?

variable annex
  \ Flag: annex mode? In annex mode, the input file is not a
  \ glossary but an annex, whose implicit links are converted to
  \ actual links into the glossary.

variable sections
  \ Flag: add section headings?

: echo ( ca len -- )
  verbose @ if type cr else 2drop then ;
  \ Display the string _ca len_ if `verbose` is on.

\ ==============================================================
\ Strings

\ 1024 64 * allocate-stringer
here 1024 64 * dup allot set-stringer

synonym s+ ss+
  \ Overwrite the ordinary `s+`, which returns the contatenated
  \ string in the heap, with `ss+`, which uses the `stringer`
  \ instead.

: c>hex ( c -- ca len )
  base @ >r hex s>d <# # # #> r> base ! ;
  \ Convert a character _c_ to a 2-character string _ca len_
  \ with its hex value.

: string>hex ( ca1 len1 -- ca2 len2 )
  s" " 2swap bounds do i c@ c>hex s+ loop ;
  \ Return a string _ca2 len2_ which consists of the hex values
  \ of the characters of string _ca1 len1_ (one 2-digit hex
  \ number per original character).

\ ==============================================================
\ Entry counters word list

\ In order to manage duplicated entries, homonymous variables
\ are created in a word list. Their current value is used as the
\ suffix of their corresponding files.

wordlist constant counters-wordlist

: search-counters ( ca len -- false | xt true )
  counters-wordlist search-wordlist 0<> ;
  \ If entry name _ca len_ has an associated counter, return its
  \ _xt_ and _true_. Else return _false_.

: known-counter ( xt -- n )
  execute dup @ 1 rot +! ;
  \ Return current value of entry counter _xt_, and increase its
  \ value for the next time.

: new-counter ( ca len -- 0 )
  get-current >r counters-wordlist set-current nextname variable
  r> set-current 1 latestxt execute ! 0 ;
  \ Create a new entry counter for entry name _ca len_, and
  \ return zero, its initial value.

: entry-counter ( ca len -- n )
  2dup search-counters if   nip nip known-counter
                       else new-counter then ;
  \ Create a new entry counter for entry name _ca len_, if not
  \ done before, and return its current value _n_.

: entry-counter@ ( ca len -- n )
  search-counters 0= abort" Unknown entry counter"
  known-counter ;
  \ Return the current value _n_ of the entry counter of entry
  \ name _ca len_.

\ ==============================================================
\ Files {{{1

256 chars constant /path
  \ Max length of a path, directory or filename.

: default-temp-dir ( -- ca len ) s" /tmp/glosara/" ;

create temp-dir> /path allot
  \ Storage for the temporary directory.

: !dir ( ca len a -- ) >r s" /" s+ r> place ;
  \ Store directory _ca len_ into _a_. A trailing slash is added
  \ first, just in case.

: temp-dir! ( ca len -- ) temp-dir> !dir ;
  \ Store _ca len_ into the temporary directory.

default-temp-dir temp-dir!
  \ Use the default temporary directory.

: temp-dir ( -- ca len ) temp-dir> count ;

-529 constant file-exists-error#
 493 constant dir-permissions \ 0o755 (= #493)

: make-temp-dir ( -- )
  temp-dir dir-permissions mkdir-parents
  dup file-exists-error# = if drop else throw then ;
  \ Make the temporary directory with its parents and
  \ permissions 0o755 (= #493).

make-temp-dir
  \ XXX REMARK -- Create the default temporary directory, even
  \ if an specific one could be specified in the command line.
  \ The reason is the main program is executed when some options
  \ are found in the command line. Therefore the default
  \ directory must be ready just in case.

: filename-prefix ( -- ca len ) s" glosara.entry." ;

: /filename-prefix ( -- len ) filename-prefix nip ;

2variable output-filename

2variable processed-file

variable output-file \ output file identifier

: set-output ( -- )
  to outfile-id ;

: set-standard-output ( -- )
  stdout set-output ;

: create-output-file ( ca len -- fid )
  w/o create-file throw dup output-file ! ;

: update-output ( -- )
  output-filename 2@ dup if   create-output-file set-output
                         else 2drop
                         then ;

: restore-output ( -- )
  output-file @ ?dup if   close-file throw
                     then set-standard-output ;

                       31 constant max-word-length
max-word-length 2 * chars constant /basefilename

create null-filename /basefilename allot
       null-filename /basefilename '0' fill
  \ A string full of '0' characters, used to pad the base filename
  \ of each glossary entry file.­

: entryname>count-suffix ( ca1 len2 -- ca2 len2 )
  entry-counter unique @ 0= and c>hex s" -" 2swap s+ ;
  \ Convert entry name _ca1 len2_ to its filename counter suffix
  \ _ca2 len2_.

: slashes>hyphens ( ca len -- ca' len )
  s" -" s" /" replaced ;
  \ Replace slashes in _ca len_ with hyphens.

: dots>hyphens ( ca len -- ca' len )
  s" -" s" ." replaced ;
  \ Replace dots in _ca len_ with hyphens.

: filename>xreflabel-suffix ( ca len -- ca' len' )
  s" -" 2swap slashes>hyphens dots>hyphens s+ ;
  \ Convert filename _ca len_ to string _ca' len'_ suitable as
  \ part of a xreflabel (HTML id attribute). HTML 5 supports
  \ any character in an ID, but it's safer to convert characters
  \ not supported by previous standards.
  \
  \ XXX TODO -- Complete the conversion of special characters.

: xreflabel-file-suffix ( -- ca len )
  processed-file 2@ filename>xreflabel-suffix ;

: entryname>suffix ( ca1 len2 -- ca2 len2 )
  entryname>count-suffix s" -" s+ xreflabel-file-suffix s+ ;
  \ Convert entry name _ca1 len2_ to its filename suffix _ca2
  \ len2_.

: extension+ ( ca1 len1 -- ca2 len2 )
  s" .adoc" s+ ;
  \ Add the Asciidoctor filename extension to filename _ca1
  \ len1_, resulting _ca2 len2_.

: entryname>basefilename ( ca1 len1 -- ca2 len2 )
  2dup string>hex
  dup >r null-filename /basefilename r> -
  dup 0< abort" Entry name too long" s+
  2swap entryname>suffix s+ extension+ ;
  \ Convert a glossary entry name _ca1 len1_ (a Forth word) to
  \ its temporary filename _ca2 len2_. The filename consists of
  \ `max-word-length` 8-bit hex numbers that represent the
  \ characters of the entry name, with trailing '0' digits to
  \ its maximum length.

: complete-filename ( ca1 len1 -- ca2 len2 )
  filename-prefix 2swap s+
  temp-dir 2swap s+ ;
  \ Complete filename _ca1 len1_ with common prefix and path.

: entryname>filename ( ca1 len1 -- ca2 len2 )
  entryname>basefilename complete-filename ;
  \ Convert a glossary entry name _ca1 len1_ (a Forth word) to
  \ its temporary filename _ca2 len2_, including the path.

: section>basefilename ( c -- ca2 len2 )
  string>hex
  dup >r null-filename /basefilename r> -
  dup 0< abort" Section name too long" s+
  s" -----SECTION-HEADING" s+
  extension+ ;
  \ Convert a glossary section title _ca1 len1_ (a character) to
  \ its temporary filename _ca2 len2_. The filename consists of
  \ `max-word-length` 8-bit hex numbers that represent the
  \ characters of the entry name, with trailing '0' digits to
  \ its maximum length.

: section>filename ( ca1 len1 -- ca2 len2 )
  section>basefilename complete-filename ;
  \ Convert a glossary section title _ca1 len1_ (a character) to
  \ its temporary filename _ca2 len2_, including the path.

variable entry-file

: (close-entry-file) ( fid -- )
  close-file throw entry-file off ;
  \ Close the glossary entry file _fid_.

: close-entry-file ( -- )
  entry-file @ ?dup if (close-entry-file) then ;
  \ Close the glossary entry file, if needed.

: file-exists? ( ca len -- )
  file-status nip 0= ;

: (create-entry-file) ( ca len -- fid )
  close-entry-file w/o create-file throw dup entry-file ! ;

: create-entry-file ( ca len -- fid )
  entryname>filename (create-entry-file) ;
  \ Create a file for glossary entry name _ca len_.
  \ If a previous entry file is open, close it.

: temp-files-pattern ( -- ca len )
  temp-dir filename-prefix s+ s" *" s+ ;
  \ Wildcard pattern for all temporary entry files.

: delete-temp-files ( -- )
  s" rm -f " temp-files-pattern s+ system ;
  \ Delete all temporary entry files.

\ ==============================================================
\ Links (cross references) {{{1

\ Glosara recognizes any string without spaces and between
\ backticks (the Asciidoctor markup for monospace) as an
\ implicit link, i.e. a cross reference to a Forth word in the
\ glossary.
\
\ In order to include in the documentation also Forth words that
\ are not part of the system, and therefore must not be
\ converted to links, the unconstrained notation of Asciidoctor
\ can be used instead, i.e. double backticks.
\
\ Example extracted from the documentation of Solo Forth:

\ ____
\
\   This word is not Forth-83's ``?branch``, [...] Solo Forth
\   borrows `0branch` from fig-Forth [...]
\ ____

rgx-create implicit-link-rgx
  s" (.*)`(\S+)`(.*)" implicit-link-rgx
rgx-compile 0= [if]
  cr .( Compilation of regular expression)
  cr .( for implicit link failed on position ) . cr
  quit
[then]

rgx-create explicit-link-rgx
s" (.*)`<<(\S+)?\s*,\s*(\S+)>>`(.*)" explicit-link-rgx
  \ XXX TODO -- Check.
rgx-compile 0= [if]
  cr .( Compilation of regular expression)
  cr .( for explicit link failed on position ) . cr
  quit
[then]

rgx-create constrained-code-rgx
s" (.*)``(.+)``(.*)" constrained-code-rgx
rgx-compile 0= [if]
  cr .( Compilation of regular expression)
  cr .( for constrained code failed on position ) . cr
  quit
[then]

: entryname>common-id ( ca1 len1 -- ca2 len2 )
  string>hex s" entry" 2swap s+ ;
  \ Create a cross reference label _ca2 len2_ from entry name
  \ _ca1 len1_.

: ?filename ( ca1 len1 -- ca1 len1 | ca2 len2 )
  ?dup 0= if drop processed-file 2@ then ;
  \ If filename _ca1 len1_ is empty, return the currently parsed
  \ file _ca2 len2_ instead.
  \
  \ This makes markup lighter: Explicit links with an empty
  \ xreflabel point to a word of the current file.
  \
  \ Example: `<<,i>>`.

: >unique-id ( ca1 len1 ca2 len2 -- ca3 len3 )
  ?filename filename>xreflabel-suffix
  2swap entryname>common-id 2swap s+ ;
  \ Create a unique cross reference label _ca3 len3_ from entry
  \ name _ca1 len1_ and filename_ca2 len2_.

: entryname>unique-id ( ca1 len1 -- ca2 len2 )
  processed-file 2@ >unique-id ;
  \ Create a unique cross reference label _ca2 len2_ from entry
  \ name _ca1 len1_ and the currently parsed file.

2variable before-match
2variable xreflabel
2variable link-text
2variable constrained-code
2variable after-match

: (.matchparts) ( -- )
  cr ." before-match     «" before-match     2@ type ." »"
  cr ." link-text        «" link-text        2@ type ." »"
  cr ." constrained-code «" constrained-code 2@ type ." »"
  cr ." after-match      «" after-match      2@ type ." »" cr ;
  \ XXX TMP -- For debugging.

: .matchparts ( -- )
  debug? if (.matchparts) then ;
  \ XXX INFORMER
  \ XXX TMP -- For debugging.

: `<< ( -- ca len ) s" BACKTICKANDOPENCROSSREFERENCE" ;

: >>` ( -- ca len ) s" CLOSECROSSREFERENCEANDBACKTICK" ;

: escaped ( ca len -- ca' len' )
  s" &#35;"       s" #"   replaced
  s" &#34;"       s\" \q" replaced
  s" &#42;"       s" *"   replaced
  s" &#45;"       s" -"   replaced
  s" &#60;"       s" <"   replaced
  s" &#61;"       s" ="   replaced
  s" &#62;"       s" >"   replaced
  s" {backslash}" s" \"   replaced ;
  \ Escape special characters in string _ca len_, returning
  \ the result string _ca' len'_. Escaping certain characters is
  \ needed in order to prevent troubles during the conversion of
  \ Asciidoctor into HTML and PDF.

: match>substring ( ca1 len1 +n2 +n1 -- ca2 len2 )
  2dup - >r nip nip + r> >stringer ;
  \ Extract from string _ca1 len1_ the link text of the explicit
  \ link that starts at position _+n1_ and ends before position
  \ _+n2_. Return the result _ca2 len2_ in the `stringer`.

: prepare-implicit-link ( ca len -- )
  2dup 1 implicit-link-rgx rgx-result ( ca len +n2 +n1)
       match>substring
       before-match 2!
  2dup 2 implicit-link-rgx rgx-result ( ca len +n2 +n1)
       match>substring
       link-text 2!
       3 implicit-link-rgx rgx-result ( ca len +n2 +n1)
       match>substring
       after-match 2!  ;
  \ Prepare the first implicit link found in string _ca len_
  \ by extracting its pieces into variables.

: build-link ( ca1 len1 -- ca2 len2 )
  before-match 2@ `<< s+ 2swap s+ s" , " s+ link-text 2@ escaped s+
                  >>` s+ after-match 2@ s+ ;
  \ Build a link from its previously extracted pieces and
  \ the destination label _ca1 len1_.

: build-implicit-link ( -- ca len )
  link-text 2@ entryname>common-id build-link ;
  \ Build an implicit link from its previously extracted pieces.

: actual-implicit-link? ( -- f )
  .matchparts
  before-match 2@ dup if 1- + c@ '`' = if false exit then else 2drop then
  after-match  2@     if      c@ '`' = if false exit then else  drop then
  true ;

: convert-implicit-link ( ca1 len1 -- ca2 len2 )
  2dup prepare-implicit-link
  actual-implicit-link? if 2drop build-implicit-link then ;
  \ Convert the first implicit link (a word between backticks,
  \ e.g. `entryname`) contained in entry line _ca1 len1_ to
  \ Asciidoctor markup (e.g. <<ID, entryname>>), returning the
  \ modified string _ca2 len2_.
  \
  \ In order to prevent recursion in `implicit-links`,
  \ backticks in the result string are replaced with a temporary
  \ string. They are restored later.
  \
  \ In the result string, the space after the comma is
  \ important: It's added in order to prevent later mismatches
  \ and recursion. It has no effect to Asciidoctor.
  \
  \ XXX TODO -- Factor/combine with `explicit-link`.

: finish-links ( ca1 len1 -- ca2 len2 )
  s" `<<" `<< replaced
  s" >>`" >>` replaced ;
  \ Replace the temporary markup in _ca1 len1_.

: implicit-link? ( ca len -- f )
  implicit-link-rgx rgx-csearch -1 > ;
  \ Does string _ca len_ contain an implicit link, i.e.
  \ a word sourrounded by backticks?

: convert-implicit-links ( ca1 len1 -- ca2 len2 )
  begin 2dup implicit-link? while convert-implicit-link repeat ;
  \ If the entry line _ca1 len1_ contains implicit links, i.e.
  \ words sourrounded by backticks, convert them to Asciidoctor
  \ markup and return the modified string _ca2 len2_; else do
  \ nothing.

: prepare-constrained-code ( ca len -- )
  2dup 1 constrained-code-rgx rgx-result ( ca len +n2 +n1)
       match>substring
       before-match 2!
  2dup 2 constrained-code-rgx rgx-result ( ca len +n2 +n1)
       match>substring
       constrained-code 2!
       3 constrained-code-rgx rgx-result ( ca len +n2 +n1)
       match>substring
       after-match 2! ;
  \ Prepare the first implicit link found in string _ca len_
  \ by extracting its pieces into variables.

: `` ( -- ca len ) s" DOUBLEBACKTICKSWEREHERE" ;

: preserve-double-backticks ( ca len -- ca' len' )
  `` s" ``" replaced ;
  \ Preserve all double backticks in _ca len_, replacing them
  \ with a temporary markup.

: restore-double-backticks ( ca len -- ca' len' )
  s" ``" `` replaced ;
  \ Restore all double backticks that were in _ca len_ replacing the
  \ temporary markup with actual double backticks.

: build-constrained-code ( -- ca len )
  before-match 2@ `` s+ constrained-code 2@ escaped s+ `` s+
  after-match  2@ s+ ;
  \ Build the constrained code from its pieces.

: (constrained-code ( ca1 len1 -- ca2 len2 )
  prepare-constrained-code build-constrained-code ;
  \ Manage the first constrained code (a text between double
  \ backticks, e.g. ``dup + drop``) contained in entry line _ca1
  \ len1_.

: constrained-code? ( ca len -- f )
  constrained-code-rgx rgx-csearch -1 > ;
  \ Does string _ca len_ contain constrained code?

: convert-constrained-code ( ca len -- ca' len' )
  begin  2dup constrained-code?
  while  (constrained-code
  repeat restore-double-backticks ;
  \ If the entry line _ca len_ contains constrained code, i.e.
  \ code sourrounded by double backticks, escape the especial
  \ characters of the code.

: prepare-explicit-link ( ca len -- )
  2dup 1 explicit-link-rgx rgx-result ( ca len +n2 +n1)
         match>substring before-match 2!
  2dup 2 explicit-link-rgx rgx-result ( ca len +n2 +n1)
         match>substring xreflabel 2!
  2dup 3 explicit-link-rgx rgx-result ( ca len +n2 +n1)
         match>substring link-text 2!
       4 explicit-link-rgx rgx-result ( ca len +n2 +n1)
         match>substring after-match  2! ;
  \ Prepare the first explicit link found in string _ca len_ by
  \ extracting its pieces into variables.

: build-explicit-link ( -- ca len )
  link-text 2@ xreflabel 2@ >unique-id build-link ;
  \ Build an explicit link from its previously extracted pieces.

: convert-explicit-link ( ca1 len1 -- ca2 len2 )
  prepare-explicit-link build-explicit-link ;

: explicit-link? ( ca len -- f )
  explicit-link-rgx rgx-csearch -1 > ;
  \ Does string _ca len_ contain an explicit link?

: convert-explicit-links ( ca len -- ca' len' )
  begin 2dup explicit-link? while convert-explicit-link repeat ;
  \ If the entry line _ca len_ contains explicit and unfinished
  \ Asciidoctor links, finish them and return the modified
  \ string _ca' len'_; otherwise do nothing, and _ca' len'_ is
  \ identical to _ca len_.

: convert-links ( ca len -- ca' len' )
  preserve-double-backticks convert-explicit-links
                            convert-implicit-links finish-links
  restore-double-backticks ;
  \ If the entry line _ca len_ contains links,
  \ convert them to Asciidoctor markup and return the modified
  \ string _ca' len'_; else do nothing and _ca' len'_ is
  \ identical to _ca len_.
  \
  \ Note: The order matters. Explicit links must be treated
  \ first. Otherwise, a explicit link without a space at any
  \ side of the comma will be taken for a implicit link.
  \ Example:
  \
  \   `<<file,entryname>>`
  \
  \ XXX TODO -- Double backticks should be ignored by the
  \ regular expression. But it seems it can not be done with the
  \ current version of Forth Foundation Library. This
  \ alternative temporary method, preserving and restoring them
  \ with text substitutions, is rudimentary, but it works,
  \ provided no Forth word mentioned in the documentation has a
  \ double backtick in its name...

\ ==============================================================
\ Source parser {{{1

255 constant /line-buffer

create line-buffer /line-buffer 2 + chars allot

variable entry-line#
  \ Counter of non-empty lines (first line is 1).

variable entry-header
  \ Flag: processing an entry header?

variable header-status
  \ 0=not found yet; 1=processing; 2=finished.

: processing-header? ( -- f )
  header-status @ 1 = ;

16 chars constant /marker
  \ Maximum length of the starting and ending markers, in chars.

: ?marker ( len -- )
  /marker > abort" Marker too long" ;

create starting-marker /marker allot
  \ Starting marker storage.

create ending-marker /marker allot
  \ Ending marker storage.

s" doc{" starting-marker place
  \ Default starting marker.

s" }doc" ending-marker place
  \ Default ending marker.

: start-of-entry? ( ca len -- f )
  starting-marker count str= ;
  \ Is line _ca len_ the start of a glossary entry?

: end-of-entry? ( ca len -- f )
  ending-marker count str= ;
  \ Is line _ca len_ the end of a glossary entry?

: start-entry ( -- )
  1 entry-line# ! entry-header off header-status off ;
  \ Start a glossary entry.

: ?start-entry ( ca len -- )
  start-of-entry? if start-entry then ;
  \ If line _ca len_ is the start of a glossary entry, start it.

: entryname>common-anchor ( ca1 len1 -- ca2 len2 )
  s" [[" 2swap entryname>common-id s+ s" ]]" s+ ;
  \ Convert word name _ca1 len1_ to an Asciidoctor inline anchor
  \ _ca2 len2_, which is common to all homonymous entries.

variable entry-heading-level

1 constant min-heading-level
6 constant max-heading-level

create (heading-markup) max-heading-level chars allot
(heading-markup) max-heading-level '=' fill

: entry-heading-markup ( -- ca len )
  (heading-markup) entry-heading-level @ ;

: section-heading-markup ( -- ca len )
  entry-heading-markup 1- ;

: .entry-heading-markup ( -- ) entry-heading-markup type space ;

: common-anchor ( ca len -- )
  2dup entry-counter@ 1 = if   entryname>common-anchor type
                          else 2drop then ;
  \ If entry name _ca len_ is the first one with its name,
  \ create a common anchor for it.

: entry-heading-attr-list ( ca len -- )
  cr ." [#" entryname>unique-id type ." ]" cr ;
  \ Create the Asciidoctor attribute list for entry name _ca1
  \ len1_. The attribute list contains only the corresponding
  \ id block attribute, which is unique for each entry.

: entry-heading-line ( ca len )
  .entry-heading-markup 2dup common-anchor escaped type cr ;
  \ Create a glossary heading line for entry name _ca len_.
  \ The Asciidoctor inline macro `pass:[]` is used to force
  \ replacement of special characters of Forth names (e.g. '<')
  \ to HTML entities.

: heading ( ca len -- )
  2dup entry-heading-attr-list
       entry-heading-line ;
  \ Create a glossary heading for entry name _ca len_.
  \
  \ Note: When the common anchor is created on its own line,
  \ before the attribute list of the heading, Asciidoctor
  \ ignores it during the conversion to HTML. That's why it's
  \ created as part of the heading line.

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
  2dup first-name
  2dup create-entry-file
  set-output heading cr header-boundary ;
  \ Start an entry header, whose first line is _ca len_.

: start-of-header? ( -- f )
  entry-line# @ 2 = ;

: end-of-header? ( len -- f )
  0= processing-header? and ;

: update-entry-line# ( len -- )
  0<> abs entry-line# +! ;
  \ If _len_ (the length of the current entry line)
  \ is not zero, increase the count of entry lines.

: (process-entry-line) ( ca len -- )
  dup update-entry-line#
  dup end-of-header?   if end-header   exit then
      start-of-header? if start-header exit then
  convert-constrained-code
  convert-links type cr ;
  \ Process line _ca len_, which is part of the contents
  \ of a glossary entry.

: add-source-file ( -- )
  cr ." Source file: <" processed-file 2@ type ." >." cr ;

: end-entry ( -- )
  add-source-file entry-line# off ;

: process-entry-line ( ca len -- )
  2dup end-of-entry? if   2drop end-entry
                     else (process-entry-line) then ;
  \ Process line _ca len_, which is part of a glossary
  \ entry, maybe its end markup.

: tidy ( ca len -- ca' len' )
  /name 2nip dup 0<> abs /string ;
  \ Remove the first name (a substring delimited by spaces) from
  \ _ca len_ and the first space after it. The removed name is
  \ supposed to be the Forth line comment backslash, or the
  \ corresponding markup of other languages.

: entry? ( -- f )
  entry-line# @ 0<> ;
  \ Is there an entry being processed?

: process-line ( ca len -- )
  tidy entry? if process-entry-line else ?start-entry then ;
  \ Process line _ca len_.

: read-line? ( fid -- ca len f )
  >r line-buffer dup /line-buffer r> read-line throw ;

: process-glossary-file ( fid -- )
  entry-line# off entry-header off header-status off
  begin dup read-line? while process-line repeat 2drop
  close-entry-file set-standard-output ;
  \ Process the glossary file _fid_, extracting the glossary
  \ entries from it and creating the corresponding entry files.

: process-annex-file ( fid -- )
  update-output
  begin dup read-line? while convert-links type cr repeat 2drop
  restore-output ;
  \ Process the annex file _fid_, converting the links found in
  \ it and sending the result to the current output.

defer (process-file) ( fid -- )

' process-glossary-file ' (process-file) defer!

: process-file ( ca len -- )
  2dup processed-file 2!
  r/o open-file throw dup (process-file) close-file throw ;
  \ Process the glossary or annex file _ca len_.

\ ==============================================================
\ Glossary {{{1

: (>file) ( ca1 len1 -- ca2 len2 )
  s"  > " s+ output-filename 2@ s+ ;
  \ Complete shell command _ca1 len1_ with the redirection to
  \ the output file specified in the command line, resulting
  \ _ca2 len2_.

: >file ( ca1 len1 -- ca1 len1 | ca2 len2 )
  output-filename @ if (>file) then ;
  \ If an output file was specified in the command line, add its
  \ redirection to the given shell command _ca1 len1_, resulting
  \ _ca2 len2_. Otherwise do nothing.

: cat ( -- ca len )
  s" cat " temp-files-pattern s+ ;
  \ Return the shell `cat` command that concatenates all the
  \ glossary entry files and sends them to standard output.

variable previous-initial
  \ The initial character of the previous entry.

: new-initial? ( c -- f )
  previous-initial @ over previous-initial ! <> ;
  \ Is character _c_ the initial of the previous entry?

: section-heading ( c -- ca len )
  >r s\" \n\n" section-heading-markup s+
     s"  " s+
  r> c>str s+
     s\" \n\n" s+ ;

: section-file ( c -- ca len )
  c>str section>filename ;
  \ Convert initial _c_ to section filename _ca len_.

: new-section ( c -- )
  dup >r section-heading
      r> section-file unslurp-file ;
  \ Create a glossary section file for words with an initial
  \ _c_. The file contains the heading markup. The filename is a
  \ variant of the glossary entry files, to make sure it's
  \ sorted in the right place.

: filename>initial ( ca len -- c )
  /filename-prefix /string drop 2
  base @ >r hex evaluate r> base ! ;
  \ Return the initial character _c_ of the glossary entry whose
  \ file is _ca len_.

: entry-file? ( ca len -- f )
  filename-prefix string-prefix? ;
  \ Is _ca len_ an entry file?

: (?new-section) ( ca len -- )
  filename>initial dup new-initial? if new-section else drop then ;
  \ If a new section is needed before the entry glossary whose file is
  \ _ca len_, create it. Else do nothing.

: ?new-section ( ca len -- )
  2dup entry-file? if (?new-section) else 2drop then ;
  \ If _ca len_ is an entry filename, and a new section is
  \ needed before its corresponding glossary entry, create it.
  \ Else do nothing.

create file> /path allot
  \ Storage for each filename read from the directory.

: create-glossary-sections ( -- )
  temp-dir open-dir throw {: dir-id :}
  begin  file> /path dir-id read-dir throw
  while  file> swap ?new-section
  repeat drop dir-id close-dir throw ;
  \ Create the glossary sections by reading each glossary
  \ entry file from the temporary directory, comparing its
  \ initial (coded in hex after the filename prefix) with
  \ the previous one and, if so, creating a new file with
  \ the section heading markup and a proper name.

: join-glossary-files ( -- )
  cat >file system ;
  \ Send all the glossary files to standard output
  \ (or to the output file, if specified).

: create-glossary ( -- )
  sections @ if create-glossary-sections then
  join-glossary-files ;
  \ Create the glossary sections, if needed.
  \ Then send all the glossary files to standard output
  \ (or to the output file, if specified).

: finnish ( -- )
  annex @ 0= if create-glossary then ;
  \ Finnish the task, by creating the glossary file, if needed.

\ ==============================================================
\ Argument parser {{{1

\ Create a new argument parser:
s" glosara" \ command
s\"  { [option...] { <input-file> | <-i file> } [option...] ...}\n\n"
s\" Note: both `input-file` and the `-i` option start the program\n" s+
s\" immediately with the currently parsed options." s+
  \ usage
version
s" (C) 2015-2020 Marcos Cruz (programandala.net)" \ extra
arg-new constant arguments

\ Overwrite the default help option:
'?' \ short option
s" help" \ long option
s" Show this help."
  \ description
true \ switch type
arg.help-option arguments arg-add-option

\ Overwrite the default version option:
'V' \ short option
s" version" \ long option
s" Show version info."
  \ description
true \ switch type
arg.version-option arguments arg-add-option

\ Add the -i/--input option:
4 constant arg.input-option
'i' \ short option
s" input" \ long option
s" Set a file containing a list of input files (one per line). "
s" This option, as well as an explicit input file, " s+
s" starts the program immediately, " s+
s" using only the options that were specified before it." s+
  \ description
false \ switch type
arg.input-option arguments arg-add-option

\ Add the -l/--level option:
5 constant arg.level-option
'l' \ short option
s" level" \ long option
s" Set the headings level (1..6)." \ description
false \ switch type
arg.level-option arguments arg-add-option

\ Add the -o/--output option:
6 constant arg.output-option
'o' \ short option
s" output" \ long option
s" Set the output file." \ description
false \ switch type
arg.output-option arguments arg-add-option

\ Add the -u/--unique option:
7 constant arg.unique-option
'u' \ short option
s" unique" \ long option
s" Flag: reject the duplicated entries." \ description
true \ switch type
arg.unique-option arguments arg-add-option

\ Add the -v/--verbose option:
8 constant arg.verbose-option
'v' \ short option
s" verbose" \ long option
s" Flag: activate the verbose mode." \ description
true \ switch type
arg.verbose-option arguments arg-add-option

\ Add the -m/--markers option:
9 constant arg.markers-option
'm' \ short option
s" markers" \ long option
s" Set the glossary entry markers used in the source code; "
s\" default: \"doc{ }doc\"." s+
  \ description
false \ switch type
arg.markers-option arguments arg-add-option

\ Add the -a/--annex option:
10 constant arg.annex-option
'a' \ short option
s" annex" \ long option
s" Flag: set the annex mode, "
s" i.e. just convert the links of the given file, " s+
s" but don't extract glossary entries from it." s+
  \ description
true \ switch type
arg.annex-option arguments arg-add-option

\ Add the -s/--sections option:
11 constant arg.sections-option
's' \ short option
s" sections" \ long option
s" Flag: add a section heading before every "
s" glossary entry that starts with a new initial." s+
  \ description
true \ switch type
arg.sections-option arguments arg-add-option

\ Add the -d/--dir option:
12 constant arg.dir-option
'd' \ short option
s" dir" \ long option
s" Set the temp directory; default: "
default-temp-dir s+ s" ." s+
  \ description
false \ switch type
arg.dir-option arguments arg-add-option

: markers-option ( ca len -- )
  2dup first-name      dup ?marker starting-marker place
  /name 1 /string trim dup ?marker ending-marker   place ;
  \ Set the starting and ending markers, specified by string _ca
  \ len_. The markers are separated by at least one space.

variable helped
  \ Flag: help or version already shown?

: help-option ( -- )
  arguments arg-print-help helped on ;
  \ Show the help.

: verbose-option ( -- )
  verbose on s" Verbose mode is on" echo ;

: unique-option ( -- )
  unique on s" Unique mode is on" echo ;

: check-hierarchy ( -- )
  sections @ entry-heading-level @ 1 = and
  abort" Level 1 entry headings are incompatible with section headings" ;
  \ Check the level of entry headings is compatible with section
  \ headings.

: level-option ( ca len -- )
  0. 2swap >number nip
  abort" Invalid headings level"
  d>s dup min-heading-level max-heading-level 1+ within 0=
  abort" Headings level not in range 1..6"
  entry-heading-level ! check-hierarchy ;

: annex-option ( -- )
  ['] process-annex-file ['] (process-file) defer! annex on ;

: sections-option ( -- )
  sections on check-hierarchy
  s" Section headings are on" echo ;

: dir-option ( ca len -- )
  temp-dir! make-temp-dir ;
  \ Set the temporary directory _ca len_.

variable input-files# \ counter

: input-file ( ca len -- )
  1 input-files# +!
  s" Processing input file " 2over s+ echo process-file ;

: input-option ( ca len -- )
  s" Processing input files list " 2over s+ echo
  r/o open-file throw {: file-id :}
  begin  file-id read-line?
  while  save-mem input-file
  repeat file-id close-file throw ;
  \ Manage input-option _ca len_, which a file containing a list
  \ of input files.

: output-option ( ca len -- )
  output-filename @ abort" More than one output file specified"
  save-mem output-filename 2! ;

: version-option ( -- )
  ." Glosara " arguments arg>version @ str-get type cr
  helped on ;

: option ( n -- )
  case
    arg.help-option     of help-option     endof
    arg.version-option  of version-option  endof
    arg.input-option    of input-option    endof
    arg.output-option   of output-option   endof
    arg.unique-option   of unique-option   endof
    arg.verbose-option  of verbose-option  endof
    arg.level-option    of level-option    endof
    arg.markers-option  of markers-option  endof
    arg.annex-option    of annex-option    endof
    arg.sections-option of sections-option endof
    arg.dir-option      of dir-option      endof
    arg.non-option      of input-file      endof
  endcase ;

: option? ( -- n f )
  arguments arg-parse dup arg.done <> over arg.error <> and ;
  \ Parse the next option. Is it right?

\ ==============================================================
\ Boot {{{1

: init ( -- )
  helped off
  delete-temp-files
  argc off entry-file off
  input-files# off verbose off output-filename off
  unique off annex off
  previous-initial off
  2 entry-heading-level ! ;

: options ( -- )
  begin option? while option repeat drop ;

: done? ( -- f )
  input-files# @ 0> helped @ or ;
  \ Something done after parsing all of the options?

: run ( -- )
  init options done? if finnish else help-option then ;

run bye

\ vim: filetype=gforth textwidth=64
