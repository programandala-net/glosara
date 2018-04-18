#! /usr/bin/env gforth

\ glosara.fs

: version s" 0.29.0+201804181612" ;

\ ==============================================================
\ Description

\ Glosara is a command line tool that extracts the glossary
\ documentation out of Forth sources, and creates a glossary
\ document in Asciidoctor format.

\ Glosara is written in Forth (http://forth-standard.org) with
\ Gforth (http://gnu.org/software/gforth).

\ ==============================================================
\ Author and license

\ Copyright (C) 2015, 2016, 2017, 2018 Marcos Cruz
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

require galope/first-name.fs      \ `first-name`
require galope/minus-leading.fs   \ `-leading`
require galope/replaced.fs        \ `replaced`
require galope/s-plus.fs          \ `s+`
require galope/s-s-quote.fs       \ `ss"` \ XXX TMP --
require galope/slash-name.fs      \ `/name`
require galope/stringer.fs        \ `stringer` \ XXX TMP --
require galope/trim.fs            \ `trim`

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

: echo ( ca len -- )
  verbose @ if type cr else 2drop then ;
  \ Display the string _ca len_ if `verbose` is on.

\ ==============================================================
\ Strings

\ 1024 allocate-stringer
here 1024 dup allot set-stringer

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
\ Files

: temp-directory ( -- ca len ) s" /tmp/" ;

: entry-filename-prefix ( -- ca len ) s" glosara.entry." ;

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

: entryname>count-suffix ( ca1 len2 -- ca2 len2 )
  entry-counter unique @ 0= and c>hex s" -" 2swap s+ ;
  \ Convert entry name _ca1 len2_ to its filename counter suffix
  \ _ca2 len2_.

: slashes>hyphens ( ca len -- ca' len' )
  s" -" s" /" replaced ;
  \ Replace slashes in _ca len_ with hyphens.

: dots>hyphens ( ca len -- ca' len' )
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

: entryname>basefilename ( ca1 len1 -- ca2 len2 )
  2dup string>hex dup >r null-filename /basefilename r> -
  dup 0< abort" Entry name too long" s+
  2swap entryname>suffix s+ ;
  \ Convert a glossary entry name _ca1 len1_ (a Forth word) to
  \ its temporary filename _ca2 len2_. The filename consists of
  \ `max-word-length` 8-bit hex numbers that represent the
  \ characters of the entry name, with trailing '0' digits to
  \ its maximum length.

: entryname>filename ( ca1 len1 -- ca2 len2 )
  entryname>basefilename
  entry-filename-prefix 2swap s+
  temp-directory 2swap s+ ;
  \ Convert a glossary entry name _ca1 len1_ (a Forth word) to
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

: entry-files-pattern ( -- ca len )
  temp-directory entry-filename-prefix s+ s" *" s+ ;
  \ Wildcard pattern for all temporary entry files.

: delete-entry-files ( -- )
  s" rm -f " entry-files-pattern s+ system ;
  \ Delete all temporary entry files.

\ ==============================================================
\ Links (cross references)

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

: .matchparts ( -- )
  debug? if
    cr ." before-match     «" before-match     2@ type ." »"
    cr ." link-text        «" link-text        2@ type ." »"
    cr ." constrained-code «" constrained-code 2@ type ." »"
    cr ." after-match      «" after-match      2@ type ." »" cr
  then ;
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
  debug? if cr ." PREPARE-IMPLICIT-LINK" 2dup type then \ XXX INFORMER
  2dup 1 implicit-link-rgx rgx-result ( ca len +n2 +n1)
       match>substring
  debug? if cr ."   BEFORE-MATCH=" 2dup type then \ XXX INFORMER
       before-match 2!
  2dup 2 implicit-link-rgx rgx-result ( ca len +n2 +n1)
       match>substring
  debug? if cr ."   LINK-TEXT=" 2dup type then \ XXX INFORMER
       link-text 2!
       3 implicit-link-rgx rgx-result ( ca len +n2 +n1)
       match>substring
  debug? if cr ."   AFTER-MATCH=" 2dup type then \ XXX INFORMER
       after-match 2!  ;
  \ Prepare the first implicit link found in string _ca len_
  \ by extracting its pieces into variables.

: build-implicit-link ( -- ca len )
  before-match 2@ `<< s+ link-text 2@ entryname>common-id s+
                     s" , " s+ link-text 2@ escaped s+
                 >>` s+ after-match 2@ s+ ;
  \ Build the implicit link from its pieces.

: actual-implicit-link? ( -- f )
  .matchparts
  before-match 2@ dup if 1- + c@ '`' = if false exit then else 2drop then
  after-match  2@     if      c@ '`' = if false exit then else  drop then
  true ;

: convert-implicit-link ( ca1 len1 -- ca2 len2 )
  debug? if cr ." IMPLICIT-LINK" 2dup type then \ XXX INFORMER
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
  debug? if cr ." PREPARE-CONSTRAINED-CODE" 2dup type then \ XXX INFORMER
  2dup 1 constrained-code-rgx rgx-result ( ca len +n2 +n1)
       match>substring
  debug? if cr ."   BEFORE-MATCH=" 2dup type then \ XXX INFORMER
       before-match 2!
  2dup 2 constrained-code-rgx rgx-result ( ca len +n2 +n1)
       match>substring
  debug? if cr ."   CONSTRAINED-CODE=" 2dup type then \ XXX INFORMER
       constrained-code 2!
       3 constrained-code-rgx rgx-result ( ca len +n2 +n1)
       match>substring
  debug? if cr ."   AFTER-MATCH=" 2dup type then \ XXX INFORMER
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
  debug? if cr ." BUILD-CONSTRAINED-CODE " then \ XXX INFORMER
  before-match 2@ `` s+ constrained-code 2@ escaped s+ `` s+
  after-match  2@ s+
  debug? if cr ." BUILD-CONSTRAINED-CODE" 2dup type then \ XXX INFORMER
  ;
  \ Build the constrained code from its pieces.

: (constrained-code ( ca1 len1 -- ca2 len2 )
  debug? if cr ." (CONSTRAINED-CODE " then \ XXX INFORMER
  prepare-constrained-code build-constrained-code ;
  \ Manage the first constrained code (a text between double
  \ backticks, e.g. ``dup + drop``) contained in entry line _ca1
  \ len1_.

: constrained-code? ( ca len -- f )
  debug? if cr .s cr ." CONSTRAINED-CODE? " then \ XXX INFORMER
  constrained-code-rgx rgx-csearch -1 >
  debug? if cr ." End of CONSTRAINED-CODE?=" dup . then \ XXX INFORMER
  ;
  \ Does string _ca len_ contain constrained code?

: convert-constrained-code ( ca len -- ca' len' )
  debug? if cr .s cr ." CONVERT-CONSTRAINED-CODE=" 2dup type then \ XXX INFORMER
  begin  2dup constrained-code?
  while  (constrained-code
  repeat
  debug? if cr .s cr ." CONVERT-CONSTRAINED-CODE before restore=" 2dup type then \ XXX INFORMER
  restore-double-backticks
  debug? if cr ." End of CONVERT-CONSTRAINED-CODE=" 2dup type then \ XXX INFORMER
  ;
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
  before-match 2@ `<< s+ link-text 2@ xreflabel 2@ >unique-id s+
                     s" , " s+ link-text 2@ escaped s+
                 >>` s+ after-match 2@ s+ ;
  \ Build the explicit link from its pieces.
  \
  \ XXX TODO -- Factor and combine with `build-implicit-link`.

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
  debug? if cr ." CONVERT-LINKS=" 2dup type then \ XXX INFORMER
  preserve-double-backticks convert-explicit-links
                            convert-implicit-links finish-links
  restore-double-backticks
  debug? if cr ." End of CONVERT-LINKS=" 2dup type then \ XXX INFORMER
  ;
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
\ Source parser

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

16 constant /marker
  \ Maximum length of the starting and ending markers, in chars.

: ?marker ( len -- )
  /marker > abort" Marker too long" ;

create starting-marker /marker chars allot
  \ Starting marker storage.

create ending-marker /marker chars allot
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

      variable headings-level
1 constant min-headings-level
6 constant max-headings-level

create (heading-markup) max-headings-level chars allot
(heading-markup) max-headings-level '=' fill

: heading-markup ( -- ca len )
  (heading-markup) headings-level @ ;

: .heading-markup ( -- ) heading-markup type space ;

: common-anchor ( ca len -- )
  2dup entry-counter@ 1 = if   entryname>common-anchor type
                          else 2drop then ;
  \ If entry name _ca len_ is the first one with its name,
  \ create a common anchor for it.

: heading-attr-list ( ca len -- )
  cr ." [#" entryname>unique-id type ." ]" cr ;
  \ Create the Asciidoctor attribute list for entry name _ca1
  \ len1_. The attribute list contains only the corresponding
  \ id block attribute, which is unique for each entry.

: heading-line ( ca len )
  .heading-markup 2dup common-anchor escaped type cr ;
  \ Create a glossary heading line for entry name _ca len_.
  \ The Asciidoctor inline macro `pass:[]` is used to force
  \ replacement of special characters of Forth names (e.g. '<')
  \ to HTML entities.

: heading ( ca len -- )
  2dup heading-attr-list heading-line ;
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
  \ debug? if cr ." START-HEADER=" 2dup type then \ XXX INFORMER
  1 entry-line# +!
  \ debug? if cr ." In START-HEADER (0) " .s then \ XXX INFORMER
  2dup first-name
  \ debug? if cr ." In START-HEADER (1) " .s then \ XXX INFORMER
  2dup create-entry-file
  \ debug? if cr ." In START-HEADER (2) " .s then \ XXX INFORMER
  to outfile-id
  \ debug? if cr ." In START-HEADER (3) " .s then \ XXX INFORMER
                       heading cr
  \ debug? if cr ." In START-HEADER (4) " .s then \ XXX INFORMER
       header-boundary
  \ debug? if cr ." End of START-HEADER " .s then \ XXX INFORMER
  ;
  \ Start an entry header, whose first line is _ca len_.

: start-of-header? ( -- f )
  debug? if cr ." START-OF-HEADER? " then \ XXX INFORMER
  entry-line# @ 2 =
  debug? if cr ." START-OF-HEADER?=" dup . then \ XXX INFORMER
  ;

: end-of-header? ( len -- f )
  debug? if cr ." END-OF-HEADER? " then \ XXX INFORMER
  0= processing-header? and
  debug? if cr ." END-OF-HEADER?=" dup . then \ XXX INFORMER
  ;

: update-entry-line# ( len -- )
  0<> abs entry-line# +! ;
  \ If _len_ (the length of the current entry line)
  \ is not zero, increase the count of entry lines.

: (process-entry-line) ( ca len -- )
  debug? if cr ." (PROCESS-ENTRY-LINE)=" 2dup type then \ XXX INFORMER
  dup update-entry-line#
  dup end-of-header?   if end-header   exit then
      start-of-header? if start-header exit then
  convert-constrained-code
  debug? if cr ." (PROCESS-ENTRY-LINE) before CONVERT-LINKS=" 2dup type then \ XXX INFORMER
  convert-links type cr ;
  \ Process line _ca len_, which is part of the contents
  \ of a glossary entry.

: add-source-file ( -- )
  cr ." Source file: <" processed-file 2@ type ." >." cr ;

: end-entry ( -- )
  add-source-file entry-line# off ;

: process-entry-line ( ca len -- )
  debug? if cr ." PROCESS-ENTRY-LINE=" 2dup type then \ XXX INFORMER
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
  debug? if cr ." ENTRY? " then \ XXX INFORMER
  entry-line# @ 0<> ;
  \ Is there an entry being processed?

: process-line ( ca len -- )
  debug? if cr ." LINE=" 2dup type then \ XXX INFORMER
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
  debug? if cr ." FILE=" 2dup type then \ XXX INFORMER
  2dup processed-file 2!
  r/o open-file throw dup (process-file) close-file throw ;
  \ Process the glossary or annex file _ca len_.

\ ==============================================================
\ Glossary

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
  s" cat " entry-files-pattern s+ ;
  \ Return the shell `cat` command that concatenates all the
  \ glossary entry files and sends them to standard output.

: create-glossary ( -- )
  cat >file system ;
  \ Send all the glossary entry files to standard output or to
  \ the output file, if specified.

: finnish ( -- )
  annex @ 0= if create-glossary then ;
  \ Finnish the task, by creating the glossary file, if needed.

\ ==============================================================
\ Argument parser

\ Create a new argument parser:
s" glosara" \ name
s" [ OPTION | INPUT-FILE ] ..." \ usage
version
s" (C) 2015-2017 Marcos Cruz (programandala.net)" \ extra
arg-new constant arguments

\ Add the default -?/--help option:
arguments arg-add-help-option

\ Add the default --version option:
arguments arg-add-version-option

\ Add the -i/--input option:
4 constant arg.input-option
'i'               \ short option
s" input"         \ long option
s" set file containing a list of input files (one per line)"
                  \ description
false             \ switch type
arg.input-option arguments arg-add-option

\ Add the -l/--level option:
5 constant arg.level-option
'l'                                 \ short option
s" level"                           \ long option
s" set the headings level (1..6); "
s" must be used before the input files or the --input option" s+
                                    \ description
false                               \ switch type
arg.level-option arguments arg-add-option

\ Add the -o/--output option:
6 constant arg.output-option
'o'                     \ short option
s" output"              \ long option
s" set the output file" \ description
false                   \ switch type
arg.output-option arguments arg-add-option

\ Add the -u/--unique option:
7 constant arg.unique-option
'u'                           \ short option
s" unique"                    \ long option
s" reject duplicated entries" \ description
true                          \ switch type
arg.unique-option arguments arg-add-option

\ Add the -v/--verbose option:
8 constant arg.verbose-option
'v'                       \ short option
s" verbose"               \ long option
s" activate verbose mode" \ description
true                      \ switch type
arg.verbose-option arguments arg-add-option

\ Add the -m/--markers option:
9 constant arg.markers-option
'm'                       \ short option
s" markers"               \ long option
s" set markers, e.g. 'glossary{ }glossary'; default: `doc{ }doc`"
                          \ description
false                     \ switch type
arg.markers-option arguments arg-add-option

\ Add the -a/--annex option:
10 constant arg.annex-option
'a'               \ short option
s" annex"         \ long option
s" set annex mode: only convert the links"
                  \ description
true              \ switch type
arg.annex-option arguments arg-add-option

: markers-option ( ca len -- )
  2dup first-name      dup ?marker starting-marker place
  /name 1 /string trim dup ?marker ending-marker   place ;
  \ Set the starting and ending markers, specified by string _ca
  \ len_. The markers are separated by at least one space.

variable helped \ flag: help already shown?

: help ( -- )
  arguments arg-print-help helped on ;
  \ Show the help.

: verbose-option ( -- )
  verbose on s" Verbose mode is on" echo ;

: unique-option ( -- )
  unique on s" Unique mode is on" echo ;

: level-option ( ca len -- )
  0. 2swap >number nip
  abort" Invalid headings level"
  d>s dup min-headings-level max-headings-level 1+ within 0=
  abort" Headings level not in range 1..6"
  headings-level ! ;

: annex-option ( -- )
  ['] process-annex-file ['] (process-file) defer! annex on ;

variable input-files# \ counter

: input-file ( ca len -- )
  1 input-files# +!
  s" Processing input file " 2over s+ echo process-file ;

variable tmp \ XXX TMP --

: input-option ( ca len -- )
  s" Processing input files list " 2over s+ echo
  r/o open-file throw tmp !
  begin  tmp @ read-line?
  while  save-mem input-file
  repeat tmp @ close-file throw ;
  \ XXX FIXME -- The system crashes when the stack is
  \ used to hold the _fid_. That's why `tmp` is used at the
  \ moment. The problem is something is left on the data stack
  \ during the parsing.

: output-option ( ca len -- )
  output-filename @ abort" More than one output file specified"
  save-mem output-filename 2! ;

: version-option ( -- )
  arguments arg-print-version ;

: option ( n -- )
  case
    arg.help-option    of help           endof
    arg.version-option of version-option endof
    arg.input-option   of input-option   endof
    arg.output-option  of output-option  endof
    arg.unique-option  of unique-option  endof
    arg.verbose-option of verbose-option endof
    arg.level-option   of level-option   endof
    arg.markers-option of markers-option endof
    arg.annex-option   of annex-option   endof
    arg.non-option     of input-file     endof
  endcase ;

: option? ( -- n f )
  arguments arg-parse dup arg.done <> over arg.error <> and ;
  \ Parse the next option. Is it right?

\ ==============================================================
\ Boot

: init ( -- )
  helped off
  delete-entry-files argc off entry-file off
  input-files# off verbose off output-filename off
  unique off annex off
  2 headings-level ! ;

: options ( -- )
  begin option? while option repeat drop ;

: fine? ( -- f )
  input-files# @ 0> helped @ or ;
  \ Fine options?

: run ( -- )
  init options fine? if finnish else help then ;

run bye

\ vim: filetype=gforth textwidth=64
