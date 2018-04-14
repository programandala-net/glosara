#! /usr/bin/env gforth

\ glosara.fs

: version s" 0.23.1+201804140217" ;

\ ==============================================================
\ Description

\ Glosara is a command line tool that extracts the glossary
\ documentation out of Forth sources, and creates a glossary
\ document in Asciidoctor format.

\ Glosara is written in Forth (http://forth-standard.org) with
\ Gforth (http://gnu.org/software/gforth).

\ ==============================================================
\ Author and license

\ Copyright (C) 2015, 2016, 2017 Marcos Cruz (programandala.net)

\ You may do whatever you want with this work, so long as you
\ retain the copyright notices and this license in all
\ redistributed copies and derived works. There is no warranty.

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

: (checkstack) ( ca len -- )
  cr type cr ."   " .s ;

: checkstack ( ca len -- )
  debugging @ if (checkstack) else 2drop then ;

: (checkstr) ( ca1 len1 ca2 len2 -- )
  (checkstack) cr ."   " 2dup type ;

: checkstr ( ca1 len1 ca2 len2 -- )
  debugging @ if (checkstr) else 2drop then ;

\ ==============================================================
\ Misc

variable verbose
  \ Flag: verbose mode?

variable unique
  \ Flag: unique mode?

: echo ( ca len -- )
  verbose @ if type cr else 2drop then ;
  \ Print the string _ca len_ if `verbose` is on.

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

2variable parsed-file

variable output-file \ output file identifier

: set-standard-output ( -- )
  stdout to outfile-id ;

: create-output-file ( -- fid )
  output-filename 2@
  w/o create-file throw dup output-file ! ;
  \ XXX TODO -- Not used.

: close-output-file ( -- )
  output-file @ close-file throw ;
  \ XXX TODO -- Not used.

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
  parsed-file 2@ filename>xreflabel-suffix ;

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

: (close-entry-file) ( -- )
  close-file throw entry-file off ;
  \ Close the glossary entry file.

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
s" `\S+`" implicit-link-rgx
rgx-compile 0= [if]
  cr .( Compilation of regular expression)
  cr .( for implicit link failed on position ) . cr
  quit
[then]

rgx-create explicit-link-rgx
s" `<<(\S+)?\s*,\s*(\S+)>>`" explicit-link-rgx
rgx-compile 0= [if]
  cr .( Compilation of regular expression)
  cr .( for explicit link failed on position ) . cr
  quit
[then]

: match>len ( +n2 +n1 -- len )
  - ;
  \ Convert regular expression match result start position _+n1_
  \ and end position _+n2_ to the length _len_ of the
  \ corresponding substring.
  \ XXX TODO -- Remove.

: match>implicit-link-text ( ca1 len1 +n2 +n1 -- ca2 len2 )
  2dup match>len 2 - >r nip nip + 1+ r> ;
  \ Extract from string _ca1 len1_ the link text _ca2 len2_ of
  \ the implicit link matched by _+n2 +n1_, i.e. the link text
  \ that starts at position _+n1_ and ends before position
  \ _+n2_.

: 4dup ( x1..x4 -- x1..x4 x1..x4 )
  2over 2over ;

: entryname>common-id ( ca1 len1 -- ca2 len2 )
  string>hex s" entry" 2swap s+ ;
  \ Create a cross reference label _ca2 len2_ from entry name
  \ _ca1 len1_.

: ?filename ( ca1 len1 -- ca1 len1 | ca2 len2 )
  ?dup 0= if drop parsed-file 2@ then ;
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
  parsed-file 2@ >unique-id ;
  \ Create a unique cross reference label _ca2 len2_ from entry
  \ name _ca1 len1_ and the currently parsed file.

2variable before-link
2variable xreflabel
2variable link-text
2variable after-link

: .linkparts ( -- )
  cr ." before-link " before-link 2@ type
  cr ." link-text   " link-text   2@ type
  cr ." after-link  " after-link  2@ type cr ;
  \ XXX INFORMER
  \ XXX TMP -- For debugging.

: `<< ( -- ca len ) s" [backtick-here][begin-cross-reference-here]" ;

: >>` ( -- ca len ) s" [end-cross-reference-here][backtick-here]" ;

: match>before ( ca1 len1 +n2 +n1 -- ca2 len2 )
  nip nip >stringer ;
  \ Return string _ca2 len2_ that is the left part of string
  \ _ca1 len1_ before a regular expression match result _+n2
  \ +n1_, being _+n1_ the start of the match and _+n2_ the end
  \ of the match. _ca2 len2_ is in the `stringer`, in order to
  \ protect it from later modifications of the main string _ca1
  \ len1_ done by `escaped`, which may move it in the heap.

: match>after ( ca1 len1 +n2 +n1 -- ca2 len2 )
  drop /string >stringer ;
  \ Return string _ca2 len2_ that is the right part of string
  \ _ca1 len1_ after a regular expression match result _+n2
  \ +n1_, being _+n1_ the start of the match and _+n2_ the end
  \ of the match. _ca2 len2_ is in the `stringer`, in order to
  \ protect it from later modifications of the main string _ca1
  \ len1_ done by `escaped`, which may make it longer,
  \ overwritting the right part after the match, or move the
  \ whole string in the heap.

: prepare-implicit-link ( ca len -- )
  0 implicit-link-rgx rgx-result ( ca len +n2 +n1)
  4dup match>implicit-link-text link-text   2!
  4dup match>before             before-link 2!
       match>after              after-link  2! ;
  \ Prepare the first implicit link found in string _ca len_
  \ by extracting its pieces into variables.
  \
  \ XXX TODO -- Use the stack instead of variables.

: escaped ( ca1 len1 -- ca2 len2 )
  s" &#35;"       s" #"   replaced
  s" &#34;"       s\" \q" replaced
  s" &#42;"       s" *"   replaced
  s" &#60;"       s" <"   replaced
  s" &#61;"       s" ="   replaced
  s" &#62;"       s" >"   replaced
  s" {backslash}" s" \"   replaced ;
  \ Escape special characters in string _ca1 len1_, returning
  \ the result string _ca2 len2_. Escaping certain characters is
  \ needed in order to prevent troubles during the conversion of
  \ Asciidoctor into HTML and PDF.

: build-implicit-link ( -- ca len )
  before-link 2@ `<< s+ link-text 2@ entryname>common-id s+
                     s" , " s+ link-text 2@ escaped s+
                 >>` s+ after-link 2@ s+ ;
  \ Build the implicit link from its pieces.
  \
  \ XXX TODO -- Use the stack instead of variables.
  \ XXX TODO -- Factor and combine with `build-implicit-link`.

: implicit-link ( ca1 len1 -- ca2 len2 )
  prepare-implicit-link build-implicit-link ;
  \ Convert the first implicit link (a word between backticks,
  \ e.g. `entryname`) contained in entry line _ca1 len1_ to
  \ Asciidoctor markup (e.g. <<ID, entryname>>), returning the
  \ modified string _ca2 len2_.
  \
  \ In order to prevent recursion in `implicit-links?`,
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

: implicit-links? ( ca1 len1 -- ca1 len1 false | ca2 len2 true )
  0 >r begin  2dup implicit-link-rgx rgx-csearch -1 >
              dup r> + >r
       while  implicit-link
       repeat r> 0<> ;
  \ If the entry line _ca1 len1_ contains implicit links, i.e.
  \ words sourrounded by backticks, convert them to Asciidoctor
  \ markup and return the modified string _ca2 len2_ and a true
  \ flag; else return the original string and a false flag.

: implicit-links ( ca1 len1 -- ca1 len1 | ca2 len2 )
  implicit-links? drop ;
  \ If the entry line _ca1 len1_ contains implicit links, i.e.
  \ words sourrounded by backticks, convert them to Asciidoctor
  \ markup and return the modified string _ca2 len2_; else do
  \ nothing.

: match>substring ( ca1 len1 +n2 +n1 -- ca2 len2 )
  2dup match>len >r nip nip + r> ;
  \ Extract from string _ca1 len1_ the link text of the explicit
  \ link that starts at position _+n1_ and ends before position
  \ _+n2_.

: prepare-explicit-link ( ca len -- )
  2>r
  2r@ 1 explicit-link-rgx rgx-result ( ca1 len1 +n2 +n1)
        match>substring xreflabel 2!
  2r@ 2 explicit-link-rgx rgx-result ( ca1 len1 +n2 +n1)
        match>substring link-text 2!
  2r> 0 explicit-link-rgx rgx-result ( ca1 len1 +n2 +n1)
        4dup match>before before-link 2!
             match>after  after-link  2! ;
  \ Prepare the first explicit link found in string _ca len_ by
  \ extracting its pieces into variables.
  \
  \ XXX TODO -- Use the stack instead of variables.

: build-explicit-link ( -- ca len )
  before-link 2@ `<< s+ link-text 2@ xreflabel 2@ >unique-id s+
                     \ s" , pass:c[" s+ link-text 2@ s+ s" ]" s+
                     \ XXX OLD
                     s" , " s+ link-text 2@ escaped s+
                 >>` s+ after-link 2@ s+ ;
  \ Build the explicit link from its pieces.
  \
  \ XXX TODO -- Use the stack instead of variables.
  \ XXX TODO -- Factor and combine with `build-implicit-link`.

: explicit-link ( ca1 len1 -- ca2 len2 )
  prepare-explicit-link build-explicit-link ;

: explicit-links? ( ca1 len1 -- ca1 len1 false | ca2 len2 true )
  0 >r begin  2dup explicit-link-rgx rgx-csearch -1 >
              dup r> + >r
       while  explicit-link
       repeat r> 0<> ;
  \ If the entry line _ca1 len1_ contains explicit and
  \ unfinished Asciidoctor links, finish them and
  \ return the modified string _ca2 len2_ and a true flag; else
  \ return the original string and a false flag.

: explicit-links ( ca1 len1 -- ca1 len1 | ca2 len2 )
  explicit-links? drop ;
  \ If the entry line _ca1 len1_ contains explicit and
  \ unfinished Asciidoctor links, finish them and
  \ return the modified string _ca2 len2_; else do nothing.

: double-backticks-substitution ( -- ca len )
  s" [Unconstrained monospace markup was here!]" ;
  \ The temporary string used to replace double backticks.

: preserve-double-backticks ( ca1 len1 -- ca1 len1 | ca2 len2 )
  double-backticks-substitution s" ``" replaced ;
  \ Replace all double backticks in _ca1 len1_ with a temporary
  \ string.

: restore-double-backticks ( ca1 len1 -- ca1 len1 | ca2 len2 )
  s" ``" double-backticks-substitution replaced ;
  \ Restore all double backticks that were in _ca1 len1_,
  \ replacing the temporary string used by
  \ `preserve-double-backticks`.

: links ( ca1 len1 -- ca1 len1 | ca2 len2 )
  preserve-double-backticks explicit-links
                            implicit-links finish-links
  restore-double-backticks ;
  \ If the entry line _ca1 len1_ contains links,
  \ convert them to Asciidoctor markup and return the modified
  \ string _ca2 len2_; else do nothing.
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

: end-of-entry? ( ca len -- f )
  ending-marker count str= ;

: start-entry ( -- )
  1 entry-line# ! entry-header off header-status off ;

: process-code-line ( ca len -- )
  start-of-entry? if start-entry then ;
  \ Process input file line _ca len_.

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
  \ .heading-markup 2dup common-anchor ." pass:c[" type ." ]" cr ;
                     \ XXX OLD
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
  1 entry-line# +!
  2dup first-name 2dup create-entry-file to outfile-id
                       heading cr
       header-boundary ;
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
  links type cr ;
  \ Process input line _ca len_, which is part of the contents
  \ of a glossary entry.

: add-source-file ( -- )
  cr ." Source file: <" parsed-file 2@ type ." >." cr ;

: end-entry ( -- )
  add-source-file entry-line# off ;

: process-entry-line ( ca len -- )
  2dup end-of-entry? if   2drop end-entry
                     else (process-entry-line) then ;
  \ Process input line _ca len_, which is part of a glossary
  \ entry, maybe its end markup.

: tidy ( ca len -- ca' len' )
  /name 2nip dup 0<> abs /string ;
  \ Remove the first name (a substring delimited by spaces) from
  \ _ca len_ and the first space after it. The removed name is
  \ supposed to be the Forth line comment backslash, or the
  \ corresponding markup of other languages.

: process-line ( ca len -- )
  tidy entry-line# @ if   process-entry-line
                     else process-code-line then ;
  \ Process the input line _ca len_.

: read-line? ( fid -- ca len f )
  >r line-buffer dup /line-buffer r> read-line throw ;

: begin-parsing ( -- )
  entry-line# off entry-header off header-status off ;
  \ Init the parser variables.

: end-parsing ( -- )
  close-entry-file set-standard-output ;
  \ Set `emit` and `type` to standard output.

: parse-file ( fid -- )
  begin-parsing
  begin dup read-line? while process-line repeat 2drop
  end-parsing ;
  \ Extract the glossary information from file _fid_ and
  \ print it to standard output.

: parse-input-file ( ca len -- )
  2dup parsed-file 2!
  r/o open-file throw dup parse-file close-file throw ;
  \ Extract the glossary information from file _ca len_ and
  \ print it to standard output.

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
  \ Return the shell `cat` command to concatenate and print all
  \ glossary entry files.

: glossary ( -- )
  cat >file system ;
  \ Print the final glossary to standard output or to the output
  \ file, if specified.

\ ==============================================================
\ Argument parser

\ Create a new argument parser:
s" glosara" \ name
s" [ OPTION | INPUT-FILE ] ..." \ usage
version
s" (C) 2015-2017 Marcos Cruz (programandala.net)" \ extra
arg-new constant arguments

\ Add the default ?/--help option:
arguments arg-add-help-option

\ Add the default --version option:
arguments arg-add-version-option

\ Add the -i/--input option:
4 constant arg.input-option
'i'               \ short option
s" input"         \ long option
s" set file that contains a list of input files (one per line)"
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

\ Add the -v/--verbose option:
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
s" set markers; e.g. 'glossary{ }glossary'" \ description
false                     \ switch type
arg.markers-option arguments arg-add-option

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

variable input-files# \ counter

: input-file ( ca len -- )
  1 input-files# +!
  s" Processing input file " 2over s+ echo parse-input-file ;

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
    arg.non-option     of input-file     endof
  endcase ;

: option? ( -- n f )
  arguments arg-parse dup arg.done <> over arg.error <> and ;
  \ Parse the next option. Is it right?

\ ==============================================================
\ Boot

: init ( -- )
  helped off
  delete-entry-files argc off
  input-files# off verbose off output-filename off unique off
  2 headings-level ! ;

: options ( -- )
  begin option? while option repeat drop ;

: fine? ( -- f )
  input-files# @ 0> helped @ or ;
  \ Fine options?

: run ( -- )
  init options fine? if glossary else help then ;

run bye

\ vim: filetype=gforth textwidth=64
