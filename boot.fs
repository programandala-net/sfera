( boot.fs )

( This file is part of Sfera, a library for QL SUPERFORTH )
( Version: 0.0.0+201512312310)
( http://programandala.net/en.program.sfera.html )

( Author: Marcos Cruz [programandala.net] )

( ============================================================ )
( License )

( You may do whatever you want with this work, so long as you )
( retain all the copyright/authorship/acknowledgment/credits )
( notice[s] and this license in all redistributed copies and )
( derived works.  There is no warranty. )

( ============================================================ )
( History )

( See: )
( http://programandala.net/en.program.sfera.history.html )

( ============================================================== )
( Basic extensions )

LOWER

( ---------------------------------------------- )
( Comments )

: \  ( "ccc<eol>" -- )
  10 word drop  ;  immediate

\ ----------------------------------------------
\ Address artithmetic

: cell+  ( a1 -- a2 )  2+  ;
: cell-  ( a1 -- a2 )  2-  ;
: cells  ( a1 -- a2 )  2*  ;

: chars  ( ca1 -- ca2 )  ; immediate

\ ----------------------------------------------
\ Stack

: nip  ( x1 x2 -- x2 )  swap drop  ;
: bounds  ( ca len -- ca2 ca )  over + swap  ;

\ ----------------------------------------------
\ Operators

: 0<>  ( x1 x2 -- f )  = 0=  ;

\ ----------------------------------------------
\ Word headers

: traverse  ( a1 n -- a2 )
  swap  begin  over + 127 over c@ <  until
  swap drop  ;
  \ Move across a name field.  _a1_ is the address of either the
  \ length byte or the last letter.  If _n_=1, the motion is
  \ toward hi memory; if _n_=-l, the motion is toward low
  \ memory.  The _a2_ resulting is the address of the other end
  \ of the name.
  \
  \ Origin: fig-Forth.

: >name       ( cfa -- nfa )  1- -1 traverse  ;
: name>       ( nfa -- cfa )  1 traverse 1+  ;
: name>link   ( nfa -- lfa )  cell-  ;
: name>body   ( nfa -- pfa )  name> >body  ;
: >link       ( cfa -- lfa )  >name name>link  ;
: body>       ( pfa -- cfa )  cell-  ;
: body>name   ( pfa -- nfa )  body> >name  ;

: executing?  ( -- f )  state @ 0=  ;
: compiling?  ( -- f )  executing? 0=  ;

64 constant immediate-mask
: immediate?  ( cfa -- f )  >name c@ immediate-mask and  ;

\ ----------------------------------------------
\ Parsing

: parse-word  ( "name" -- ca )  bl word  ;

: parse-name  ( "name" -- ca len )  parse-word count  ;

: defined  ( "name" -- ca 0 | cfa 1 | cfa -1 )  parse-word find  ;

: [undefined]  ( "name" -- f )  defined nip 0=  ; immediate
: [defined]  ( "name" -- f )  [compile] [undefined] 0=  ; immediate

\ ----------------------------------------------
\ Compiling

: compile,  ( cfa -- )  ,  ;

: postpone  ( "name" -- )
  defined dup 0= 0 ?error  (  cfa 1 | cfa -1 )
  -1 = if    compile compile  \ non-immediate
       then  compile,
  ; immediate

\ ----------------------------------------------
\ Number prefixes

: c#  ( "name" -- c )
  parse-name drop c@
  compiling? if  postpone literal  then  ; immediate
  \ Parse a name and return the code of the its first
  \ character. This is an alternative to the standard words
  \ `char` and `[char]`.
  \
  \ Credits:
  \
  \ Code from the Solo Forth library.
  \ Original code inspired by eForth.

\ ----------------------------------------------
\ Strings

: s"  ( "ccc<quote>" --- ca len )
  c# " word dup count pad swap cmove c@ pad swap ;

\ ==============================================================
\ Tools

: binary  ( -- )  2 base !  ;
: hex.  ( n -- )  base @ >r  hex u. r> base !  ;
: bin.  ( n -- )  base @ >r  binary u. r> base !  ;

: (d.)  ( d n -- ca len )  <# 0 do  #  loop  #>  ;

variable base'

: <hex  ( -- )  base @ base' ! hex ; \ switch to hex
: hex>  ( -- )  base' @ base !     ; \ and back

: (dhex.)  ( d n -- )  <hex (d.) hex> type space  ;

: 32hex.   ( d -- ) 8 (dhex.)  ;
: 16hex.   ( n -- ) s->d 4 (dhex.)  ;
: 8hex.    ( b -- ) s->d 2 (dhex.)  ;

: words  ( -- )
  latest ( lfa )
  begin
    dup id. space
    @ dup 32768 =  \ last?
  until  drop  ;

: xwords  ( -- )
  latest ( lfa )
  begin
    dup link> 16hex dup id. space
    @ dup 32768 =  \ last?
  until  drop  ;

: .n  ( cfa -- )  >link id.  ;

: .dump-address  ( a -- )  cr 16hex. space  ;

: (dump)  ( ca len -- )
  8 2dup mod - + 8 / 0
  do
    dup .dump-address
    8 0 do  i over + c@ 8hex. loop
    dup 8 type
  8 + loop  drop  ;
  \ Show the contents of _n_ bytes starting from _ca_.

: dump  ( ca len -- )
  ?dup if  (dump)  else  drop  then  ;
  \ Show the contents of _n_ bytes starting from _ca_.

: (wdump)  ( a n -- )
  0 do
    i 4 mod 0= if  dup .dump-address  then
    dup @ 16hex. cell+
  loop  drop  ;
  \ Show the contents of _n_ cells starting from _a_.

: wdump  ( a n -- )
  ?dup if  (wdump)  else  drop  then  ;
  \ Show the contents of _n_ cells starting from _a_.

\ ==============================================================
\ Windows

\ Note: The main window can not be changed during interpretation
\ of a file, because `#in` is used to redirect the file being
\ interpreted to the keyboard (see section 10.1 of the
\ SUPERFORTH manual).

exvec: default-colors  ( -- )

: (default-colors)  ( -- )
  0 paper 4 ink 0 strip 0 set_mode 2 1 csize 2 1 border  ;

assign default-colors to-do (default-colors)

default-colors cls

: set-window  ( d -- )
  2dup #in 2! #out 2!  ;
  \ Make channel _d_ the current window.

: set-default-channel  ( d -- )
  #default close ['] #default >body 2!  ;
  \ Make channel _d_ the default main window,
  \ which is restored after an error.

: window  ( d "name" -- )
  2constant
  does>  ( -- ) ( dfa )  2@ set-window  ;
  \ Create a word called "name" that, when executed,
  \ will set channel _d_ as the current window.

: set-main-window  ( d -- )
  2dup set-window set-default-channel default-colors cls  ;
  \ Make channel _d_ the main window.

: main-window  ( d "name" -- )
  2constant
  does>  ( -- ) ( dfa )
    2@ set-main-window  ;
  \ Create a word called "name" that, when executed,
  \ will set channel _d_ as the main window.

0 open con_800x600a0x0  2dup 2constant #800x600
0 open con_1280x800a0x0 2dup 2constant #1280x800

: w800  ( -- )  #800x600 set-main-window  ;
: w1280 ( -- )  #1280x800 set-main-window  ;

cr
.( Words to change the main window:)  cr
.(   w800  = 800x600)  cr
.(   w1280 = 1280x800)  cr

\ ==============================================================
\ Devices

: create_device  ( "name" -- )

  \ creates a new default device and enables you to
  \ switch between it and mdv1_ etc. use is eg
  \    create_device fdr1_      then fdr1_ will set a
  \ device called fdr1_ as the default

  create
    latest 5 + @      \ get characters 3 and 4
    dup 63 and 48 - , \ compile device number
    ,                 \ compile characters 3 and 4
    latest 3 + @ ,    \ compile first 2 characters
  does>  ( -- ) ( pfa )
    >r r@ @
    r@ cell+ @
    r> [ 2 cells ] literal + @
    \ ." sdv parameters: " .s  cr \ XXX INFORMER
    sdv
  ;

create_device win1_
create_device nfa1_
create_device nfa4_
nfa4_

\ ==============================================================
\ Files

: include  ( "name" -- )
  cr .s \ XXX INFORMER
  \ #in 2@ >r >r
  #file 2@ >r >r
  load_file  \ XXX FIXME does nothing
  \ XXX FIXME the channels are wrong
  r> r> #file 2!
  \ r> r> #in 2!
  ;

end_file \ XXX TMP

\ ==============================================================
\ Decode

  \ XXX OLD -- ITC version

  \ This file is part of Solo Forth
  \ http://programandala.net/en.program.solo_forth.html

forth definitions decimal

need name>body  need case  need [if]

variable decode-level  decode-level off \ depth of nesting
variable decode-address  \ in the word being decoded

: indent  ( -- )
  cr decode-address @ u. decode-level @ 2 * spaces  ;

: indent+  ( -- )  1 decode-level +! indent  ;

  \ Special cases

: decode-branch    ( a1 -- a2 )  cell+ dup @ u.  ;

: decode-literal   ( a1 -- a2 )  cell+ dup @ .  ;

: decode-cliteral  ( a1 -- a2 )  cell+ dup c@ . 1-  ;

: decode-sliteral  ( a1 -- a2 )
  cell+ dup count type  dup c@ + 1-  ;

: decode-compile   ( a1 -- a2 )
  cell+ dup @ cell+ body>name id.  ;

  \ Special cases dispatcher

: decode-special  ( a1 -- a1 | a2 )

  dup @ case

    ['] compile   of  decode-compile    endof
    ['] lit       of  decode-literal    endof
    ['] clit      of  decode-cliteral   endof
    ['] slit      of  decode-sliteral   endof
    ['] branch    of  decode-branch     endof
    ['] 0branch   of  decode-branch     endof
    ['] ?branch   of  decode-branch     endof

    [defined] -branch [if]
      ['] -branch   of  decode-branch     endof  [then]

    \ XXX OLD -- for fig-Forth `do loop`:
    \ ['] (loop)    of  decode-branch     endof
    \ ['] (+loop)   of  decode-branch     endof

    \ XXX NEW -- for Forth-83 `do loop`:
    ['] (do)      of  decode-branch     endof
    ['] (?do)     of  decode-branch     endof

    ['] (.")      of  decode-sliteral   endof

  endcase  ;

( itc-decode )

  \ Checks of the main code

: decode-end?  ( xt -- f )
  \ Is the given xt the end of a definition?
  dup  ['] exit =  swap ['] (;code) =  or  ;

: colon-pfa?  ( pfa -- f )
  \ Is the given pfa a colon definition?
  body> @ ['] : @ =  ;

  \ Main code

: (decode)  ( pfa -- )

  \ Decode the definition at the given pfa.

  dup colon-pfa? if
    dup body> decode-address ! indent  ." : " dup body>name id.
    begin   ( pfa+n ) dup decode-address !
            dup @ dup ( pfa+n xt xt ) decode-end? 0=
            \ ( pfa+n xt f )
    while  \ high level & not end of colon definition
      \ ( pfa+n xt )
      cell+ ( pfa+n pfa' ) dup indent+  body>name id.
      key case  [char] q  of  sp0 @ sp! quit  endof \ q
                      bl  of  drop            endof \ space
                                 swap recurse \ default
          endcase  decode-special
      cell+  -1 decode-level +!
    repeat  indent cell+ body>name id. \ show the last word
  else  ." Not a colon definition."  then  drop  ;  -->

( itc-decode )

  \ Interface

: decode-usage  ( -- )
     \  <------------------------------>
  cr ." Keys: space=more, q=quit, other=deeper." cr  ;

: decode  ( "name" -- )
  decode-usage

  \ XXX OLD -- defined  ( "name" -- x 0 | xt 1 | xt -1 )
  \ defined if  >body  0 decode-level !  (decode)
  \         else  drop  -13 throw  then  ;

  \ XXX OLD -- defined  ( "name" -- ca len false | xt b true )
  \ defined 0= -13 ?throw
  \ drop >body  0 decode-level !  (decode)  ;

  \ XXX NEW -- defined  ( "name" -- nt | 0 )
  defined dup 0= -13 ?throw
  name>body  0 decode-level !  (decode)  ;

\ ==============================================================
\ XXX UNDER DEVELOPMENT

\ include nfa4_words_fth

\ : words  ( -- )

\   \ lists all the words in the current vocabulary

\   cr latest
\   begin
\       dup id. space
\       @ dup 32768 =   \ link is 32768 at end
\   until  drop  ;


: [else]  ( "..." -- )

  \ 1 begin   parse-name 2dup swap c@ and \ XXX OLD
  1 begin   parse-name dup
    while   2dup s" [if]" s=
            if    2drop 1+
            else  2dup s" [else]" s=
                  if    2drop 1- dup if  1+  then
                  else  s" [then]" s= if  1-  then
                  then
            then  ?dup 0= if  exit  then
  repeat  2drop drop  ; immediate

: [if]  ( "..." -- )  0= if postpone [else] then  ; immediate

: [then]  ( -- )  ; immediate

\ ==============================================================
\ Error

variable catcher

: catch  ( xt -- exception# | 0 )
  sp@ >r          ( xt )  \ save data stack pointer
  catcher @ >r    ( xt )  \ save previous catcher
  rp@ catcher !   ( xt )  \ set current catcher
  execute         ( )     \ `execute` returns if no `throw`
  r> catcher !    ( )     \ restore previous catcher
  r> drop         ( )     \ discard saved stack pointer
  0  ;            ( 0 )   \ normal completion, no error

\ ==============================================================
\ Number prefixes


\ XXX TODO evaluate catch throw

  \ Credits:
  \
  \ Code from the Solo Forth library.
  \ Original code adapted from eForth.

: x# ( -- ) ( "ccc" -- n | d )
  does> c@              \ new radix
  base @ >r  base !     \ save and set radix
  parse-name            \ get string
  ['] evaluate catch    \ convert to number, set trap
  r> base !  throw  ;   \ restore radix before error control

create b# ( "name" -- n | d )  2 c, x# immediate
create d# ( "name" -- n | d ) 10 c, x# immediate
create h# ( "name" -- n | d ) 16 c, x# immediate

end_file

\ vim: filetype=superforth foldmarker=\ \:,\ ; foldmethod=marker
