( boot.fs )

( This file is part of Sfera, a library for QL SUPERFORTH )
( Version: 0.0.0+201512310308)
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

( ============================================================ )

LOWER

: \  ( "text<eol>" -- )
  10 word drop  ;  immediate

\ XXX FIXME -- `#in` can not be manipulated here,
\ because it's used to redirect the file being interpreted to
\ the keyboard (section 10.1 of the SUPERFORTH manual).

\ 0 open con_800x440a0x0
\ 2dup #in 2!
\ 2dup #out 2!
\ #default close
\ ' #default >body 2!

: set-window  ( d -- )
  2dup #in 2! #out 2!  ;

: set-default-channel  ( d -- )
  #default close ['] #default >body 2!  ;

: window  ( d "name" -- )
  2constant
  does>  ( -- ) ( dfa )  2@ set-window  ;

: set-main-window  ( d -- )
  2dup set-window set-default-channel  ;

: main-window  ( d "name" -- )
  2constant
  does>  ( -- ) ( dfa )
    2@ set-main-window  ;

0 open con_800x600a0x0 window 800x600
0 open con_1280x800a0x0 window 1280x800


\ 4 mode
0 paper
0 strip
4 ink
2 1 csize
2 1 border
\ cls

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
    >r r@ @ r@ 2+ @ r> 4 + @ sdv
  ;

create_device nfa1_
create_device nfa4_
nfa4_

: include  ( "name" -- )
  #in 2@ >r >r
  #file 2@ >r >r
  load_file  \ XXX FIXME does nothing
  \ XXX FIXME the channels are wrong
  r> r> #file 2!
  r> r> #in 2!  ;

\ include nfa4_words_fth

\ : words  ( -- )

\   \ lists all the words in the current vocabulary

\   cr latest
\   begin
\       dup id. space
\       @ dup 32768 =   \ link is 32768 at end
\   until  drop  ;

end_file

\ vim: filetype=superforth foldmarker=\ \:,\ ; foldmethod=marker
