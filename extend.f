: immediate
  last @ 4 + dup c@ 128 or swap c! ;

: \  #tib @ >in ! ; immediate
: (  41 word drop ; immediate

\ extend.4 -- bootstrap enough Core for a minimal tester on Itsy
\ IMMEDIATE \ and ( come first so comments and later IMMEDIATE work.

\ --- basic arithmetic / logic ---
: 1+  1 + ;
: 1-  1 - ;
: 2*  dup + ;
: negate  invert 1+ ;
: 0=  0 = ;
: 0<> 0= invert ;
: cell  4 ;
: cells 4 * ;
: cell+ 4 + ;
: bl 32 ;
: true  0 invert ;
: false 0 ;
: decimal 10 base ! ;
: hex 16 base ! ;
: over  >r dup r> swap ;
: nip swap drop ;
: tuck swap over ;
: 2dup over over ;
: 2drop drop drop ;
: +!  dup @ rot + swap ! ;

\ --- dictionary / memory ---
: here dp @ ;
: allot dp @ + dp ! ;
: variable  create 0 , ;
: constant  create , ;

\ EMIT via TYPE (TYPE is the primitive)
\ Avoid ( ... ) stack comments: ( is the paren-comment word
variable emit-buf
: emit  emit-buf c!  emit-buf 1 type ;
: cr  10 emit ;
: space 32 emit ;

\ Resolve key x-ts (value cells; use @ when compiling them)
\ FIND returns ( c-addr 0 | xt 1 | xt -1 )
\ Temporary tick without IF (drop flag; abort path omitted)
: (')  bl word find drop ;
(') 0branch constant '0branch
(') branch  constant 'branch
(') lit     constant 'lit
(') exit    constant 'exit
(') drop    constant 'drop

\ --- control flow ---
: if
  '0branch @ ,
  here 0 ,
; immediate

: then
  here swap !
; immediate

: else
  'branch @ ,
  here 0 ,
  swap
  here swap !
; immediate

: begin  here ; immediate

: until
  '0branch @ ,
  ,
; immediate

: again
  'branch @ ,
  ,
; immediate

: while
  '0branch @ ,
  here 0 ,
; immediate

: repeat
  'branch @ ,
  swap ,
  here swap !
; immediate

: ?dup  dup if else drop then ;

\ --- tick (full version with error check) ---
: '  bl word find 0= if abort then drop ;

\ --- CHAR ---
: char  bl word 1+ c@ ;
: [char]
  char 'lit @ , ,
; immediate

\ --- SOURCE (TYPE is a primitive) ---
: source  tib @  #tib @ ;

\ --- S"  (interpret-mode friendly) ---
: s"  34 word count ;

decimal

