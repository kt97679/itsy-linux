\ tester-min.fr -- minimal tests for Itsy
\ cat extend.4 tester-min.fr | ./itsy-linux

decimal

variable #errors
0 #errors !

: show-ok  [char] * emit ;
: show-fail  [char] ! emit  1 #errors +! ;
: assert  = if show-ok else show-fail then ;

1 2 drop  1 assert
1 2 swap  1 assert  2 assert
1 2 over  drop drop drop  show-ok
1 2 3 rot  1 assert  3 assert  2 assert

1 2 +  3 assert
5 3 -  2 assert
4 5 *  20 assert
0 0 =  true assert
1 0 =  false assert
0 0=  true assert
5 0<  false assert
0 1 - 0<  true assert
1 2 and  0 assert
3 1 and  1 assert
1 2 or  3 assert
0 invert  true assert

5 >r r>  5 assert

variable v1
42 v1 !
v1 @  42 assert
3 v1 +!
v1 @  45 assert

: sq  dup * ;
7 sq  49 assert

: tif  if 1 else 2 then ;
0 tif  2 assert
true tif  1 assert
: tloop  0 begin 1+ dup 5 = until ;
tloop  5 assert

#errors @ 0 assert
cr
bye
