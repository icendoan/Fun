I:265149
T:1 12 23 1024

axes:`n`e`w`s! (N:{(4*x*x)+1-x}; E:{(4*x*x)-(3*x)-1}; W:{(4*x*x)+x+1}; S:{(4*x*x)+(3*x)+1})
basis:`n`e`w`s!(0 1;1 0;-1 0;0 -1)
side:`n`e`w`s!((`w`e);(`s`n);(`e`w);(`n`s))

A:{1+(4*x*x)+(-x;-3*x;x;3*x)}
SE:{1+4*x*x+1}
Dp:{0^*&x<SE'!x}
Sd:{min abs x - A Dp x}
{(D x) + S x} I

cn:{
 a:(((Dp x)*basis d:*`n`e`w`s@&{x=min x}abs(A Dp x)-x));
 a+(Sd x)*basis (side d)@*0<(x-A Dp x)@&{x=min abs x}x-A Dp x
 }
 
nc:{
 m:*(*(`w`e;`s`n)@&(abs x)=max abs x)@0<x@&(abs x)=max abs x;
 s:*(*(`w`e;`s`n)@&(abs x)=min abs x)@0<x@&(abs x)=min abs x;
 ((axes m)@abs@*x@&(abs x)=max abs x)+(+/basis s)**x@&(abs x)=min abs x
 }

nc'cn'T
cn 23