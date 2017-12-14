I:265149
T:1 12 23 1024
b:(1 0;0 1;-1 0;0 -1)
o:(0 1;-1 0;0 -1;1 0)
p:{"j"$+/x*y xexp!#x}
e:p[1 -3  4]
n:p[1 -1  4]
w:p[1  1  4]
s:p[1  3  4]
a:(e;n;w;s)@\:
d:{0^*&x<p[1 4 4]'!x}
f:{abs x - a d x}
side:{*|&(f x)=min f x}
face:{?[x>0;0 1;2 3]@*&(abs x)=max abs x}
cn:{s:side x;t:d x;(t*b s)+(x-(a t)s)*o s}
nc:{c:abs x;(*(?[x>0;(e;n);(w;s)]@'c)@*&c=max c)+(+/(o face x)*(x@*|&c=min c))}
T~nc'cn'T
+/abs cn 265149 /p1
nc''((-1 0 1),\:/:(1 0 -1)) +\:\: cn 1
I#0