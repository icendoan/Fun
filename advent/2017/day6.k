I:"J"$'"\t"\:*:0:`:day6.input
T:0 2 7 0
R:{(x_y),x#y}
p:{a:(#x)#_(v:x@r:*&x=max x)%#x;x[r]:0;a[(!.q.mod[v;#x])]+:1;x+R[(#x)-r+1;a]}
5=&/.{&/1_x}'=100 p\ T
min@.{min 1_x}'=10000 p\ I /p1
(-).|(. g)@*&2=#:'. g:=6681 p\ I /p2

