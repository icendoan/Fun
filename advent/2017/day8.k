T:{"SSJSSJ"$'x 0 1 2 4 5 6}'" "\:'("b inc 5 if a > 1"; "a inc 1 if b < 5"; "c dec -10 if a >= 1"; "c inc -20 if c == 10");
I:{"SSJSSJ"$'x 0 1 2 4 5 6}'" "\:'0:`day8.input
c:(`$(">";"<";">=";"<=";"==";"!="))!(>;<;{(x+1)>y};{x<1+y};=;{~x=y})
o:(`inc`dec)!(+;-)
R:(`$())!7h$()
p:{[x;y]if[(c y 4).(0^x y 3;y 5);x[*y]:(o y 1).(0^x@*y;y 2)];x}
{*(. x)@&(. x)=max@(. x)} (p/)[R;I] /p1
S:R p\ I
q)S:flip (uj/) 2 _ enlist each S
|/.|/'S /p2