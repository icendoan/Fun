'use strict'; // IPO, TRADE, DIV, PRICE, STOCK, TRAIN
const E = { acct: [], sym: [], event: [], price: [], qty: [], hooks :[] }
const B = { acct: [], cash: [], eqty: [], hooks: [] }
const M = { sym: [], price: [], avail: [], total: [], hooks: [] }
const S = { acct: [], sym: [], qty: [], hooks: [] }
const D = { acct: [], earnings: [], hooks: [] }
const event=(a,s,e,p,q)=>{const i=E.acct.length;E.acct.push(a);E.sym.push(s);E.event.push(e);E.price.push(p);E.qty.push(q);hooks(E,i)}
const clear = t => {while(t.rows.length>1)t.deleteRow(-1);return t;}
const prx = s => {const i=M.sym.findIndex(x=>x===s);return i<0?0:M.price[i];}
const tot = s => {const i=M.sym.findIndex(x=>x===s);return i<0?0:M.total[i];}
const own = (a,s)=> {for(var i=0;i<S.sym.length;i++)if((S.sym[i]===s)&&S.acct[i]===a)return S.qty[i];return 0;}
const ipo   = (a,p,q) => event(a, '', 'IPO', p, q)
const trade = (a,s,q) => event(a, s,'TRADE', prx(s), q)
const div   = (a,p,q) => event(a, '', 'DIV', p, q)
const price = (s,p)   => event('', s, 'PRICE', p, 0)
const train = (a,p,q) => event(a,'','TRAIN',p,q)
const stock = (a,s,q) => event(a, s, 'STOCK', prx(s), q)
const player= (a,p)   => event(a,'','IPO',p,0)
const cash  = (a,q)   => event(a,'','CASH',0,q)
const dbg = (x) => {console.log(x);x}
const hooks = (t, i) => {var p = [];for (var f in t)if(f!=='hooks')p.push(t[f][i]);t.hooks.forEach(f=>f.apply(null,p));}
const balance_event_hook = (a,s,e,p,q) => {
    var i = B.acct.findIndex(x=>x===a);
    if(e==='IPO'){if(i<0){i=B.acct.length;B.acct.push(a);B.cash.push(q>0?0:p);B.eqty.push(0);}else{B.cash[i]=q>0?0:p;B.eqty[i]=0;}hooks(B,i)}
    if(e==='TRADE'){B.cash[i]-=q*prx(s);hooks(B,i);}
    if(e==='FOLD'){B.cash[i]=0;B.eqty[i]=0;hooks(B,i);}
    if(e==='DIV'){for(var j=0;j<S.acct.length;j++)if(S.sym[j]===a){const k=B.acct.findIndex(x=>x===S.acct[j]);B.cash[k]+=p*q*S.qty[j]/tot(a);hooks(B,j);}B.cash[i]+=p*(1-q);hooks(B,i);}
    if(e==='CASH'){B.cash[i]+=q;hooks(B,i)}
    if(e==='TRAIN'){B.cash[i]-=p*q;hooks(B,i);}}
const market_event_hook = (a,s,e,p,q) => {
    const i = M.sym.findIndex(x=>x===s);
    if((q>0)&&(i<0)&&(e==='IPO')){M.sym.push(a);M.price.push(p/q);M.avail.push(0);M.total.push(q);hooks(M,M.sym.length-1)}
    if((q>0)&&(i>=0)&&(e==='IPO')){M.price[i]=p/q;M.avail[i]=0;M.total[i]=q;hooks(M,i);}
    if(e==='STOCK'){M.total[i]+=q;hooks(M,i);}
    if(e==='PRICE'){M.price[i]=p;hooks(M,i);}
    if(e==='TRADE'){M.avail[i]-=q;hooks(M,i);}}
const stock_event_hook = (a,s,e,p,q) => {
    var i=-1;for(var x=0;x<S.acct.length;x++)if((S.acct[x]===a)&&(S.sym[x]===s)){i=x;break;};
    if((q>0)&&e==='IPO'){if(i<0){i=S.sym.length;S.acct.push(a);S.sym.push(a);S.qty.push(q);}else{S.qty[i]=q;}hooks(S,i)}
    if(e==='TRADE'){if(i<0){i=S.sym.length;S.acct.push(a);S.sym.push(s);S.qty.push(q);}else{S.qty[i]+=q}hooks(S,i)}
    if(e==='STOCK'){if(i<0){i=S.sym.length;S.acct.push(a);S.sym.push(s);S.qty.push(q);}else{S.qty[i]+=q;}hooks(S,i)}}
const div_event_hook = (a,s,e,p,q) => {
    var i=D.acct.findIndex(x=>x===a);
    if(e==='DIV'){if(i<0){i=D.acct.length;D.acct.push(a);D.earnings.push(p);}else{D.earnings[i]=p;}hooks(D,i);}}
const bal_stock_hook = (a,s,q) => {const i=B.acct.findIndex(x=>x===a);var v=0;for(var j=0;j<S.sym.length;j++)if(S.acct[j]===a)v+=S.qty[j]*prx(S.sym[j]);B.eqty[i]=v;hooks(B,i);}
const bal_mkt_hook = (s,p,a,t) => {for(var i=0;i<S.acct.length;i++)if(S.sym[i]===s)bal_stock_hook(S.acct[i],s,NaN);}
const draw_event_hook = (a,s,e,p,q) => {const tr = document.getElementById("events-table").insertRow(1); const td = (i,x) => tr.insertCell(i).innerHTML=x; td(0,a);td(1,e);td(2,s);td(3,p);td(4,q);}
const draw_balances_hook = () => {const t = clear(document.getElementById("balances-table")); for(var i=0;i<B.acct.length;i++){const r=t.insertRow(i+1);r.insertCell(0).innerHTML=B.acct[i];r.insertCell(1).innerHTML=B.cash[i].toString(); r.insertCell(2).innerHTML=B.eqty[i].toString();}}
const draw_earnings_hook = () => {const t=clear(document.getElementById("earnings-table")); for(var i=0;i<D.acct.length;i++){const r=t.insertRow(i+1);r.insertCell(0).innerHTML=D.acct[i]; r.insertCell(1).innerHTML=D.earnings[i].toString();}}
const draw_stocks_hook = () => {const t=clear(document.getElementById("stocks-table")); for(var i=0;i<S.acct.length;i++){const r=t.insertRow(i+1); r.insertCell(0).innerHTML=S.acct[i]; r.insertCell(1).innerHTML=S.sym[i]; r.insertCell(2).innerHTML=S.qty[i].toString();}}
const draw_market_hook =()=>{const t=clear(document.getElementById("market-table")); for(var i=0;i<M.sym.length;i++){const r=t.insertRow(i+1);r.insertCell(0).innerHTML=M.sym[i];r.insertCell(1).innerHTML=M.price[i].toString(); r.insertCell(2).innerHTML=M.avail[i].toString();r.insertCell(3).innerHTML=M.total[i].toString();}}
E.hooks=E.hooks.concat([balance_event_hook,market_event_hook,stock_event_hook,div_event_hook,draw_event_hook])
S.hooks=S.hooks.concat([bal_stock_hook,draw_stocks_hook]);
B.hooks.push(draw_balances_hook);
M.hooks=M.hooks.concat([bal_mkt_hook,draw_market_hook]);
D.hooks.push(draw_earnings_hook);
const test = () => { player("caleb",1000); ipo("B&O",1000,5); trade("B&O","B&O",-5); trade("caleb","B&O",2); train("B&O",100,2); div("B&O",100,0.5); price("B&O",82); stock("B&O","B&O",5); }
