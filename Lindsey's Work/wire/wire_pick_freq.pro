PRO wire_pick_freq, f1org, d1org, sm, f1, d1, lim=lim

default9, lim, 1.

f1test = f1org

w = where(f1test gt 1e-6,c)
f1test = f1test(w)
jmp = f1test(1:c-1) - f1test(0:c-2)

mm = median(jmp)
lim = mm * 12.

nj = n_elements(f1org)
jmp2 =  f1org(1:nj-1) - f1org(0:nj-2)

w = where(abs(jmp2) gt lim, c)

f1 = f1org & f1(w) = -100.  
d1 = d1org & d1(w) = -100. 
f2 = f1 & d2 = d1

wg = where(f1 gt 1e-6,cg)
if cg ge 1 then begin
 f1g = f1(wg)
 d1g = d1(wg)

 d3 = d2
 d3(wg) = smooth(d1g,sm,/edge_truncate,/NaN)

 d1 = d3
 f1 = f2
endif else help,wg,lim,mm



END
