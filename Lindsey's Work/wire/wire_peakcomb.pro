PRO  wire_peakcomb,f1,f2,fc,p=p,out=out

np = 11
f = findgen(np) - 5.

fc = fltarr(np*np+20)
cnt = 0

col = getcolor(/load)

if n_elements(out) eq 0 then begin
 colx = col.green
 print,' %%% Color is green '
endif else begin
 if out ge 1 then begin
 colx = 0L
 print,' %%% Color is black '
 endif else begin
  colx = col.green
  print,' %%% Color is green '
 endelse 
endelse

for i=0,np-1 do begin
for j=0,np-1 do begin

frequency = f1 * f(i) + f2 * f(j)
fc(cnt) = frequency
cnt = cnt + 1

endfor
endfor

fc = fc(0:cnt-1)

w = where(fc gt 0.,c)
fc = fc(w)

if n_elements(p) ge 1 then begin
 for i=0,c-1 do $
  plots,fc(i),!y.crange,col=colx,line=2
endif

end
